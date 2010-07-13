package decac;

import scala.collection.mutable.Queue;
import jllvm.LLVMType;
import jllvm.LLVMVoidType;
import jllvm.LLVMStructType;
import jllvm.LLVMFunctionType;

abstract class SigmaType {
  def findUnconstrained(queue: Queue[TauVariable],substitution: TauSubstitution): Unit
}

class ForallSigma(a: TauVariable,s: SigmaType) extends SigmaType {
  val alpha = new SigmaVariable(this)
  val sigma = s match {
    case tvar: TauVariable => throw new Exception("A universal quantifier must have a rho type inside it.")
    case rho: RhoType => rho.replace(a,alpha)
    case _ => s
  }
  def instantiateVariable(from: SigmaVariable,to: TauVariable): SigmaType = sigma match {
    case rho: RhoType => rho.replace(from,to)
    case forall: ForallSigma => forall.instantiateVariable(from,to)
    case tvar: TauVariable => throw new Exception("A universal quantifier must have a rho type inside it.")
  }
  def instantiate: SigmaType = instantiateVariable(alpha,new TauVariable)
  def findUnconstrained(queue: Queue[TauVariable],substitution: TauSubstitution): Unit = sigma.findUnconstrained(queue,substitution)
}

abstract class TauType extends SigmaType {
  def subtypes(tau: TauType,possibly: Boolean): Boolean
  def equals(tau: TauType,possibly: Boolean): Boolean = tau == this
  def compile(substitution: TauSubstitution): Option[LLVMType]
  def findUnconstrained(queue: Queue[TauVariable],substitution: TauSubstitution): Unit
}

abstract class RhoType extends TauType {
  override def equals(tau: TauType,possibly: Boolean): Boolean = tau == this
  def replace(from: TauVariable,to: TauType): RhoType
  protected def replaceComponent(t: TauType,from: TauVariable,to: TauType): TauType = t match {
    case tauvar: TauVariable => if(tauvar.equals(from)) to else tauvar
    case rho: RhoType => rho.replace(from,to)
  }
}

case class TypeDefinition(t: RhoType,n: String,context: Module) extends Definition {
  val rho = t
  val name = n
  val scope = { context.declare(this); context }
}

abstract class PrimitiveRho extends RhoType {
  override def equals(tau: TauType,possibly: Boolean): Boolean = tau match {
    case range: RhoRange => possibly && subtypes(range.upperBound,possibly) && range.lowerBound.subtypes(this,possibly)
    case tvar: TauVariable => possibly
    case _ => tau == this
  }
  override def replace(from: TauVariable,to: TauType): RhoType = this
  override def findUnconstrained(queue: Queue[TauVariable],substitution: TauSubstitution): Unit = {
  }
}

object TopRho extends PrimitiveRho {
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = tau match {
    case range: RhoRange => false
    case tvar: TauVariable => possibly
    case rho: RhoType => false
  }
  override def compile(substitution: TauSubstitution): Option[LLVMType] = Some(new LLVMVoidType)
}

object BottomRho extends PrimitiveRho {
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = tau match {
    case range: RhoRange => subtypes(range.lowerBound,possibly)
    case tvar: TauVariable => possibly
    case rho: RhoType => true
  }
  override def compile(substitution: TauSubstitution): Option[LLVMType] = Some(new LLVMVoidType)
}

class RecordMember(str: String,t: TauType) {
  val name = str
  val tau = t
}

class RecordRho(f: List[RecordMember]) extends RhoType {
  val fields: List[RecordMember] = f
  val length: Int = fields.length
  
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = tau match {
    case rec: RecordRho => rec == this || (length >= rec.length && fields.zip(rec.fields).forall(pair => pair._1.tau.subtypes(pair._2.tau,possibly)))
    case range: RhoRange => subtypes(range.lowerBound,possibly)
    case tvar: TauVariable => possibly
    case _ => false
  }
  
  override def equals(tau: TauType,possibly: Boolean): Boolean = tau match {
    case rec: RecordRho => rec == this || (length == rec.length && fields.zip(rec.fields).forall(pair => pair._1.tau.equals(pair._2.tau,possibly)))
    case range: RhoRange => possibly && subtypes(range.upperBound,possibly) && range.lowerBound.subtypes(this,possibly)
    case tvar: TauVariable => possibly
    case _ => false
  }
  
  override def replace(from: TauVariable,to: TauType): RhoType = {
    new RecordRho(fields.map(field => new RecordMember(field.name,replaceComponent(field.tau,from,to))))
  }
  
  override def findUnconstrained(queue: Queue[TauVariable],substitution: TauSubstitution): Unit = {
    fields.map(field => field.tau.findUnconstrained(queue,substitution))
  }
  
  override def compile(substitution: TauSubstitution): Option[LLVMType] = {
    val compiledMembers = fields.map(field => field.tau.compile(substitution) match {
      case Some(t) => t
      case None => throw new Exception("Field type of record rho-type failed to compile.")
    })
    Some(new LLVMStructType(compiledMembers.toArray,true))
  }
}

class FunctionRho(d: List[TauType],r: TauType) extends RhoType {
  val domain = d
  val range = r
  
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = tau match {
    case func: FunctionRho => func == this || (func.domain.zip(domain).map(pair => pair._1.subtypes(pair._2,possibly)).foldLeft(true)((x: Boolean,y: Boolean) => x && y) && range.subtypes(func.range,possibly))
    case range: RhoRange => subtypes(range.lowerBound,possibly)
    case tvar: TauVariable => possibly
    case _ => false
  }
  
  override def equals(tau: TauType,possibly: Boolean): Boolean = tau match {
    case func: FunctionRho => func == this || (func.domain.zip(domain).map(pair => pair._1.subtypes(pair._2,possibly)).foldLeft(true)((x: Boolean,y: Boolean) => x && y) && func.range.equals(range,possibly))
    case range: RhoRange => possibly && subtypes(range.upperBound,possibly) && range.lowerBound.subtypes(this,possibly)
    case tvar: TauVariable => possibly
    case _ => false
  }
  
  override def replace(from: TauVariable,to: TauType): RhoType = {
    new FunctionRho(domain.map(tau => replaceComponent(tau,from,to)),replaceComponent(range,from,to))
  }
  
  override def findUnconstrained(queue: Queue[TauVariable],substitution: TauSubstitution): Unit = {
    domain.map(tau => tau.findUnconstrained(queue,substitution))
    range.findUnconstrained(queue,substitution)
  }
  
  override def compile(substitution: TauSubstitution): Option[LLVMType] = {
    val compiledRange: LLVMType = range.compile(substitution) match {
      case Some(t) => t
      case None => throw new Exception("Range type of function rho-type does not compile successfully.")
    }
    val compiledDomain: List[LLVMType] = domain.map(tau => tau.compile(substitution) match {
      case Some(t) => t
      case None => throw new Exception("Domain type of function rho-type does not compile successfully.")
    })
    Some(new LLVMFunctionType(compiledRange,compiledDomain.toArray,false))  
  }
}

class TauVariable extends TauType {
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = {
    equals(tau,possibly)
  }
  override def equals(tau: TauType,possibly: Boolean): Boolean = {
    tau == this || possibly
  }
  override def findUnconstrained(queue: Queue[TauVariable],substitution: TauSubstitution): Unit = {
    if(substitution.solve(this) == this)
      queue.enqueue(this)
  }
  override def compile(substitution: TauSubstitution): Option[LLVMType] = None
}

class RhoRange(low: Option[RhoType],high: Option[RhoType]) extends TauVariable {
  val lowerBound: RhoType = low match { case None => BottomRho case Some(bound) => bound }
  val upperBound: RhoType = high match { case None => TopRho case Some(bound) => bound }
  
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = upperBound.subtypes(tau,possibly)
}

class SigmaVariable(fa: ForallSigma) extends TauVariable {
  val forallQuantifier = fa
}
