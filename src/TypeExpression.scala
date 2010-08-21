package decac;

import scala.collection.mutable.Queue;
import jllvm.LLVMType;
import jllvm.LLVMVoidType;
import jllvm.LLVMStructType;
import jllvm.LLVMFunctionType;

abstract class SigmaType {
  def findUnconstrained(queue: Queue[TauVariable],substitution: TauSubstitution): Unit
  def countUniversals: Int
}

trait VariableContainingType {
  def replace(from: TauVariable,to: TauType): SigmaType
}

class ForallSigma(a: TauVariable,s: SigmaType) extends SigmaType with VariableContainingType {
  val alpha = new SigmaVariable(this)
  val sigma = s match {
    case tvar: TauVariable => throw new Exception("A universal quantifier must have a rho type or another universal quantifier inside it.")
    case beta: VariableContainingType => beta.replace(a,alpha)
  }
  def replace(from: TauVariable,to: TauType): ForallSigma = sigma match {
    case beta: VariableContainingType => new ForallSigma(alpha,beta.replace(from,to))
  }
  def instantiateVariable(from: SigmaVariable,to: TauVariable): SigmaType = sigma match {
    case rho: RhoType => rho.replace(from,to)
    case forall: ForallSigma => forall.instantiateVariable(from,to)
    case tvar: TauVariable => throw new Exception("A universal quantifier must have a rho type inside it.")
  }
  def instantiate: SigmaType = instantiateVariable(alpha,new TauVariable)
  override def findUnconstrained(queue: Queue[TauVariable],substitution: TauSubstitution): Unit = {
    sigma.findUnconstrained(queue,substitution)
    queue.filter(tau => tau != alpha)
  }
  override def countUniversals: Int = 1 + sigma.countUniversals
  def specialize(types: List[RhoType]): RhoType = sigma match {
    case rho: RhoType => {
      if(types.length != 1)
        throw new Exception("Cannot specialize universally-quantified type with specialization parameter list of incorrect length.")
      rho.replace(alpha,types.head)
    }
    case forall: ForallSigma => forall.specialize(types.tail).replace(alpha,types.head)
  }
}

abstract class TauType extends SigmaType {
  def subtypes(tau: TauType,possibly: Boolean): Boolean
  def generateMatch(tau: TauType): Option[TauType]
  def equals(tau: TauType,possibly: Boolean): Boolean = tau == this
  def compile: LLVMType
  override def countUniversals: Int = 0
  def mangle: String
}

abstract class RhoType extends TauType with VariableContainingType {
  protected var definition: Option[TypeDefinition] = None
  override def equals(tau: TauType,possibly: Boolean): Boolean = tau == this
  def replace(from: TauVariable,to: TauType): RhoType
  protected def replaceComponent(t: TauType,from: TauVariable,to: TauType): TauType = t match {
    case tauvar: TauVariable => if(tauvar.equals(from)) to else tauvar
    case rho: RhoType => rho.replace(from,to)
  }
  def define(d: TypeDefinition): Option[TypeDefinition] = {
    definition = Some(d)
    return definition
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
  override def generateMatch(tau: TauType): Option[PrimitiveRho] = {
    if(tau == this)
      Some(this)
    else
      None
  }
  override def replace(from: TauVariable,to: TauType): RhoType = this
  override def findUnconstrained(queue: Queue[TauVariable],substitution: TauSubstitution): Unit = {
  }
  override def mangle: String = definition match {
    case Some(defined) => defined.name
    case None => toString
  }
}

object TopRho extends PrimitiveRho {
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = tau match {
    case range: RhoRange => false
    case tvar: TauVariable => possibly
    case rho: RhoType => false
  }
  override def compile: LLVMType = new LLVMVoidType
  override def mangle: String = "top"
}

object BottomRho extends PrimitiveRho {
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = tau match {
    case range: RhoRange => subtypes(range.lowerBound,possibly)
    case tvar: TauVariable => possibly
    case rho: RhoType => true
  }
  override def compile: LLVMType = new LLVMVoidType
  override def mangle: String = "bottom"
}

class RecordMember(str: String,t: TauType) {
  val name = str
  val tau = t
}

class RecordRho(f: List[RecordMember]) extends RhoType {
  val fields: List[RecordMember] = f.map(field => if(field.tau == null) new RecordMember(field.name,this) else field)
  val length: Int = fields.length
  
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = tau == this || (tau match {
    case rec: RecordRho => (length >= rec.length && fields.zip(rec.fields).forall(pair => (pair._1 == this && pair._2 == rec) || pair._1.tau.equals(pair._2.tau,possibly)))
    case range: RhoRange => subtypes(range.lowerBound,possibly)
    case tvar: TauVariable => possibly
    case _ => false
  })
  
  override def generateMatch(tau: TauType): Option[RecordRho] = tau match {
    case rec: RecordRho => {
      if(rec == this)
        return Some(this)
      else if(length >= rec.length && fields.zip(rec.fields).forall(pair => (pair._1.tau == this && pair._2.tau == rec) || pair._1.tau.equals(pair._2.tau,true)))
        Some(new RecordRho(fields.zip(rec.fields).map(pair => new RecordMember(pair._1.name,if(pair._1 == this && pair._2 == rec) null else pair._1.tau.generateMatch(pair._2.tau) match { case Some(tauMatch) => tauMatch case None => return None }))))
      else
        None
    }
    case range: RhoRange => generateMatch(range.lowerBound)
    case _ => None
  }
  
  override def equals(tau: TauType,possibly: Boolean): Boolean = tau == this || (tau match {
    case rec: RecordRho => (length == rec.length && fields.zip(rec.fields).forall(pair => (pair._1.tau == this && pair._2.tau == rec) || pair._1.tau.equals(pair._2.tau,possibly)))
    case range: RhoRange => possibly && subtypes(range.upperBound,possibly) && range.lowerBound.subtypes(this,possibly)
    case tvar: TauVariable => possibly
    case _ => false
  })
  
  override def replace(from: TauVariable,to: TauType): RhoType = {
    new RecordRho(fields.map(field => new RecordMember(field.name,replaceComponent(field.tau,from,to))))
  }
  
  override def findUnconstrained(queue: Queue[TauVariable],substitution: TauSubstitution): Unit = {
    fields.map(field => field.tau.findUnconstrained(queue,substitution))
  }
  
  override def compile: LLVMType = {
    new LLVMStructType(fields.map(field => field.tau.compile).toArray,true)
  }
  
  override def mangle: String = "pi" + fields.map(field => field.name + ":" + field.tau.mangle).foldRight("")((head: String,tail: String) => "_" + head + tail)
}

class FunctionRho(d: List[TauType],r: TauType) extends RhoType {
  val domain = d.map(domainType => if(domainType == null) this else domainType)
  val range = if(r == null) this else r
  
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = equals(tau,possibly) || (tau match {
    case func: FunctionRho => func.domain.zip(domain).map(pair => (pair._1 == this && pair._2 == tau) || pair._1.subtypes(pair._2,possibly)).foldLeft(true)((x: Boolean,y: Boolean) => x && y) && ((range == this && func.range == func) || range.subtypes(func.range,possibly))
    case range: RhoRange => subtypes(range.lowerBound,possibly)
    case tvar: TauVariable => possibly
    case _ => false
  })
  
  override def generateMatch(tau: TauType): Option[FunctionRho] = tau match {
    case func: FunctionRho => {
      if(func == this)
        Some(this)
      else if(func.domain.zip(domain).map(pair => (pair._1 == this && pair._2 == func) || pair._1.subtypes(pair._2,true)).foldLeft(true)((x: Boolean,y: Boolean) => x && y) && range.subtypes(func.range,true)) {
        val domainMatch = domain.zip(func.domain).map(pair => if(pair._1 == this && pair._2 == func) null else pair._1.generateMatch(pair._2) match { case funcMatch: FunctionRho => funcMatch case _ => return None })
        val rangeMatch = if(range == this && func.range == func) null else range.generateMatch(func.range) match { case funcMatch: FunctionRho => funcMatch case _ => return None }
        return Some(new FunctionRho(domainMatch,rangeMatch))
      }
      else
        return None
    }
    case range: RhoRange => generateMatch(range.lowerBound)
    case _ => None
  }
  
  override def equals(tau: TauType,possibly: Boolean): Boolean = tau == this || (tau match {
    case func: FunctionRho => (func.domain.zip(domain).map(pair => (pair._1 == this && pair._2 == func) || pair._1.equals(pair._2,possibly)).foldLeft(true)((x: Boolean,y: Boolean) => x && y) && func.range.equals(range,possibly))
    case range: RhoRange => possibly && subtypes(range.upperBound,possibly) && range.lowerBound.subtypes(this,possibly)
    case tvar: TauVariable => possibly
    case _ => false
  })
  
  override def replace(from: TauVariable,to: TauType): RhoType = {
    new FunctionRho(domain.map(tau => replaceComponent(tau,from,to)),replaceComponent(range,from,to))
  }
  
  override def findUnconstrained(queue: Queue[TauVariable],substitution: TauSubstitution): Unit = {
    domain.map(tau => tau.findUnconstrained(queue,substitution))
    range.findUnconstrained(queue,substitution)
  }
  
  override def compile: LLVMType = {
    val compiledRange: LLVMType = range.compile
    val compiledDomain: List[LLVMType] = domain.map(tau => tau.compile)
    new LLVMFunctionType(compiledRange,compiledDomain.toArray,false)
  }
  override def mangle: String = "(" + domain.map(tau => tau.mangle).foldRight("")((x: String,y: String) => x + "," + y) + ")_arrow_" + range.mangle
}

class TauVariable extends TauType {
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = {
    equals(tau,possibly)
  }
  override def generateMatch(tau: TauType): Option[TauVariable] = tau match {
    case tvar: TauVariable => Some(this)
    case _ => None
  }
  override def equals(tau: TauType,possibly: Boolean): Boolean = {
    tau == this || possibly
  }
  override def findUnconstrained(queue: Queue[TauVariable],substitution: TauSubstitution): Unit = {
    if(substitution.solve(this) == this)
      queue.enqueue(this)
  }
  override def compile: LLVMType = {
    throw new Exception("Cannot compile a type variable.")
  }
  override def mangle: String = toString
}

class RhoRange(low: Option[RhoType],high: Option[RhoType]) extends TauVariable {
  val lowerBound: RhoType = low match { case None => BottomRho case Some(bound) => bound }
  val upperBound: RhoType = high match { case None => TopRho case Some(bound) => bound }
  
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = upperBound.subtypes(tau,possibly)
}

class SigmaVariable(fa: ForallSigma) extends TauVariable {
  val forallQuantifier = fa
}
