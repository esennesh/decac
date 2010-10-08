package decac;

import jllvm.LLVMType
import jllvm.LLVMVoidType
import jllvm.LLVMFunctionType
import jllvm.LLVMStructType

trait SigmaType {
  def instantiate(args: List[TauType]): GammaType
  def freshlyInstantiate: GammaType
  def specialize(args: List[GammaType]): BetaSpecialization
  def body: GammaType
}

/* trait ComplexSigma extends SigmaType {
  def replace(from: TauVariable,to: TauType): ComplexSigma = {
    map(tau => tau match { case tvar: TauVariable => if(tvar == from) to else tvar case _ => tau })
  }
  def map(f: (TauType) => TauType): ComplexSigma
  def filter(p: (TauType) => Boolean): List[TauType]
} */

abstract class TauType {
  def subtypes(tau: TauType): Boolean
  def equals(tau: TauType): Boolean = tau == this
  def mangle: String
}

abstract class GammaType extends TauType with SigmaType {
  protected var definition: Option[TypeDefinition] = None
  
  def define(d: TypeDefinition): Option[TypeDefinition] = {
    definition = Some(d)
    return definition
  }
  
  def compile: LLVMType
  
  override def instantiate(args: List[TauType]): GammaType = {
    if(args.length > 0)
      throw new Exception("Cannot specialize gamma type on non-empty arguments.")
    this
  }
  
  override def freshlyInstantiate: GammaType = this
  
  override def specialize(args: List[GammaType]): BetaSpecialization = {
    if(args.length > 0)
      throw new Exception("Cannot specialize gamma type on non-empty arguments.")
    new BetaSpecialization
  }
  
  override def body: GammaType = this
}

case class TypeDefinition(t: GammaType,n: String,context: Module) extends Definition {
  val gamma = t
  val name = n
  val scope = { context.define(this); context }
}

abstract class PrimitiveGamma extends GammaType {
  override def equals(tau: TauType): Boolean = tau match {
    case bvar: BetaVariable => true
    case range: GammaRange => subtypes(range.upperBound) && range.lowerBound.subtypes(this)
    case tvar: TauVariable => false
    case _ => tau == this
  }
  
  override def mangle: String = definition match {
    case Some(defined) => defined.name
    case None => toString
  }
}

object TopGamma extends PrimitiveGamma {
  override def subtypes(tau: TauType): Boolean = tau match {
    case bvar: BetaVariable => true
    case range: GammaRange => subtypes(range.lowerBound)
    case tvar: TauVariable => false
    case gamma: GammaType => false
  }
  override def compile: LLVMType = new LLVMVoidType
  override def mangle: String = "top"
}

object BottomGamma extends PrimitiveGamma {
  override def subtypes(tau: TauType): Boolean = tau match {
    case bvar: BetaVariable => true
    case range: GammaRange => subtypes(range.lowerBound)
    case tvar: TauVariable => false
    case rho: GammaType => true
  }
  override def compile: LLVMType = new LLVMVoidType
  override def mangle: String = "bottom"
}

abstract class RhoType extends GammaType {
  def replace(from: TauVariable,to: TauType): RhoType
  def map(f: (TauType) => TauType): RhoType
  def scopeMap(f: (ScopeType) => ScopeType): RhoType
  def filter(p: (TauType) => Boolean): List[TauType]
  def generalize(substitution: TauSubstitution): SigmaType = {
    val tvars = filter(tau => tau.equals(substitution.solve(tau)))
    if(tvars == Nil)
      this
    else {
      val head = tvars.head match { case tvar: TauVariable => tvar case _ => throw new Exception("Given something other than a tau variable in what should be a list of tau variables.") }
      val betaHead = new BetaRho(this,head)
      substitution.substitute(head,betaHead.alpha)
      tvars.tail.foldLeft[BetaType](betaHead)((beta: BetaType,tau: TauType) => tau match {
        case tvar: TauVariable => {
          val newBeta = new BetaBeta(beta,tvar)
          substitution.substitute(tvar,newBeta.alpha)
          newBeta
        }
        case _ => throw new Exception("Given something other than a tau variable in what should be a list of tau variables.")
      })
    }
  }
}

class RecordMember(str: Option[String],t: TauType) {
  val name = str
  val tau = t
}

class RecordPi(f: List[RecordMember]) extends RhoType {
  val fields: List[RecordMember] = f
  val length: Int = fields.length
  
  override def subtypes(tau: TauType): Boolean = tau == this || (tau match {
    case rec: RecordPi => (length >= rec.length && fields.zip(rec.fields).forall(pair => pair._1.tau.equals(pair._2.tau)))
    case range: GammaRange => subtypes(range.lowerBound)
    case bvar: BetaVariable => true
    case tvar: TauVariable => false
    case TopGamma => true
    case _ => false
  })
  
  override def equals(tau: TauType): Boolean = tau == this || (tau match {
    case rec: RecordPi => (length == rec.length && fields.zip(rec.fields).forall(pair => pair._1.tau.equals(pair._2.tau)))
    case bvar: BetaVariable => true
    case range: GammaRange => subtypes(range.upperBound) && range.lowerBound.subtypes(this)
    case _ => false
  })
  
  override def replace(from: TauVariable,to: TauType): RecordPi = {
    map(tau => tau match { case tvar: TauVariable => if(tvar == from) to else tvar case _ => tau })
  }
  
  override def map(f: (TauType) => TauType): RecordPi = {
    new RecordPi(fields.map(field => new RecordMember(field.name,field.tau match { case rho: RhoType => rho.map(f) case _ => f(field.tau) })))
  }
  
  override def scopeMap(f: (ScopeType) => ScopeType): RecordPi = {
    new RecordPi(fields.map(field => new RecordMember(field.name,field.tau match { case rho: RhoType => rho.scopeMap(f) case _ => field.tau })))
  }
  
  override def filter(p: (TauType) => Boolean): List[TauType] = {
    var result: List[TauType] = Nil
    fields.foreach(field => field.tau match {
      case rho: RhoType => result = result ++ rho.filter(p)
      case _ => if(p(field.tau)) result = field.tau :: result
    })
    result
  }
  
  override def compile: LLVMStructType = {
    val compiledFields = fields.map(field => field.tau match {
      case gammaField: GammaType => gammaField.compile
      case _ => throw new Exception("Cannot compile non-gamma field type " + field.tau.mangle + " of record type " + mangle + ".")
    })
    new LLVMStructType(compiledFields.toArray,true)
  }
  
  override def mangle: String = "pi" + fields.map(field => field.name + ":" + field.tau.mangle).foldRight("")((head: String,tail: String) => "_" + head + tail)
}

class FunctionArrow(d: List[TauType],r: TauType) extends RhoType {
  val domain = d
  val range = r
  
  override def subtypes(tau: TauType): Boolean = equals(tau) || (tau match {
    case func: FunctionArrow => func.domain.zip(domain).map(pair => pair._1.subtypes(pair._2)).foldLeft(true)((x: Boolean,y: Boolean) => x && y) && range.subtypes(func.range)
    case bvar: BetaVariable => true
    case range: RhoRange => subtypes(range.lowerBound)
    case TopGamma => true
    case _ => false
  })
  
  override def equals(tau: TauType): Boolean = tau == this || (tau match {
    case func: FunctionArrow => (func.domain.zip(domain).map(pair => pair._1.equals(pair._2)).foldLeft(true)((x: Boolean,y: Boolean) => x && y) && func.range.equals(range))
    case bvar: BetaVariable => true
    case range: RhoRange => subtypes(range.upperBound) && range.lowerBound.subtypes(this)
    case _ => false
  })
  
  override def replace(from: TauVariable,to: TauType): FunctionArrow = {
    map(tau => tau match { case tvar: TauVariable => if(tvar == from) to else tvar case _ => tau })
  }
  
  override def map(f: (TauType) => TauType): FunctionArrow = {
    val mappedDomain = domain.map(tau => tau match { case rho: RhoType => rho.map(f) case _ => f(tau) })
    val mappedRange = range match { case rho: RhoType => rho.map(f) case _ => f(range) }
    new FunctionArrow(mappedDomain,mappedRange)
  }
  
  override def scopeMap(f: (ScopeType) => ScopeType): FunctionArrow = {
    val mappedDomain = domain.map(tau => tau match { case rho: RhoType => rho.scopeMap(f) case _ => tau })
    val mappedRange = range match { case rho: RhoType => rho.scopeMap(f) case _ => range }
    new FunctionArrow(mappedDomain,mappedRange)
  }
  
  override def filter(p: (TauType) => Boolean): List[TauType] = {
    var result: List[TauType] = Nil
    domain.foreach(tau => tau match {
      case rho: RhoType => result = result ++ rho.filter(p)
      case _ =>
        if(p(tau))
          result = tau :: result
    })
    range match {
      case rho: RhoType => result = result ++ rho.filter(p)
      case _ =>
        if(p(range))
          result = range :: result
    }
    result
  }
  
  override def compile: LLVMFunctionType = {
    val compiledRange: LLVMType = range match {
      case gammaRange: GammaType => gammaRange.compile
      case _ => throw new Exception("Cannot compile non-gamma range type " + range.mangle + " of arrow type " + mangle + ".")
    }
    val compiledDomain: List[LLVMType] = domain.map(tau => tau match {
      case gammaDomain: GammaType => gammaDomain.compile
      case _ => throw new Exception("Cannot compile non-gamma domain type " + tau.mangle + " of arrow type " + mangle + ".")
    })
    new LLVMFunctionType(compiledRange,compiledDomain.toArray,false)
  }
  
  override def mangle: String = "(" + domain.map(tau => tau.mangle).foldRight("")((x: String,y: String) => x + "," + y) + ")_arrow_" + range.mangle
}

class TauVariable extends TauType {
  override def subtypes(tau: TauType): Boolean = {
    equals(tau)
  }
  
  override def equals(tau: TauType): Boolean = {
    tau == this
  }
  
  override def mangle: String = toString
}

class GammaRange(low: Option[GammaType],high: Option[GammaType]) extends TauVariable {
  val lowerBound: GammaType = low match { case None => BottomGamma case Some(bound) => bound }
  val upperBound: GammaType = high match { case None => TopGamma case Some(bound) => bound }
  
  override def subtypes(tau: TauType): Boolean = upperBound.subtypes(tau)
}

abstract class BetaType extends SigmaType {
  val alpha: BetaVariable
  
  def replace(from: TauVariable,to: TauType): BetaType
  def map(f: (TauType) => TauType): BetaType
  def scopeMap(f: (ScopeType) => ScopeType): BetaType
  def filter(p: (TauType) => Boolean): List[TauType]
  
  override def instantiate(args: List[TauType]): RhoType
  override def freshlyInstantiate: RhoType
  override def specialize(args: List[GammaType]): BetaSpecialization
  override def body: RhoType
}

class BetaVariable(b: BetaType) extends TauVariable {
  val beta: BetaType = b
}

class BetaRho(r: RhoType,tvar: TauVariable) extends BetaType {
  override val alpha: BetaVariable = new BetaVariable(this)
  val rho: RhoType = r.replace(tvar,alpha)
  
  override def replace(from: TauVariable,to: TauType): BetaRho = new BetaRho(rho.replace(from,to),alpha)
  override def map(f: (TauType) => TauType): BetaRho = new BetaRho(rho.map(f),alpha)
  override def scopeMap(f: (ScopeType) => ScopeType): BetaRho = new BetaRho(rho.scopeMap(f),alpha)
  override def filter(p: (TauType) => Boolean): List[TauType] = rho.filter(p)
  
  override def instantiate(args: List[TauType]): RhoType = {
    if(args.length == 1)
      rho.replace(alpha,args.head)
    else
      throw new Exception("Given wrong number of arguments to instantiate rho-containing beta type.")
  }
  
  override def freshlyInstantiate: RhoType = rho.replace(alpha,new TauVariable)
  
  override def specialize(args: List[GammaType]): BetaSpecialization = {
    if(args.length == 1) {
      val result = new BetaSpecialization
      result.substitute(alpha,args.head)
      result
    }
    else
      throw new Exception("Given wrong number of arguments to specialize rho-containing beta type.")
  }
  
  override def body: RhoType = rho
}

class BetaBeta(b: BetaType,tvar: TauVariable) extends BetaType {
  override val alpha: BetaVariable = new BetaVariable(this)
  val beta: BetaType = b.replace(tvar,alpha)
  
  override def replace(from: TauVariable,to: TauType): BetaBeta = new BetaBeta(beta.replace(from,to),alpha)
  override def map(f: (TauType) => TauType): BetaBeta = new BetaBeta(beta.map(f),alpha)
  override def scopeMap(f: (ScopeType) => ScopeType): BetaBeta = new BetaBeta(beta.scopeMap(f),alpha)
  override def filter(p: (TauType) => Boolean): List[TauType] = beta.filter(p)
  
  override def instantiate(args: List[TauType]): RhoType = beta.instantiate(args.tail).replace(alpha,args.head)
  
  override def freshlyInstantiate: RhoType = beta.freshlyInstantiate.replace(alpha,new TauVariable)
  
  override def specialize(args: List[GammaType]): BetaSpecialization = {
    val result = beta.specialize(args.tail)
    result.substitute(alpha,args.head)
    result
  }
  
  override def body: RhoType = beta.body
}
