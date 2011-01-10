package decac;

import jllvm.LLVMType
import jllvm.LLVMVoidType
import jllvm.LLVMFunctionType
import jllvm.LLVMStructType
import jllvm.LLVMTypeHandle
import jllvm.LLVMOpaqueType

trait SigmaType {
  def instantiate(args: List[TauType]): GammaType
  def freshlyInstantiate: GammaType
  def specialize(args: List[GammaType]): BetaSpecialization
  def body: GammaType
}

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

class OpaqueGamma extends PrimitiveGamma {
  protected val compiled: LLVMOpaqueType = new LLVMOpaqueType
  override def subtypes(tau: TauType): Boolean = tau match {
    case bvar: BetaVariable => true
    case range: GammaRange => subtypes(range.lowerBound)
    case _ => false
  }
  override def compile: LLVMType = compiled
  override def mangle: String = "opaque"
}

abstract class RhoType extends GammaType {
  def replace(from: TauVariable,to: TauType): RhoType = {
    map(tau => tau match { case tvar: TauVariable => if(tvar == from) to else tvar case _ => tau })
  }
  def map(f: (TauType) => TauType): RhoType
  def scopeMap(f: (ScopeType) => ScopeType): RhoType
  def filter(p: (TauType) => Boolean): List[TauType]
  def generalize(substitution: TauSubstitution): SigmaType = {
    val tvars = filter(tau => tau.isInstanceOf[TauVariable] && tau.equals(substitution.solve(tau)))
    if(tvars == Nil)
      substitution.solve(this).asInstanceOf[RhoType]
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

abstract class RecursiveVariable
case class UnrecursiveAlpha(alpha: TauVariable) extends RecursiveVariable
case class MuBinding(mu: RecursiveRho) extends RecursiveVariable

class RecursiveRho(rho: RhoType,alpha: RecursiveVariable) extends RhoType {
  val contents: RhoType = alpha match {
    case UnrecursiveAlpha(alpha) => rho.replace(alpha,this)
    case MuBinding(mu) => rho.map(tau => if(tau == mu) this else tau)
  }
  
  override def map(f: (TauType) => TauType): RecursiveRho = {
    new RecursiveRho(contents.map(tau => if(tau == this) tau else f(tau)),MuBinding(this))
  }
  
  override def scopeMap(f: (ScopeType) => ScopeType): RecursiveRho = {
    new RecursiveRho(contents.scopeMap(f),MuBinding(this))
  }
  
  override def filter(f: (TauType) => Boolean): List[TauType] = {
    contents.filter(tau => if(tau == this) false else f(tau))
  }
  
  override def subtypes(tau: TauType): Boolean = tau match {
    case mu: RecursiveRho => {
      val alpha = new TauVariable
      val unrecursiveBody = contents.map(tau => if(tau == this) alpha else tau)
      val rho = mu.map(tau => if(tau == mu) alpha else tau)
      unrecursiveBody.subtypes(rho)
    }
    case range: GammaRange => subtypes(range.lowerBound)
    case bvar: BetaVariable => true
    case tvar: TauVariable => false
    case TopGamma => true
    case _ => false
  }
  
  override def equals(tau: TauType): Boolean = tau == this || (tau match {
    case mu: RecursiveRho => {
      val alpha = new TauVariable
      val unrecursiveBody = contents.map(tau => if(tau == this) alpha else tau)
      val rho = mu.map(tau => if(tau == mu) alpha else tau)
      unrecursiveBody.equals(rho)
    }
    case bvar: BetaVariable => true
    case range: GammaRange => subtypes(range.upperBound) && range.lowerBound.subtypes(this)
    case _ => false
  })
  
  override def compile: LLVMType = {
    val opaque = new OpaqueGamma
    val bodyType = contents.map(tau => if(tau == this) opaque else tau).compile
    val bodyHandle = new LLVMTypeHandle(bodyType)
    LLVMTypeHandle.refineType(opaque.compile,bodyType)
    bodyHandle.resolve
  }
  
  override def mangle: String = {
    val alpha = new TauVariable
    "mu " + alpha.mangle + "." + contents.map(tau => if(tau == this) alpha else tau).mangle
  }
}

class RecordMember(str: Option[String],t: TauType) {
  val name = str
  val tau = t
}

class RecordPi(f: List[RecordMember]) extends RhoType {
  val fields: List[RecordMember] = f
  val length: Int = fields.length
  
  override def subtypes(tau: TauType): Boolean = tau match {
    case rec: RecordPi => (length >= rec.length && fields.zip(rec.fields).forall(pair => pair._1.tau.equals(pair._2.tau)))
    case range: GammaRange => subtypes(range.lowerBound)
    case bvar: BetaVariable => true
    case tvar: TauVariable => false
    case TopGamma => true
    case _ => false
  }
  
  override def equals(tau: TauType): Boolean = tau == this || (tau match {
    case rec: RecordPi => (length == rec.length && fields.zip(rec.fields).forall(pair => pair._1.tau.equals(pair._2.tau)))
    case bvar: BetaVariable => true
    case range: GammaRange => subtypes(range.upperBound) && range.lowerBound.subtypes(this)
    case _ => false
  })
  
  override def map(f: (TauType) => TauType): RecordPi = {
    new RecordPi(fields.map(field => new RecordMember(field.name,field.tau match { case mu: RecursiveRho => f(mu) case rho: RhoType => rho.map(f) case _ => f(field.tau) })))
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
    new LLVMStructType((SigmaLattice.find(this).represent :: compiledFields).toArray,true)
  }
  
  override def mangle: String = "pi" + fields.map(field => field.name + ":" + field.tau.mangle).foldRight("")((head: String,tail: String) => "_" + head + tail)
}

class FunctionArrow(d: List[TauType],r: TauType) extends RhoType {
  val domain = d
  val range = r
  
  override def subtypes(tau: TauType): Boolean = tau match {
    case func: FunctionArrow => func.domain.zip(domain).map(pair => pair._1.subtypes(pair._2)).foldLeft(true)((x: Boolean,y: Boolean) => x && y) && range.subtypes(func.range)
    case bvar: BetaVariable => true
    case range: GammaRange => subtypes(range.lowerBound)
    case TopGamma => true
    case _ => false
  }
  
  override def equals(tau: TauType): Boolean = tau == this || (tau match {
    case func: FunctionArrow => (func.domain.zip(domain).map(pair => pair._1.equals(pair._2)).foldLeft(true)((x: Boolean,y: Boolean) => x && y) && func.range.equals(range))
    case bvar: BetaVariable => true
    case range: GammaRange => subtypes(range.upperBound) && range.lowerBound.subtypes(this)
    case _ => false
  })
  
  override def map(f: (TauType) => TauType): FunctionArrow = {
    val mappedDomain = domain.map(tau => tau match { case mu: RecursiveRho => f(mu) case rho: RhoType => rho.map(f) case _ => f(tau) })
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
  
  override def mangle: String = "(" + (domain match { case head :: tail => head.mangle + tail.map(tau => tau.mangle).foldLeft("")((x: String,y: String) => x + "," + y) case Nil => "" }) + ")->" + range.mangle
}

class TauVariable extends TauType {
  override def subtypes(tau: TauType): Boolean = {
    equals(tau)
  }
  
  override def equals(tau: TauType): Boolean = tau == this
  
  override def mangle: String = toString
  
  def refine(low: Option[GammaType],high: Option[GammaType]): GammaRange = {
    val lower = low match {
      case Some(gamma) => gamma
      case None => BottomGamma
    }
    val upper = high match {
      case Some(gamma) => gamma
      case None => TopGamma
    }
    new GammaRange(lower,upper)
  }
}

class GammaRange(l: GammaType,h: GammaType) extends TauVariable {
  val lowerBound: GammaType = l
  val upperBound: GammaType = h
  
  override def subtypes(tau: TauType): Boolean = upperBound.subtypes(tau)
  override def mangle: String = super.mangle + "(" + lowerBound.mangle + "," + upperBound.mangle + ")"
  
  override def refine(low: Option[GammaType],high: Option[GammaType]): GammaRange = {
    val lower = low match {
      case Some(gamma) => {
        if(!lowerBound.subtypes(gamma) && !lowerBound.equals(gamma))
          throw new Exception(lowerBound.mangle + " </: " + gamma.mangle)
        gamma
      }
      case None => lowerBound
    }
    val upper = high match {
      case Some(gamma) => {
        if(!gamma.subtypes(upperBound) && !gamma.equals(upperBound))
          throw new Exception(gamma.mangle + " </: " + upperBound.mangle)
        gamma
      }
      case None => upperBound
    }
    if(!lower.subtypes(upper) && !lower.equals(upper))
      throw new Exception(lower.mangle + " </: " + upper.mangle)
    new GammaRange(lower,upper)
  }
  
  def getUpperBound: GammaType = if(upperBound == TopGamma) lowerBound else upperBound
  def getLowerBound: GammaType = if(lowerBound == BottomGamma) lowerBound else upperBound
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
