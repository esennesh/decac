package decac;

import jllvm.LLVMType
import jllvm.LLVMVoidType
import jllvm.LLVMFunctionType
import jllvm.LLVMStructType

trait SigmaType {
  def instantiate(args: List[TauType]): GammaType
  def specialize(args: List[GammaType]): SigmaSubstitution
  def body: GammaType
}

abstract class TauType {
  def subtypes(tau: TauType,possibly: Boolean): Boolean
  def matchSupertype(tau: TauType): Option[TauType]
  def equals(tau: TauType,possibly: Boolean): Boolean = tau == this
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
  
  override def specialize(args: List[GammaType]): SigmaSubstitution = {
    if(args.length > 0)
      throw new Exception("Cannot specialize gamma type on non-empty arguments.")
    new SigmaSubstitution
  }
  
  override def body: GammaType = this
}

case class TypeDefinition(t: GammaType,n: String,context: Module) extends Definition {
  val gamma = t
  val name = n
  val scope = { context.declare(this); context }
}

abstract class PrimitiveGamma extends GammaType {
  override def equals(tau: TauType,possibly: Boolean): Boolean = tau match {
    case range: GammaRange => possibly && subtypes(range.upperBound,true) && range.lowerBound.subtypes(this,true)
    case tvar: TauVariable => possibly
    case _ => tau == this
  }
  
  override def matchSupertype(tau: TauType): Option[PrimitiveGamma] = {
    if(tau == this)
      Some(this)
    else
      None
  }
  
  override def mangle: String = definition match {
    case Some(defined) => defined.name
    case None => toString
  }
}

object TopGamma extends PrimitiveGamma {
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = tau match {
    case range: GammaRange => subtypes(range.lowerBound,possibly)
    case tvar: TauVariable => possibly
    case gamma: GammaType => false
  }
  override def compile: LLVMType = new LLVMVoidType
  override def mangle: String = "top"
}

object BottomGamma extends PrimitiveGamma {
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = tau match {
    case range: GammaRange => subtypes(range.lowerBound,possibly)
    case tvar: TauVariable => possibly
    case rho: GammaType => true
  }
  override def compile: LLVMType = new LLVMVoidType
  override def mangle: String = "bottom"
}

abstract class RhoType extends GammaType {
  def replace(from: TauVariable,to: TauType): RhoType
  def map(f: (TauType) => TauType): RhoType
  def filter(p: (TauType) => Boolean): List[TauType]
  def generalize(substitution: TauSubstitution): SigmaType = {
    val tvars = filter(tau => tau.equals(substitution.solve(tau),false))
    if(tvars == Nil)
      this
    else {
      val head = tvars.head match { case tvar: TauVariable => tvar case _ => throw new Exception("Given something other than a tau variable in what should be a list of tau variables.") }
      tvars.tail.foldLeft[BetaType](new BetaRho(this,head))((beta: BetaType,tau: TauType) => tau match {
        case tvar: TauVariable => new BetaBeta(beta,tvar)
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
  
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = tau == this || (tau match {
    case rec: RecordPi => (length >= rec.length && fields.zip(rec.fields).forall(pair => pair._1.tau.equals(pair._2.tau,possibly)))
    case range: GammaRange => subtypes(range.lowerBound,possibly)
    case tvar: TauVariable => possibly
    case TopGamma => true
    case BottomGamma => false
    case _ => false
  })
  
  override def matchSupertype(tau: TauType): Option[RecordPi] = tau match {
    case rec: RecordPi => {
      if(rec == this)
        return Some(this)
      else if(length >= rec.length && fields.zip(rec.fields).forall(pair => pair._1.tau.equals(pair._2.tau,true))) {
        val matchedFields = fields.zip(rec.fields).map(pair => pair._1.tau.matchSupertype(pair._2.tau) match {
          case Some(f) => new RecordMember(pair._1.name,f) 
          case None => throw new Exception("Could not match field of subtype record to field of supertype record.")
        })
        Some(new RecordPi(matchedFields))
      }
      else
        None
    }
    case range: GammaRange => matchSupertype(range.lowerBound)
    case _ => None
  }
  
  override def equals(tau: TauType,possibly: Boolean): Boolean = tau == this || (tau match {
    case rec: RecordPi => (length == rec.length && fields.zip(rec.fields).forall(pair => pair._1.tau.equals(pair._2.tau,possibly)))
    case range: GammaRange => possibly && subtypes(range.upperBound,possibly) && range.lowerBound.subtypes(this,possibly)
    case tvar: TauVariable => possibly
    case _ => false
  })
  
  override def replace(from: TauVariable,to: TauType): RecordPi = {
    map(tau => tau match { case tvar: TauVariable => if(tvar == from) to else tvar case _ => tau })
  }
  
  override def map(f: (TauType) => TauType): RecordPi = {
    new RecordPi(fields.map(field => new RecordMember(field.name,field.tau match { case rho: RhoType => rho.map(f) case _ => f(field.tau) })))
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
  
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = equals(tau,possibly) || (tau match {
    case func: FunctionArrow => func.domain.zip(domain).map(pair => pair._1.subtypes(pair._2,possibly)).foldLeft(true)((x: Boolean,y: Boolean) => x && y) && range.subtypes(func.range,possibly)
    case range: RhoRange => subtypes(range.lowerBound,possibly)
    case tvar: TauVariable => possibly
    case TopGamma => true
    case BottomGamma => false
    case _ => false
  })
  
  override def matchSupertype(tau: TauType): Option[FunctionArrow] = tau match {
    case func: FunctionArrow => {
      if(func == this)
        Some(this)
      else if(func.domain.zip(domain).map(pair => pair._1.subtypes(pair._2,true)).foldLeft(true)((x: Boolean,y: Boolean) => x && y) && range.subtypes(func.range,true)) {
        val domainMatch = domain.zip(func.domain).map(pair => pair._1.matchSupertype(pair._2) match { case Some(d) => d case None => return None })
        val rangeMatch = range.matchSupertype(func.range) match { case Some(r) => r case None => return None }
        return Some(new FunctionArrow(domainMatch,rangeMatch))
      }
      else
        return None
    }
    case range: RhoRange => matchSupertype(range.lowerBound)
    case _ => None
  }
  
  override def equals(tau: TauType,possibly: Boolean): Boolean = tau == this || (tau match {
    case func: FunctionArrow => (func.domain.zip(domain).map(pair => pair._1.equals(pair._2,possibly)).foldLeft(true)((x: Boolean,y: Boolean) => x && y) && func.range.equals(range,possibly))
    case range: RhoRange => possibly && subtypes(range.upperBound,possibly) && range.lowerBound.subtypes(this,possibly)
    case tvar: TauVariable => possibly
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
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = {
    equals(tau,possibly)
  }
  
  override def matchSupertype(tau: TauType): Option[TauVariable] = tau match {
    case tvar: TauVariable => Some(this)
    case _ => None
  }
  
  override def equals(tau: TauType,possibly: Boolean): Boolean = {
    tau == this || possibly
  }
  
  override def mangle: String = toString
}

class GammaRange(low: Option[GammaType],high: Option[GammaType]) extends TauVariable {
  val lowerBound: GammaType = low match { case None => BottomGamma case Some(bound) => bound }
  val upperBound: GammaType = high match { case None => TopGamma case Some(bound) => bound }
  
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = upperBound.subtypes(tau,possibly)
}

abstract class BetaType extends SigmaType {
  val alpha: TauVariable
  
  def replace(from: TauVariable,to: TauType): BetaType
  
  override def instantiate(args: List[TauType]): RhoType
  override def specialize(args: List[GammaType]): SigmaSubstitution
  override def body: RhoType
}

class BetaRho(r: RhoType,tvar: TauVariable) extends BetaType {
  override val alpha: TauVariable = new TauVariable
  val rho: RhoType = r.replace(tvar,alpha)
  
  override def replace(from: TauVariable,to: TauType): BetaType = new BetaRho(rho.replace(from,to),alpha)
  
  override def instantiate(args: List[TauType]): RhoType = {
    if(args.length == 1)
      rho.replace(alpha,args.head)
    else
      throw new Exception("Given wrong number of arguments to specialize rho-containing beta type.")
  }
  
  override def specialize(args: List[GammaType]): SigmaSubstitution = {
    if(args.length == 1) {
      val result = new SigmaSubstitution
      result.substitute(alpha,args.head)
      result
    }
    else
      throw new Exception("Given wrong number of arguments to specialize rho-containing beta type.")
  }
  
  override def body: RhoType = rho
}

class BetaBeta(b: BetaType,tvar: TauVariable) extends BetaType {
  override val alpha: TauVariable = new TauVariable
  val beta: BetaType = b.replace(tvar,alpha)
  
  override def replace(from: TauVariable,to: TauType): BetaType = new BetaBeta(beta.replace(from,to),alpha)
  
  override def instantiate(args: List[TauType]): RhoType = beta.instantiate(args.tail).replace(alpha,args.head)
  
  override def specialize(args: List[GammaType]): SigmaSubstitution = {
    val result = beta.specialize(args.tail)
    result.substitute(alpha,args.head)
    result
  }
  
  override def body: RhoType = beta.body
}
