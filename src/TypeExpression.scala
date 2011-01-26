package decac;

import scala.collection.mutable.Set;
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap;
import jllvm._

trait SigmaType {
  def instantiate(args: List[TauType]): GammaType
  def freshlyInstantiate: GammaType
  def specialize(args: List[GammaType]): BetaSpecialization
  def body: GammaType
}

abstract class TauType {
  def toString: String
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
  override def toString: String = definition match {
    case Some(defined) => defined.name
    case None => toString
  }
}

object TopGamma extends PrimitiveGamma {
  override def compile: LLVMType = new LLVMVoidType
  override def toString: String = "top"
}

object BottomGamma extends PrimitiveGamma {
  override def compile: LLVMType = new LLVMVoidType
  override def toString: String = "bottom"
}

object OpaqueGamma extends PrimitiveGamma {
  protected val compiled: LLVMOpaqueType = new LLVMOpaqueType
  override def compile: LLVMType = compiled
  override def toString: String = "opaque"
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
case class FutureRecursion() extends RecursiveVariable

class RecursiveRho(rho: RhoType,alpha: RecursiveVariable) extends RhoType {
  val contents: RhoType = alpha match {
    case UnrecursiveAlpha(alpha) => rho.replace(alpha,this)
    case MuBinding(mu) => rho.map(tau => if(tau == mu) this else tau)
    case FutureRecursion() => rho
  }
  
  def unfold: RhoType = contents.map(tau => tau)
  def substitute(from: TauVariable,to: TauType): RecursiveRho = {
    val unfolded = unfold.replace(from,to)
    unfolded.filter(tau => tau.isInstanceOf[RecursiveRho]).head.asInstanceOf[RecursiveRho]
  }
  
  override def replace(from: TauVariable,to: TauType): RecursiveRho = {
    map(tau => tau match { case tvar: TauVariable => if(tvar == from) to else tvar case _ => tau })
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
  
  override def compile: LLVMType = {
    val opaque = OpaqueGamma
    val bodyType = contents.map(tau => if(tau == this) opaque else tau).compile
    val bodyHandle = new LLVMTypeHandle(bodyType)
    LLVMTypeHandle.refineType(opaque.compile,bodyType)
    bodyHandle.resolve
  }
  
  override def toString: String = {
    val alpha = new TauVariable
    "mu " + alpha.toString + "." + contents.map(tau => if(tau == this) alpha else tau).toString
  }
}

class RecordMember(str: Option[String],t: TauType) {
  val name = str
  val tau = t
}

class RecordProduct(f: List[RecordMember]) extends RhoType {
  val fields: List[RecordMember] = f
  val length: Int = fields.length
  
  override def replace(from: TauVariable,to: TauType): RecordProduct = {
    map(tau => tau match { case tvar: TauVariable => if(tvar == from) to else tvar case _ => tau })
  }
  
  override def map(f: (TauType) => TauType): RecordProduct = {
    new RecordProduct(fields.map(field => new RecordMember(field.name,field.tau match { case mu: RecursiveRho => f(mu) case rho: RhoType => rho.map(f) case _ => f(field.tau) })))
  }
  
  override def scopeMap(f: (ScopeType) => ScopeType): RecordProduct = {
    new RecordProduct(fields.map(field => new RecordMember(field.name,field.tau match { case rho: RhoType => rho.scopeMap(f) case _ => field.tau })))
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
      case _ => throw new Exception("Cannot compile non-gamma field type " + field.tau.toString + " of record type " + toString + ".")
    })
    new LLVMStructType(compiledFields.toArray,true)
  }
  
  override def toString: String = "{" + fields.map(field => field.name + ":" + field.tau.toString).foldRight("")((head: String,tail: String) => "_" + head + tail) + "}"
  
  def ++(rec: RecordProduct): RecordProduct = new RecordProduct(fields ++ rec.fields)
}

object EmptyRecord extends RecordProduct(Nil)

class FunctionArrow(d: List[TauType],r: TauType) extends RhoType {
  val domain = d
  val range = r
  
  override def replace(from: TauVariable,to: TauType): FunctionArrow = {
    map(tau => tau match { case tvar: TauVariable => if(tvar == from) to else tvar case _ => tau })
  }
  
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
      case _ => throw new Exception("Cannot compile non-gamma range type " + range.toString + " of arrow type " + toString + ".")
    }
    val compiledDomain: List[LLVMType] = domain.map(tau => tau match {
      case gammaDomain: GammaType => gammaDomain.compile
      case _ => throw new Exception("Cannot compile non-gamma domain type " + tau.toString + " of arrow type " + toString + ".")
    })
    new LLVMFunctionType(compiledRange,compiledDomain.toArray,false)
  }
  
  override def toString: String = "(" + (domain match { case head :: tail => head.toString + tail.foldLeft("")((x: String,y: TauType) => x + "," + y.toString) case Nil => "" }) + ")->" + range.toString
}

/*case class CaseGuard(instantiation: TauType,general: TauType) {
  def satisfiable: Boolean = TauOrdering.equiv(instantiation,general)
  def specialize(tvar: TypeParameter,tau: TauType): CaseGuard = general match {
    case rho: RhoType => CaseGuard(instantiation,rho.replace(tvar,tau))
    case param: TypeParameter => CaseGuard(instantiation,if(param == tvar) tau else param)
    case _ => this
  }
  override def toString: String = instantiation.toString + "<=" + general.toString
}*/

abstract class DataConstructor
case class NameConstructor(name: String) extends DataConstructor
case class NamelessConstructor(representation: Int) extends DataConstructor
case class NoConstructor() extends DataConstructor

case class GuardedProduct(/*guards: Set[CaseGuard],*/name: DataConstructor,record: RecordProduct) {
  val constructor = name match {
    case NamelessConstructor(repr) => repr
    case NameConstructor(str) => DataConstructors.constructor(Some(str))
    case NoConstructor() => DataConstructors.constructor(None)
  }
  /*assert(guards.forall(g => g.instantiation match { case rho: RhoType => rho.filter(tau => tau.isInstanceOf[TauVariable]).forall(tvar => record.filter(tau => tau == tvar) != Nil) case _ => true }))*/
  
  //def inhabited: Boolean = guards.forall(guard => guard.satisfiable)
  
  def replace(tvar: TypeParameter,tau: TauType): GuardedProduct = {
    //val replacedGuards = guards.map(guard => guard.specialize(tvar,tau))
    val replacedProduct = record.replace(tvar,tau).asInstanceOf[RecordProduct]
    GuardedProduct(/*replacedGuards,*/name,replacedProduct)
  }
  
  def map(f: (TauType) => TauType): GuardedProduct = {
    /* val newGuards = guards.map(guard => CaseGuard(guard.instantiation,guard.general match { case mu: RecursiveRho => f(mu) case rho: RhoType => rho.map(f) case _ => f(guard.general) })) */
    val newRecord = record.map(f)
    GuardedProduct(/*newGuards,*/name,newRecord)
  }
  override def toString: String = {
    /*val guardStrings = guards.map(guard => guard.toString)
    val lets = guardStrings.toList match {
      case first :: rest => first + rest.foldLeft("")((x: String,y: String) => x + "," + y)
      case Nil => ""
    }*/
    //val contents = "[" + lets + "]" + record.toString
    name match {
      case NameConstructor(str) => str + "(" + /*contents*/record.toString + ")"
      case _ => /*contents*/record.toString
    }
  }
}

class SumType(addends: List[GuardedProduct]) extends RhoType {
  val sumCases = addends

  /*if(addends.map(gp => !gp.inhabited).foldLeft(false)((x: Boolean,y: Boolean) => x && y))
    throw new Exception("GADT " + toString + " has no inhabitant values.")*/
    
  override def replace(from: TauVariable,to: TauType): SumType = {
    map(tau => tau match { case tvar: TauVariable => if(tvar == from) to else tvar case _ => tau })
  }
    
  override def map(f: (TauType) => TauType): SumType = new SumType(sumCases.map(sumCase => sumCase.map(f)))
  override def scopeMap(f: (ScopeType) => ScopeType): SumType = {
    val scopedCases = sumCases.map(c => GuardedProduct(/*c.guards.map(guard => CaseGuard(guard.instantiation,guard.general match { case rho: RhoType => rho.scopeMap(f) case _ => guard.general })),*/c.name,c.record.scopeMap(f)))
    new SumType(scopedCases)
  }
  override def filter(p: (TauType) => Boolean): List[TauType] = {
    var result: List[TauType] = Nil
    sumCases.foreach(c => result = result ++ c.record.filter(p))
    /*sumCases.foreach(c => c.guards.foreach(guard => if(p(guard.instantiation)) result = result ++ (guard.instantiation :: Nil)))*/
    result
  }
  override def toString: String = {
    val sum = sumCases match {
      case head :: tail => head.toString + tail.map(x => x.toString).foldLeft("")((x: String,y: String) => x + " + " + y)
      case Nil => ""
    }
    "<" + sum + ">"
  }
  
  //Holy crap these functions are inefficient and ugly.  I *need* to rewrite these.
  protected def equalFields(recs: List[RecordProduct]): List[Option[RecordMember]] = recs match {
    case last :: Nil => last.fields.map(field => Some(field))
    case first :: rest => equalFields(rest).zip(first.fields).map(pair => pair._1 match { case Some(mem) => if(TauOrdering.equiv(mem.tau,pair._2.tau)) Some(mem) else None case None => None })
    case Nil => Nil
  }
  
  protected def firstEqualFields(fields: List[Option[RecordMember]]): List[RecordMember] = fields match {
    case first :: rest => first match {
      case Some(field) => field :: firstEqualFields(rest)
      case None => Nil
    }
    case Nil => Nil
  }
    
  def minimalRecord: RecordProduct = {
    val fields = firstEqualFields(equalFields(sumCases.map(gp => gp.record)))
    new RecordProduct(fields)
  }
  
  def enumeration: Boolean = sumCases.forall(gp => gp.record.length == 0)
  
  override def compile: LLVMType = {
    val largestConstructor = sumCases.sortWith((x: GuardedProduct,y: GuardedProduct) => x.constructor > y.constructor).head.constructor
    val representationSize = math.floor(math.log(largestConstructor) / math.log(2)).toInt + 1
    assert(representationSize > 0)
    if(enumeration) {
      new LLVMIntegerType(representationSize)
    }
    else {
      val minrec = minimalRecord
      val fields = new LLVMIntegerType(representationSize) :: new LLVMPointerType(minrec.compile,0) :: Nil
      new LLVMStructType(fields.toArray,true)
    }
  }
}

class OpenSum(base: GuardedProduct) extends SumType(base :: Nil) {
  protected var openCases = base :: Nil
  override val sumCases = openCases
  val recursiveThis = new RecursiveRho(this,FutureRecursion())
  
  override def replace(from: TauVariable,to: TauType): OpenSum = {
    map(tau => tau match { case tvar: TauVariable => if(tvar == from) to else tvar case _ => tau })
  }
  
  override def map(f: (TauType) => TauType): OpenSum = {
    val newCases = sumCases.map(sumCase => sumCase.map(f))
    val result = new OpenSum(newCases.last)
    result.openCases = newCases.map(gp => gp.map(tau => if(tau == recursiveThis) result.recursiveThis else tau))
    result
  }
  
  def expand(addend: GuardedProduct,selfReference: TauVariable,addBase: Boolean): GuardedProduct = {
    val record = if(addBase) minimalRecord ++ addend.record.replace(selfReference,recursiveThis) else addend.record.replace(selfReference,recursiveThis)
    val name = addend.name match {
      case NoConstructor() => NamelessConstructor(addend.constructor)
      case _ => addend.name
    }
    val newCase = new GuardedProduct(name,record)
    openCases = newCase :: openCases
    newCase
  }
}

object BooleanGamma extends SumType(GuardedProduct(/*Set.empty[CaseGuard],*/NameConstructor("false"),EmptyRecord) :: GuardedProduct(/*Set.empty[CaseGuard],*/NameConstructor("true"),EmptyRecord) :: Nil)

object DataConstructors {
  var next = 0
  val constructors = new HashMap[String,Int]
  
  protected def freshConstructor(name: Option[String]): Int = {
    val result = next
    next = next + 1
    name match {
      case Some(str) => constructors.put(str,result)
      case None => {}
    }
    result
  }
  
  def constructor(name: Option[String]): Int = name match {
    case Some(str) => constructors.get(str) match {
      case Some(result) => result
      case None => freshConstructor(name)
    }
    case None => freshConstructor(None)
  }
}

class TauVariable extends TauType { 
  override def toString: String = toString
  
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

class TypeParameter extends TauVariable

class GammaRange(l: GammaType,h: GammaType) extends TauVariable {
  val lowerBound: GammaType = l
  val upperBound: GammaType = h
 
  override def toString: String = super.toString + "(" + lowerBound.toString + "," + upperBound.toString + ")"
  
  override def refine(low: Option[GammaType],high: Option[GammaType]): GammaRange = {
    val lower = low match {
      case Some(gamma) => {
        if(!TauOrdering.lteq(lowerBound,gamma))
          throw new Exception(lowerBound.toString + " </: " + gamma.toString)
        gamma
      }
      case None => lowerBound
    }
    val upper = high match {
      case Some(gamma) => {
        if(!TauOrdering.lteq(gamma,upperBound))
          throw new Exception(gamma.toString + " </: " + upperBound.toString)
        gamma
      }
      case None => upperBound
    }
    if(!TauOrdering.lteq(lower,upper))
      throw new Exception(lower.toString + " </: " + upper.toString)
    new GammaRange(lower,upper)
  }
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

class BetaVariable(b: BetaType) extends TypeParameter {
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
