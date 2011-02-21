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
  def tagged: Boolean
}

abstract class GammaType extends TauType with SigmaType {
  protected var definition: Option[TypeDefinition] = None
  
  def define(d: TypeDefinition): Option[TypeDefinition] = definition match {
    case Some(defined) => Some(defined)
    case None => {
      definition = Some(d)
      return definition
    }
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
  
  override def toString: String = definition match {
    case Some(defined) => defined.name
    case None => mangle
  }
  
  def mangle: String
}

class TypeDefinition(t: SigmaType,n: String,context: Module) extends Definition {
  val sigma: SigmaType = t
  val name = n
  val scope = { context.define(this); context }
  protected val specializations = new HashMap[List[GammaType],GammaType]()
  
  def instantiate(params: List[TauType]): GammaType = {
    val result = sigma.instantiate(params)
    if(params.forall(param => param match { case tvar: TauVariable => false case rho: RhoType => rho.filter(tau => !tau.isInstanceOf[GammaType]) != Nil case _ => true }))
      specializations.put(params.map(param => param.asInstanceOf[GammaType]),result)
    result
  }
  
  def getSpecializations: Iterable[GammaType] = specializations.values
}

abstract class PrimitiveGamma extends GammaType {
  override def toString: String = definition match {
    case Some(defined) => defined.name
    case None => toString
  }
  
  override def tagged: Boolean = false
}

object TopGamma extends PrimitiveGamma {
  override def compile: LLVMType = new LLVMVoidType
  override def mangle: String = "top"
}

object BottomGamma extends PrimitiveGamma {
  override def compile: LLVMType = new LLVMVoidType
  override def mangle: String = "bottom"
}

object OpaqueGamma extends PrimitiveGamma {
  protected val compiled: LLVMOpaqueType = new LLVMOpaqueType
  override def compile: LLVMType = compiled
  override def mangle: String = "opaque"
}

abstract class RhoType extends GammaType {
  def replace(from: TauType,to: TauType): RhoType = {
    map(tau => if(tau == from) to else tau)
  }
  def map(f: (TauType) => TauType): RhoType
  def scopeMap(f: (ScopeType) => ScopeType): RhoType = {
    val originals = filter(tau => tau.isInstanceOf[PointerType])
    val newrefs = originals.map(tau => {
      val ref = tau.asInstanceOf[ScopedPointer]
      new ScopedPointer(ref.target,f(ref.scope))
    })
    originals.zip(newrefs).foldLeft(this)((rho: RhoType,pair: Tuple2[TauType,ScopedPointer]) => rho.replace(pair._1,pair._2))
  }
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
case class MuBinding(mu: RecursiveMu) extends RecursiveVariable
case class FutureRecursion() extends RecursiveVariable

class RecursiveMu(rho: RhoType,alpha: RecursiveVariable) extends RhoType {
  val contents: RhoType = alpha match {
    case UnrecursiveAlpha(alpha) => rho.replace(alpha,this)
    case MuBinding(mu) => rho.map(tau => if(tau == mu) this else tau)
    case FutureRecursion() => rho
  }
  
  override def tagged: Boolean = contents.tagged
  
  def unfold: RhoType = contents.map(tau => tau)
  def derecurse: Tuple2[TauVariable,RhoType] = {
    val alpha = new TauVariable
    (alpha,contents.map(tau => if(tau == this) alpha else tau))
  }
  def substitute(from: TauVariable,to: TauType): RecursiveMu = {
    val unfolded = unfold.replace(from,to)
    unfolded.filter(tau => tau.isInstanceOf[RecursiveMu]).head.asInstanceOf[RecursiveMu]
  }
  
  override def map(f: (TauType) => TauType): RecursiveMu = {
    val result = new RecursiveMu(contents.map(tau => if(tau == this) tau else f(tau)),MuBinding(this))
    result.definition = definition
    result
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
  
  override def mangle: String = {
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
  
  override def tagged: Boolean = false
  
  override def map(f: (TauType) => TauType): RecordProduct = {
    val result = new RecordProduct(fields.map(field => new RecordMember(field.name,field.tau match { case mu: RecursiveMu => f(mu) case rho: RhoType => rho.map(f) case _ => f(field.tau) })))
    result.definition = definition
    result
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
  
  override def mangle: String = "{" + fields.map(field => field.name + ":" + field.tau.toString).foldRight("")((head: String,tail: String) => "_" + head + tail) + "}"
  
  def ++(rec: RecordProduct): RecordProduct = new RecordProduct(fields ++ rec.fields)
  
  def sizeOf: Int = {
    val target = new LLVMTargetData("i686-pc-linux-gnu")
    target.storeSizeOfType(compile).toInt
  }
}

object EmptyRecord extends RecordProduct(Nil)

trait ArrowType {
  val domain: List[TauType]
  val range: TauType
}

class FunctionArrow(d: List[TauType],r: TauType) extends RhoType with ArrowType {
  override val domain = d
  override val range = r
  
  override def tagged: Boolean = false
  
  override def map(f: (TauType) => TauType): FunctionArrow = {
    val mappedDomain = domain.map(tau => tau match { case mu: RecursiveMu => f(mu) case rho: RhoType => rho.map(f) case _ => f(tau) })
    val mappedRange = range match { case rho: RhoType => rho.map(f) case _ => f(range) }
    val result = new FunctionArrow(mappedDomain,mappedRange)
    result.definition = definition
    result
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
  
  override def mangle: String = "((" + (domain match { case head :: tail => head.toString + tail.foldLeft("")((x: String,y: TauType) => x + "," + y.toString) case Nil => "" }) + ")->" + range.toString + ")*"
}

case class DataConstructor(name: Option[String],representation: Option[Int])

case class TaggedProduct(name: DataConstructor,record: RecordProduct) {
  val constructor = name.representation match {
    case Some(i) => i
    case None => CaseTagger.constructor(name.name)
  }
  
  def replace(tvar: TauVariable,tau: TauType): TaggedProduct = {
    val replacedProduct = record.replace(tvar,tau).asInstanceOf[RecordProduct]
    TaggedProduct(DataConstructor(name.name,Some(constructor)),replacedProduct)
  }
  
  def map(f: (TauType) => TauType): TaggedProduct = {
    val newRecord = record.map(f)
    TaggedProduct(DataConstructor(name.name,Some(constructor)),newRecord)
  }
  
  override def toString: String = {
    name.name match {
      case Some(str) => str + record.toString
      case None => record.toString
    }
  }
}

class SumType(addends: List[TaggedProduct]) extends RhoType {
  val sumCases: List[TaggedProduct] = addends

  override def tagged: Boolean = true

  override def map(f: (TauType) => TauType): SumType = {
    val result = new SumType(sumCases.map(sumCase => sumCase.map(f)))
    result.definition = definition
    result
  }
  override def filter(p: (TauType) => Boolean): List[TauType] = {
    var result: List[TauType] = Nil
    for(sumCase <- sumCases) {
      result = result ++ sumCase.record.filter(p)
      if(p(sumCase.record))
        result = sumCase.record :: result
    }
    result
  }
  override def mangle: String = {
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
  
  def tagRepresentation: LLVMIntegerType = {
    val largestConstructor = sumCases.sortWith((x: TaggedProduct,y: TaggedProduct) => x.constructor > y.constructor).head.constructor
    val representationSize = math.floor(math.log(largestConstructor) / math.log(2)).toInt + 1
    assert(representationSize > 0)
    new LLVMIntegerType(representationSize)
  }
  
  override def compile: LLVMType = {
    if(enumeration)
      tagRepresentation
    else {
      val maxRecord = sumCases.map(sumCase => sumCase.record).sortWith((x,y) => x.sizeOf >= y.sizeOf).head
      val fields = tagRepresentation :: maxRecord.compile :: Nil
      new LLVMStructType(fields.toArray,true)
    }
  }
}

class OpenSum(base: Option[TaggedProduct],selfReference: Option[TauVariable]) extends SumType(Nil) {
  val recursiveThis = new RecursiveMu(this,FutureRecursion())
  protected var openCases = (selfReference,base) match {
    case (Some(tvar),Some(b)) => b.replace(tvar,this) :: Nil
    case (None,Some(b)) => b :: Nil
    case _ => Nil
  }
  override val sumCases = openCases
  
  override def map(f: (TauType) => TauType): OpenSum = {
    val newCases = sumCases.map(sumCase => sumCase.map(f))
    val result = new OpenSum(Some(newCases.last),None)
    result.openCases = newCases.map(gp => gp.map(tau => if(tau == recursiveThis) result.recursiveThis else tau))
    result.definition = definition
    result
  }
  
  def expand(addend: TaggedProduct,selfReference: Option[TauVariable],addBase: Boolean): TaggedProduct = {
    val recursivized = selfReference match {
      case Some(mu) => addend.record.replace(mu,recursiveThis).asInstanceOf[RecordProduct]
      case None => addend.record
    }
    val record = if(addBase) minimalRecord ++ recursivized else recursivized
    val newCase = new TaggedProduct(addend.name,record)
    openCases = newCase :: openCases
    newCase
  }
  
  override def compile: LLVMType = {
    if(filter(tau => tau == recursiveThis) != Nil)
      recursiveThis.compile
    else
      super.compile
  }
}

object CaseTagger {
  protected var next = 0
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
  override def tagged: Boolean = false
  
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
  
  override def tagged: Boolean = if(lowerBound != BottomGamma) lowerBound.tagged else upperBound.tagged
 
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

object BuiltInSums {
  val BooleanGamma = {
    val cases = TaggedProduct(DataConstructor(Some("false"),None),EmptyRecord) :: TaggedProduct(DataConstructor(Some("true"),None),EmptyRecord) :: Nil
    val result = new SumType(cases)
    result.define(new TypeDefinition(result,"boolean",GlobalScope))
    result
  }
  val option: SigmaType = (new SumType(TaggedProduct(DataConstructor(Some("Some"),None),new RecordProduct(new RecordMember(None,new TauVariable) :: Nil)) :: TaggedProduct(DataConstructor(Some("None"),None),EmptyRecord) :: Nil)).generalize(new TauSubstitution)
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
