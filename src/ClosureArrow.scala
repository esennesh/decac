package decac;

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import jllvm.LLVMStructType

object ClosureEnvironments {
  protected val environments = new HashMap[FunctionArrow,OpenSum]()
  
  def represent(arrow: FunctionArrow): SumType = {
    val plain = new SumType(TaggedProduct(DataConstructor(None,None),new RecordProduct(new RecordMember(None,arrow) :: Nil)) :: Nil)
    val env = environment(arrow)
    val varyingSignature = TaggedProduct(DataConstructor(None,None),new RecordProduct(new RecordMember(None,new FunctionArrow(env :: arrow.domain,arrow.range)) :: new RecordMember(None,env) :: Nil))
    new SumType(TaggedProduct(DataConstructor(None,None),new RecordProduct(new RecordMember(None,plain) :: Nil)) :: varyingSignature :: Nil)
  }
  
  protected def environment(arrow: FunctionArrow): OpenSum = {
    assert(arrow.filter(tau => tau.isInstanceOf[BetaVariable]) == arrow.filter(tau => tau.isInstanceOf[TauVariable]))
    environments.get(arrow) match {
      case Some(env) => env
      case None => {
        val result = new OpenSum(None,None)
        environments.put(arrow,result)
        result
      }
    }
  }
  
  def closeOver(arrow: FunctionArrow,env: RecordProduct): OpenSum = {
    //Assert that a closure environment cannot hide a type variable.
    assert(env.filter(tau => tau.isInstanceOf[TauVariable]).forall(tvar => arrow.filter(tau => tau == tvar) != Nil))
    val envir = environment(arrow)
    envir.expand(TaggedProduct(DataConstructor(None,None),env),None,false)
    envir
  }
}

class ClosureArrow(d: List[TauType],r: TauType,rep: Option[SumType]) extends RhoType with ArrowType {
  override val domain = d
  override val range = r
  
  val representation: Option[SumType] = rep match {
    case Some(repr) => Some(repr)
    case None => {
      if(filter(tau => tau.isInstanceOf[BetaVariable]) == filter(tau => tau.isInstanceOf[TauVariable]))
        Some(ClosureEnvironments.represent(new FunctionArrow(d,r)))
      else
        None
    }
  }
  override val signature = new FunctionArrow(domain,range)
  
  override def tagged: Boolean = false
  
  override def map(f: (TauType) => TauType): ClosureArrow = {
    val mappedDomain = domain.map(tau => tau match { case mu: RecursiveMu => f(mu) case rho: RhoType => rho.map(f) case _ => f(tau) })
    val mappedRange = range match { case rho: RhoType => rho.map(f) case _ => f(range) }
    val mappedRepresentation = representation match {
      case Some(sum) => Some(sum.map(f))
      case None => None
    }
    val result = new ClosureArrow(mappedDomain,mappedRange,mappedRepresentation)
    result.definition = definition
    result
  }
  
  override def contents: List[TauType] = {
    var result: List[TauType] = Nil
    domain.foreach(tau => tau match {
      case rho: RhoType => result = result ++ rho.contents
      case _ => result = tau :: result
    })
    range match {
      case rho: RhoType => result = result ++ rho.contents
      case _ => result = range :: result
    }
    result
  }
  
  def compile: LLVMStructType = representation match {
    case Some(repr) => repr.compile.asInstanceOf[LLVMStructType]
    case None => throw new Exception("Cannot compile a closure type that isn't a specialization of a principal type due to not knowing the representation for such types.")
  }
  
  override def mangle: String = "(" + (domain match { case head :: tail => head.toString + tail.foldLeft("")((x: String,y: TauType) => x + "," + y.toString) case Nil => "" }) + ")->" + range.toString
}
