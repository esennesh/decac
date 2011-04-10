package decac

import scala.collection.mutable._
import jllvm._

class SkolemConstant {
  protected val cases: Set[TauType] = new HashSet[TauType]()
  
  def <(s: SkolemConstant): Boolean = cases.forall(c => s.cases.contains(c))
  def map(f: (TauType) => TauType): SkolemConstant = {
    val result = new SkolemConstant
    cases.foreach(c => result.skolemize(c))
    result
  }
  
  def contents: List[TauType] = {
    var result: List[TauType] = Nil
    for(c <- cases.toList)
      c match {
        case rho: RhoType => result = result ++ List(rho) ++ rho.contents
        case _ => result = c :: result
      }
    result
  }
  
  def compile: LLVMType = {
    val max = cases.toList.sortWith((x,y) => (x,y) match {
      case (x: GammaType,y: GammaType) => x.sizeOf >= y.sizeOf
      case (x: GammaType,_) => true
      case _ => false
    }).head
    max.asInstanceOf[GammaType].compile
  }
  
  def skolemize(tau: TauType): Unit = {
    assert(tau match {
      case rho: RhoType => rho.filter(tau => tau.isInstanceOf[TauVariable]) == rho.filter(tau => tau.isInstanceOf[BetaVariable])
      case tvar: TauVariable => tau.isInstanceOf[BetaVariable]
      case _ => true
    })
    for(t <- cases)
      if(TauOrdering.equiv(t,tau))
        return Unit
    cases.add(tau)
  }
}

case class SkolemCall(skolem: SkolemConstant,parameters: List[TauVariable],open: Option[TauVariable] = None) extends RhoType {
  override def tagged: Boolean = false
  override def contents: List[TauType] = skolem.contents ++ parameters
  override def map(f: (TauType) => TauType): SkolemCall = {
    val params = parameters.map(tvar => f(tvar)).filter(tau => tau.isInstanceOf[TauVariable]).asInstanceOf[List[TauVariable]]
    SkolemCall(skolem.map(f),params)
  }
  override def compile: LLVMType = skolem.compile
  override def mangle: String = skolem.toString + "<" + parameters.head.toString + parameters.tail.foldLeft("")((rest: String,tvar: TauVariable) => rest + "," + tvar.toString) + ">"
}

object ExistentialConstants {
  protected val constants: Map[Tuple2[RhoType,TauType],SkolemConstant] = new HashMap[Tuple2[RhoType,TauType],SkolemConstant]()
  
  def existentiallyQuantify(rho: RhoType,tau: TauType): SkolemCall = constants.get((rho,tau)) match {
    case Some(skolem) => SkolemCall(skolem,Nil)
    case None => {
      val skolem = new SkolemConstant
      skolem.skolemize(tau)
      constants.put((rho,tau),skolem)
      SkolemCall(skolem,Nil)
    }
  }
  
  def pack(rho: RhoType,tau: TauType): RhoType = rho.replace(tau,existentiallyQuantify(rho,tau))
}
