package decac

import scala.collection.mutable._
import jllvm._

class SkolemConstant extends RhoType {
  protected val cases: Set[TauType] = new HashSet[TauType]()
  
  override def tagged: Boolean = false
  
  override def map(f: (TauType) => TauType): SkolemConstant = {
    val result = new SkolemConstant
    cases.foreach(c => result.skolemize(c))
    result
  }
  
  override def contents: List[TauType] = {
    var result: List[TauType] = Nil
    for(c <- cases.toList)
      c match {
        case rho: RhoType => result = result ++ List(rho) ++ rho.contents
        case _ => result = c :: result
      }
    result
  }
  
  override def compile: LLVMType = {
    val max = cases.toList.sortWith((x,y) => (x,y) match {
      case (x: GammaType,y: GammaType) => x.sizeOf >= y.sizeOf
      case (x: GammaType,_) => true
      case _ => false
    }).head
    max.asInstanceOf[GammaType].compile
  }
  override def mangle: String = getClass().getName() + '@' + Integer.toHexString(hashCode())
  
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

object ExistentialConstants {
  protected val constants: Map[Tuple2[RhoType,TauType],SkolemConstant] = new HashMap[Tuple2[RhoType,TauType],SkolemConstant]()
  
  def pack(rho: RhoType,tau: TauType): SkolemConstant = constants.get((rho,tau)) match {
    case Some(skolem) => skolem
    case None => {
      val result = new SkolemConstant
      result.skolemize(tau)
      constants.put((rho,tau),result)
      result
    }
  }
}

abstract class ExistentialVariable {
  def contents: TauType
}
case class PackedType(tau: TauType) extends ExistentialVariable {
  override def contents: TauType = tau
}
case class PackedSkolem(skolem: SkolemConstant) extends ExistentialVariable {
  override def contents: SkolemConstant = skolem
}

class ExistentialType(rho: RhoType,alpha: ExistentialVariable) extends RhoType {
  val skolem: SkolemConstant = alpha match {
    case PackedType(alpha) => ExistentialConstants.pack(rho,alpha)
    case PackedSkolem(s) => s
  }
  val internals = rho.map(tau => if(TauOrdering.equiv(tau,alpha.contents)) skolem else tau)
  
  assert({
    val funcs = rho.filter(tau => tau.isInstanceOf[FunctionArrow])
    val safeToCall = funcs.filter(tau => tau == alpha).length == funcs.filter(tau => tau match { case p: PointerType => p.target == alpha case _ => false }).length
    val notHidingVars = internals.filter(tau => tau.isInstanceOf[TauVariable]).forall(tvar => rho.filter(tau => tau == tvar) != Nil)
    safeToCall && notHidingVars
  })
  
  override def tagged: Boolean = false
  
  override def map(f: (TauType) => TauType): ExistentialType = {
    val s = skolem.map(f)
    val b = internals.map(tau => if(tau == skolem) s else tau).map(f)
    new ExistentialType(b,PackedSkolem(s))
  }
  
  override def contents: List[TauType] = {
    skolem.contents ++ internals.contents
  }
  
  override def mangle: String = {
    val alpha = new TauVariable
    "∃" + alpha.toString + ". " + internals.map(tau => if(tau == skolem) alpha else tau).toString
  }
  
  override def compile: LLVMType = internals.compile 
}
