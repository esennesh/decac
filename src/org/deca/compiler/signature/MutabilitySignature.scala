package org.deca.compiler.signature

import scala.collection.immutable.Set
import scala.collection.mutable.GraphLattice
import org.deca.compiler.definition._

object ReadOnlyMutability extends MonoMutability {
  override def variables: Set[SignatureVariable] = Set.empty
}
object ImmutableMutability extends MonoMutability {
  override def variables: Set[SignatureVariable] = Set.empty
}
object MutableMutability extends MonoMutability {
  override def variables: Set[SignatureVariable] = Set.empty
}
object BottomMutability extends MonoMutability {
  override def variables: Set[SignatureVariable] = Set.empty
}
class MutabilityVariable extends SignatureVariable with MonoMutability
class BoundedMutabilityVariable(mu: MonoMutability,bnd: SignatureBound,univ: Boolean) extends BoundsVariable[MonoMutability](mu,bnd,univ) with MonoMutability {
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = signature.filterR(pred)
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = signature.filterT(pred)
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = signature.filterE(pred)
  override def clone(sig: MonoMutability,bnd: SignatureBound,univ: Boolean,nm: Option[String]) = new BoundedMutabilityVariable(sig,bnd,univ)
}

object MutabilityRelation extends InferenceOrdering[MonoMutability] {
  override val lattice = new GraphLattice(ReadOnlyMutability,BottomMutability)(MutabilityOrdering)
  override def lt(x: MonoMutability,y: MonoMutability): Option[Set[InferenceConstraint]] = (x,y) match {
    case (_,ReadOnlyMutability) => Some(Set.empty)
    case (mx: MonoMutability,vy: MutabilityVariable) => if(vy.universal) Some(Set.empty) else Some(Set.empty + SubsumptionConstraint(mx,vy))
    case (vx: MutabilityVariable,my: MonoMutability) => Some(Set.empty + SubsumptionConstraint(vx,my))
    case _ => if(x == y) Some(Set.empty) else None
  }
  override def equiv(x: MonoMutability,y: MonoMutability): Option[Set[InferenceConstraint]] = (x,y) match {
    case (vx: MutabilityVariable,_) => Some(Set.empty + EqualityConstraint(vx,y))
    case (_,vy: MutabilityVariable) => Some(Set.empty + EqualityConstraint(x,vy))
    case _ => if(x == y) Some(Set.empty) else None
  }
}

object MutabilityOrdering extends PartialOrdering[MonoMutability] {
  override def lt(x: MonoMutability,y: MonoMutability): Boolean = MutabilityRelation.lt(x,y) match {
    case Some(constraints) => constraints.isEmpty
    case None => false
  }
  override def equiv(x: MonoMutability,y: MonoMutability): Boolean = MutabilityRelation.equiv(x,y) match {
    case Some(constraints) => constraints.isEmpty
    case None => false
  }
  override def gt(x: MonoMutability,y: MonoMutability): Boolean = lt(y,x)
  override def lteq(x: MonoMutability,y: MonoMutability): Boolean = lt(x,y) || equiv(x,y)
  override def gteq(x: MonoMutability,y: MonoMutability): Boolean = gt(x,y) || equiv(x,y)
  override def tryCompare(x: MonoMutability,y: MonoMutability): Option[Int] = {
    if(gt(x,y))
      Some(1)
    else if(lt(x,y))
      Some(-1)
    else if(equiv(x,y))
      Some(0)
    else
      None
  }
}
