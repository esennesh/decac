package org.deca.compiler

import scala.collection.immutable.Set

object ReadOnlyMutability extends MonoMutability {
  override def variables: Set[SignatureVariable] = Set.empty
}
object ImmutableMutability extends MonoMutability {
  override def variables: Set[SignatureVariable] = Set.empty
}
object MutableMutability extends MonoMutability {
  override def variables: Set[SignatureVariable] = Set.empty
}

class BoundedMutabilityVariable(mu: MonoMutability,bnd: SignatureBound,univ: Boolean) extends BoundsVariable[MonoMutability](mu,bnd,univ) with MonoMutability {
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = signature.filterR(pred)
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = signature.filterT(pred)
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = signature.filterE(pred)
  override def clone(sig: MonoMutability,bnd: SignatureBound,univ: Boolean) = new BoundedMutabilityVariable(sig,bnd,univ)
}

/* object RegionRelation extends InferenceOrdering[MonoRegion] {
  override protected val lattice = new GraphLattice(BottomRegion,GlobalRegion)(RegionOrdering)
  def lt(x: MonoRegion,y: MonoRegion): Option[Set[InferenceConstraint]] = (x,y) match {
    case (BottomRegion,_) => Some(HashSet.empty)
    case (_,GlobalRegion) => Some(HashSet.empty)
    case (ScopeRegion(sx),ScopeRegion(sy)) => if(sx enclosedIn sy) Some(HashSet.empty) else None
    case (RegionVariable(_),RegionVariable(true)) => Some(HashSet.empty)
    case (RegionVariable(false),RegionVariable(false)) => Some(HashSet.empty[InferenceConstraint] + SubsumptionConstraint(x,y))
    case (RegionVariable(true),ScopeRegion(sy: Module)) => Some(HashSet.empty)
    case (ScopeRegion(sx: LexicalScope),RegionVariable(false)) => Some(HashSet.empty[InferenceConstraint] + (SubsumptionConstraint(x,y)))
    case (RegionVariable(false),ScopeRegion(sx: LexicalScope)) => Some(HashSet.empty[InferenceConstraint] + (SubsumptionConstraint(x,y)))
    case _ => None
  }
  def equiv(x: MonoRegion,y: MonoRegion): Option[Set[InferenceConstraint]] = (x,y) match {
    case (ScopeRegion(sx),ScopeRegion(sy)) => if(sx == sy) Some(HashSet.empty) else None
    case (RegionVariable(fx),RegionVariable(fy)) => if(fx == fy) Some(HashSet.empty[InferenceConstraint] + EqualityConstraint(x,y)) else None
    case _ => None
  }
}

object RegionOrdering extends PartialOrdering[MonoRegion] {
  override def lt(x: MonoRegion,y: MonoRegion): Boolean = RegionRelation.lt(x,y) match {
    case Some(constraints) => constraints.isEmpty
    case None => false
  }
  override def equiv(x: MonoRegion,y: MonoRegion): Boolean = RegionRelation.equiv(x,y) match {
    case Some(constraints) => constraints.isEmpty
    case None => false
  }
  override def gt(x: MonoRegion,y: MonoRegion): Boolean = lt(y,x)
  override def lteq(x: MonoRegion,y: MonoRegion): Boolean = lt(x,y) || equiv(x,y)
  override def gteq(x: MonoRegion,y: MonoRegion): Boolean = gt(x,y) || equiv(x,y)
  override def tryCompare(x: MonoRegion,y: MonoRegion): Option[Int] = {
    if(gt(x,y))
      Some(1)
    else if(lt(x,y))
      Some(-1)
    else if(equiv(x,y))
      Some(0)
    else
      None
  }
}*/
