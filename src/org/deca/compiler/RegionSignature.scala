package org.deca.compiler

import scala.collection.immutable.HashSet
import scala.collection.mutable.GraphLattice

case class ScopeRegion(scope: Scope) extends MonoRegion {
  override def variables: Set[SignatureVariable] = HashSet.empty[SignatureVariable]
}
object GlobalRegion extends ScopeRegion(GlobalScope)
object BottomRegion extends MonoRegion {
  override def variables: Set[SignatureVariable] = HashSet.empty
}
case class RegionVariable(formal: Boolean) extends MonoRegion with SignatureVariable {
  override val universal = formal
}

class BoundedRegionVariable(rho: MonoRegion,bnd: SignatureBound,univ: Boolean) extends BoundsVariable[MonoRegion](rho,bnd,univ) with MonoRegion {
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = if(pred(this)) signature.filterR(pred) + this else signature.filterR(pred)
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = signature.filterT(pred)
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = signature.filterE(pred)
  override def clone(sig: MonoRegion,bnd: SignatureBound,univ: Boolean) = new BoundedRegionVariable(sig,bnd,univ)
}

/* Normally, data "flows" through the signature system from smaller elements to larger elements in the
 * ordering.  Subtypes can be passed to supertypes, subeffects to supereffects, submutability to
 * supermutability.  However, for two pointers P(t,r) and P(t',r'), t <: t', and r' <: r.  The intuitive
 * ordering for regions flows contravariantly to the entire rest of the region system, and as a result bound
 * region variables have to be inferred towards their meet-bounds rather than their join-bounds.  I've hacked
 * around that here by just reversing the normal ordering, enabling bounds-variables to be coded as uniform
 * over all signature elements.  This ordering is "upside-down", so that data flows up the lattice.
 */
object RegionRelation extends InferenceOrdering[MonoRegion] {
  override protected val lattice = new GraphLattice(GlobalRegion,BottomRegion)(RegionOrdering)
  def lt(x: MonoRegion,y: MonoRegion): Option[Set[InferenceConstraint]] = (x,y) match {
    case (GlobalRegion,_) => Some(Set.empty)
    case (_,BottomRegion) => Some(Set.empty)
    case (ScopeRegion(sx),ScopeRegion(sy)) => if(sy enclosedIn sx) Some(Set.empty) else None
    case (RegionVariable(true),RegionVariable(_)) => Some(Set.empty)
    case (RegionVariable(false),RegionVariable(false)) => Some(Set.empty + SubsumptionConstraint(x,y))
    case (ScopeRegion(sx: Module),RegionVariable(true)) => Some(Set.empty)
    case (RegionVariable(false),ScopeRegion(sy: LexicalScope)) => Some(Set.empty + SubsumptionConstraint(x,y))
    case (ScopeRegion(sx: LexicalScope),RegionVariable(false)) => Some(Set.empty + SubsumptionConstraint(x,y))
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
}
