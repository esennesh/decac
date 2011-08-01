package decac

import scala.collection.immutable.HashSet

case class ScopeRegion(scope: Scope) extends MonoRegion
case class RegionVariable(formal: Boolean) extends SignatureVariable[MonoRegion] {
  override val universal = formal
}

object RegionRelation extends InferenceOrdering[MonoRegion] {
  def lt(x: MonoRegion,y: MonoRegion): Option[Set[InferenceConstraint[MonoRegion]]] = (x,y) match {
    case (ScopeRegion(sx),ScopeRegion(sy)) => if(sx enclosedIn sy) Some(HashSet.empty) else None
    case (RegionVariable(_),RegionVariable(true)) => Some(HashSet.empty)
    case (RegionVariable(false),RegionVariable(false)) => Some(HashSet.empty + SubsumptionConstraint(x,y))
    case (RegionVariable(true),ScopeRegion(sy: Module)) => Some(HashSet.empty)
    case (ScopeRegion(sx: LexicalScope),RegionVariable(false)) => Some(HashSet.empty + SubsumptionConstraint(x,y))
    case (RegionVariable(false),ScopeRegion(sx: LexicalScope)) => Some(HashSet.empty + SubsumptionConstraint(x,y))
    case _ => None
  }
  def equiv(x: MonoRegion,y: MonoRegion): Option[Set[InferenceConstraint[MonoRegion]]] = (x,y) match {
    case (ScopeRegion(sx),ScopeRegion(sy)) => if(sx == sy) Some(HashSet.empty) else None
    case (RegionVariable(fx),RegionVariable(fy)) => if(fx == fy) Some(HashSet.empty + EqualityConstraint(x,y)) else None
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
