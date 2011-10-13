package org.deca.compiler

import scala.collection.immutable.HashSet
import scala.collection.mutable.GraphLattice

class EffectVariable extends MonoEffect with SignatureVariable {
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = HashSet.empty
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = HashSet.empty
}
case object PureEffect extends MonoEffect {
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = HashSet.empty
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = HashSet.empty
  override def variables: Set[SignatureVariable] = HashSet.empty[SignatureVariable]
}
case class ReadEffect(region: MonoRegion) extends MonoEffect {
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = HashSet.empty
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = region.filterR(pred)
  override def mapR(f: (MonoRegion) => MonoRegion): MonoEffect = ReadEffect(f(region))
  override def variables: Set[SignatureVariable] = region.variables
}
case class WriteEffect(region: MonoRegion) extends MonoEffect {
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = HashSet.empty
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = region.filterR(pred)
  override def mapR(f: (MonoRegion) => MonoRegion): MonoEffect = WriteEffect(f(region))
  override def variables: Set[SignatureVariable] = region.variables
}
case class DestroyEffect(region: MonoRegion) extends MonoEffect {
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = HashSet.empty
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = region.filterR(pred)
  override def mapR(f: (MonoRegion) => MonoRegion): MonoEffect = DestroyEffect(f(region))
  override def variables: Set[SignatureVariable] = region.variables
}
case class ThrowEffect(exception: MonoType) extends MonoEffect {
  assert(TypeOrdering.lteq(exception,ExceptionConstructor.represent(Nil)))
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = exception.filterT(pred)
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = HashSet.empty
  override def mapT(f: (MonoType) => MonoType): MonoEffect = ThrowEffect(f(exception))
  override def variables: Set[SignatureVariable] = exception.variables
}
case class CallEffect(module: Module) extends MonoEffect {
  override def variables: Set[SignatureVariable] = HashSet.empty[SignatureVariable]
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = HashSet.empty
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = HashSet.empty
}
case class SetEffect(effects: Set[MonoEffect]) extends MonoEffect {
  assert(effects.forall(effect => !effect.isInstanceOf[SetEffect]))
  def contains(effect: MonoEffect): Boolean = !filterE(eff => eff == effect).isEmpty
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = effects.map(eff => eff.filterT(pred)).foldLeft(HashSet.empty[MonoType].asInstanceOf[Set[MonoType]])((res: Set[MonoType],ts: Set[MonoType]) => res ++ ts)
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = effects.map(eff => eff.filterR(pred)).foldLeft(HashSet.empty[MonoRegion].asInstanceOf[Set[MonoRegion]])((res: Set[MonoRegion],ts: Set[MonoRegion]) => res ++ ts)
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = effects.map(eff => eff.filterE(pred)).foldLeft(Set.empty[MonoEffect])((res,ts) => res ++ ts) ++ (if(pred(this)) Set.empty[MonoEffect] + this else Set.empty[MonoEffect])
  override def mapR(f: (MonoRegion) => MonoRegion): MonoEffect = SetEffect(effects.map(eff => eff.mapR(f)))
  override def mapT(f: (MonoType) => MonoType): MonoEffect = SetEffect(effects.map(eff => eff.mapT(f)))
  override def mapE(f: (MonoEffect) => MonoEffect): MonoEffect = f(SetEffect(effects.map(eff => eff.mapE(f))))
  override def variables: Set[SignatureVariable] = effects.map(effect => effect.variables).foldLeft(HashSet.empty[SignatureVariable])((accum: HashSet[SignatureVariable],y: Set[SignatureVariable]) => accum ++ y)
}
object TopEffect extends MonoEffect {
  override def variables: Set[SignatureVariable] = HashSet.empty[SignatureVariable]
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = HashSet.empty
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = HashSet.empty
}

object EffectRelation extends InferenceOrdering[MonoEffect] {
  override protected val lattice = new GraphLattice(PureEffect,TopEffect)(EffectOrdering)
  def lt(x: MonoEffect,y: MonoEffect): Option[Set[InferenceConstraint]] = (x,y) match {
    case (PureEffect,_) => Some(HashSet.empty)
    case (_,TopEffect) => Some(HashSet.empty)
    case (SetEffect(ex),SetEffect(ey)) => if(ex.forall(eff => ey.contains(eff))) Some(HashSet.empty) else None
    case (_,SetEffect(effects)) => if(effects.contains(x)) Some(HashSet.empty) else None
    case (ReadEffect(rx),ReadEffect(ry)) => Some(HashSet.empty[InferenceConstraint] + SubsumptionConstraint(rx,ry))
    case (WriteEffect(rx),WriteEffect(ry)) => Some(HashSet.empty[InferenceConstraint] + SubsumptionConstraint(rx,ry))
    case (ThrowEffect(tx),ThrowEffect(ty)) => Some(HashSet.empty[InferenceConstraint] + SubsumptionConstraint(tx,ty))
    case (_,vy: EffectVariable) => if(vy.universal || x == vy) Some(HashSet.empty[InferenceConstraint]) else None
    case _ => None
  }
  def equiv(x: MonoEffect,y: MonoEffect): Option[Set[InferenceConstraint]] = (x,y) match {
    case (PureEffect,PureEffect) => Some(HashSet.empty[InferenceConstraint])
    case (SetEffect(ex),SetEffect(ey)) => if(ex.forall(eff => ey.contains(eff))) Some(HashSet.empty) else None
    case (ReadEffect(rx),ReadEffect(ry)) => Some(HashSet.empty[InferenceConstraint] + EqualityConstraint(rx,ry))
    case (WriteEffect(rx),WriteEffect(ry)) => Some(HashSet.empty[InferenceConstraint] + EqualityConstraint(rx,ry))
    case (ThrowEffect(tx),ThrowEffect(ty)) => Some(HashSet.empty[InferenceConstraint] + EqualityConstraint(tx,ty))
    case (CallEffect(mx),CallEffect(my)) => if(mx == my) Some(HashSet.empty[InferenceConstraint]) else None
    case (vx: EffectVariable,vy: EffectVariable) => if(vx eq vy) Some(HashSet.empty[InferenceConstraint]) else None
    case _ => None
  }
}

object EffectOrdering extends PartialOrdering[MonoEffect] {
  override def lt(x: MonoEffect,y: MonoEffect): Boolean = EffectRelation.lt(x,y) match {
    case Some(constraints) => constraints.isEmpty
    case None => false
  }
  override def equiv(x: MonoEffect,y: MonoEffect): Boolean = EffectRelation.equiv(x,y) match {
    case Some(constraints) => constraints.isEmpty
    case None => false
  }
  override def gt(x: MonoEffect,y: MonoEffect): Boolean = lt(y,x)
  override def lteq(x: MonoEffect,y: MonoEffect): Boolean = lt(x,y) || equiv(x,y)
  override def gteq(x: MonoEffect,y: MonoEffect): Boolean = gt(x,y) || equiv(x,y)
  override def tryCompare(x: MonoEffect,y: MonoEffect): Option[Int] = {
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
