package org.deca.compiler.signature

import scala.collection.immutable.HashSet
import scala.collection.immutable.Set
import scala.collection.mutable.GraphLattice
import org.deca.compiler.definition._

class EffectVariable(override val universal: Boolean,override val name: Option[String] = None) extends MonoEffect with SignatureVariable {
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = Set.empty
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = Set.empty
}
case class ReadEffect(region: MonoRegion) extends MonoEffect {
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = Set.empty
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = region.filterR(pred)
  override def mapR(f: (MonoRegion) => MonoRegion): MonoEffect = ReadEffect(f(region))
  override def variables: Set[SignatureVariable] = region.variables
}
case class WriteEffect(region: MonoRegion) extends MonoEffect {
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = Set.empty
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = region.filterR(pred)
  override def mapR(f: (MonoRegion) => MonoRegion): MonoEffect = WriteEffect(f(region))
  override def variables: Set[SignatureVariable] = region.variables
}
case class DestroyEffect(region: MonoRegion) extends MonoEffect {
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = Set.empty
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = region.filterR(pred)
  override def mapR(f: (MonoRegion) => MonoRegion): MonoEffect = DestroyEffect(f(region))
  override def variables: Set[SignatureVariable] = region.variables
}
case class ThrowEffect(exception: MonoType) extends MonoEffect {
  assert(TypeOrdering.lteq(exception,ExceptionConstructor.represent(Nil)))
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = exception.filterT(pred)
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = Set.empty
  override def mapT(f: (MonoType) => MonoType): MonoEffect = ThrowEffect(f(exception))
  override def variables: Set[SignatureVariable] = exception.variables
}
case class CallEffect(region: MonoRegion) extends MonoEffect {
  override def variables: Set[SignatureVariable] = Set.empty[SignatureVariable]
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = Set.empty
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = Set.empty
}
case class SetEffect(var effects: Set[MonoEffect]) extends MonoEffect {
  effects = effects.foldLeft(Set.empty[MonoEffect])((set,effect) => set ++ (effect match {
    case setEffect: SetEffect => setEffect.effects
    case _ => Set.empty + effect
  }))
  assert(effects.forall(effect => !effect.isInstanceOf[SetEffect] && effect != PureEffect))
  def contains(effect: MonoEffect): Boolean = !filterE(eff => eff == effect).isEmpty
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = effects.map(eff => eff.filterT(pred)).foldLeft(Set.empty[MonoType])((res: Set[MonoType],ts: Set[MonoType]) => res ++ ts)
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = effects.map(eff => eff.filterR(pred)).foldLeft(Set.empty[MonoRegion])((res: Set[MonoRegion],ts: Set[MonoRegion]) => res ++ ts)
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = effects.map(eff => eff.filterE(pred)).foldLeft(Set.empty[MonoEffect])((res,ts) => res ++ ts) ++ (if(pred(this)) Set.empty[MonoEffect] + this else Set.empty[MonoEffect])
  override def mapR(f: (MonoRegion) => MonoRegion): MonoEffect = SetEffect(effects.map(eff => eff.mapR(f)))
  override def mapT(f: (MonoType) => MonoType): MonoEffect = SetEffect(effects.map(eff => eff.mapT(f)))
  override def mapE(f: (MonoEffect) => MonoEffect): MonoEffect = f(SetEffect(effects.map(eff => eff.mapE(f))))
  override def variables: Set[SignatureVariable] = effects.map(effect => effect.variables).foldLeft(Set.empty[SignatureVariable])((accum: Set[SignatureVariable],y: Set[SignatureVariable]) => accum ++ y)
}

object PureEffect extends SetEffect(Set.empty)

object TopEffect extends MonoEffect {
  override def variables: Set[SignatureVariable] = Set.empty[SignatureVariable]
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = Set.empty
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = Set.empty
}

class BoundedEffectVariable(epsilon: MonoEffect,bnd: SignatureBound,univ: Boolean,override val name: Option[String] = None) extends BoundsVariable[MonoEffect](epsilon,bnd,univ) with MonoEffect {
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = signature.filterR(pred)
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = signature.filterT(pred)
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = if(pred(this)) signature.filterE(pred) + this else signature.filterE(pred)
  override def clone(sig: MonoEffect,bnd: SignatureBound,univ: Boolean,nm: Option[String]) = new BoundedEffectVariable(sig,bnd,univ,nm orElse name)
}

object EffectRelation extends InferenceOrdering[MonoEffect] {
  override protected val lattice = new GraphLattice(TopEffect,PureEffect)(EffectOrdering)
  def lt(x: MonoEffect,y: MonoEffect): Option[Set[InferenceConstraint]] = (x,y) match {
    case (bx: BoundedEffectVariable,by: BoundedEffectVariable) => lt(bx.signature,by.signature)
    case (_,by: BoundedEffectVariable) => lt(x,by.signature)
    case (bx: BoundedEffectVariable,_) => lt(bx.signature,y)
    case (vx: EffectVariable,vy: EffectVariable) =>
      if(vy.universal || vx == vy)
        Some(Set.empty)
      else if(!vx.universal)
        Some(Set.empty + SubsumptionConstraint(vx,vy))
      else
        None
    case (_,vy: EffectVariable) => if(vy.universal) Some(Set.empty) else Some(Set.empty + SubsumptionConstraint(x,vy))
    case (vx: EffectVariable,_) => Some(Set.empty + SubsumptionConstraint(vx,y))
    case (PureEffect,_) => Some(Set.empty)
    case (_,TopEffect) => Some(Set.empty)
    case (SetEffect(ex),SetEffect(ey)) => if(ex.forall(eff => ey.contains(eff))) Some(Set.empty) else None
    case (_,SetEffect(effects)) => if(effects.contains(x)) Some(Set.empty) else None
    case (ReadEffect(rx),ReadEffect(ry)) => Some(Set.empty[InferenceConstraint] + SubsumptionConstraint(rx,ry))
    case (WriteEffect(rx),WriteEffect(ry)) => Some(Set.empty[InferenceConstraint] + SubsumptionConstraint(rx,ry))
    case (ThrowEffect(tx),ThrowEffect(ty)) => Some(Set.empty[InferenceConstraint] + SubsumptionConstraint(tx,ty))
    case _ => None
  }
  def equiv(x: MonoEffect,y: MonoEffect): Option[Set[InferenceConstraint]] = (x,y) match {
    case (TopEffect,TopEffect) => Some(Set.empty)
    case (SetEffect(ex),SetEffect(ey)) => if(ex.forall(ey.contains(_)) && ey.forall(ex.contains(_))) Some(Set.empty) else None
    case (ReadEffect(rx),ReadEffect(ry)) => RegionRelation.equiv(rx,ry)
    case (WriteEffect(rx),WriteEffect(ry)) => RegionRelation.equiv(rx,ry)
    case (ThrowEffect(tx),ThrowEffect(ty)) => TypeRelation.equiv(tx,ty)
    case (CallEffect(rx),CallEffect(ry)) => RegionRelation.equiv(rx,ry)
    case (vx: EffectVariable,vy: EffectVariable) =>
      if(vx eq vy)
        Some(Set.empty)
      else if(!vx.universal || !vy.universal)
        Some(Set.empty + EqualityConstraint(x,y))
      else
        None
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
