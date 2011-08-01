package decac

import scala.collection.immutable.HashSet

case object PureEffect extends MonoEffect
case class ReadEffect(region: MonoRegion) extends MonoEffect {
  override def map(f: (MonoRegion) => MonoRegion): MonoEffect = ReadEffect(f(region))
}
case class WriteEffect(region: MonoRegion) extends MonoEffect {
  override def map(f: (MonoRegion) => MonoRegion): MonoEffect = WriteEffect(f(region))
}
case class DestroyEffect(region: MonoRegion) extends MonoEffect {
  override def map(f: (MonoRegion) => MonoRegion): MonoEffect = DestroyEffect(f(region))
}
case class ThrowEffect(exception: MonoType) extends MonoEffect {
  assert(TypeOrdering.lteq(exception,ExceptionConstructor.represent(Nil)))
  override def map(f: (MonoType) => MonoType): MonoEffect = ThrowEffect(f(exception))
}
case class CallEffect(module: Module) extends MonoEffect
case class SetEffect(effects: Set[MonoEffect]) extends MonoEffect {
  assert(effects.forall(effect => !effect.isInstanceOf[SetEffect]))
  override def map(f: (MonoRegion) => MonoRegion): MonoEffect = SetEffect(effects.map(f))
  override def map(f: (MonoType) => MonoType): MonoEffect = SetEffect(effects.map(f))
  override def map(f: (MonoEffect) => MonoEffect): MonoEffect = f(SetEffect(effects.map(f)))
  override def variables: Set[SignatureVariable[MonoEffect]] = effects.map(effect => effect.variables).foldLeft(HashSet.empty)((accum: Set[SignatureVariable[MonoEffect]],y: Set[SignatureVariable[MonoEffect]]) => accum + y)
}

object EffectRelation extends InferenceOrdering[MonoEffect] {
  def lt(x: MonoEffect,y: MonoEffect): Option[Set[InferenceConstraint[MonoSignature]]] = (x,y) match {
    case (PureEffect,_) => Some(HashSet.empty)
    case (SetEffect(ex),SetEffect(ey)) => if(ex.forall(eff => ey.contains(eff))) Some(HashSet.empty) else None
    case (_,SetEffect(effects)) => if(effects.contains(x)) Some(HashSet.empty) else None
    case (ReadEffect(rx),ReadEffect(ry)) => Some(HashSet.empty.add(SubsumptionConstraint(rx,ry)))
    case (WriteEffect(rx),WriteEffect(ry)) => Some(HashSet.empty.add(SubsumptionConstraint(rx,ry)))
    case (ThrowEffect(tx),ThrowEffect(ty)) => Some(HashSet.empty.add(SubsumptionConstraint(tx,ty)))
    case (_,vy: EffectVariable) => if(vy.universal || x == vy) Some(HashSet.empty) else None
    case _ => None
  }
  def equiv(x: MonoEffect,y: MonoEffect): Option[Set[InferenceConstraint[MonoSignature]]] = (x,y) match {
    case (PureEffect,PureEffect) => Some(HashSet.empty)
    case (SetEffect(ex),SetEffect(ey)) => if(ex.forall(eff => ey.contains(eff))) Some(HashSet.empty) else None
    case (ReadEffect(rx),ReadEffect(ry)) => Some(HashSet.empty.add(EqualityConstraint(rx,ry)))
    case (WriteEffect(rx),WriteEffect(ry)) => Some(HashSet.empty.add(EqualityConstraint(rx,ry)))
    case (ThrowEffect(tx),ThrowEffect(ty)) => Some(HashSet.empty.add(EqualityConstraint(tx,ty)))
    case (CallEffect(mx),CallEffect(my)) => if(mx == my) Some(HashSet.empty) else None
    case (vx: EffectVariable,vy: EffectVariable) => if(vx == vy) Some(HashSet.empty) else None
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
