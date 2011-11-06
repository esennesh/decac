package org.deca.compiler.signature

import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.collection.mutable.Lattice
import org.deca.compiler.definition._

class SignatureSubstitution {
  protected val queue: Queue[Tuple2[SignatureVariable,MonoSignature]] = new Queue[Tuple2[SignatureVariable,MonoSignature]]
  
  def substitute(x: SignatureVariable,y: MonoSignature): Unit = {
    assert(!x.universal)
    queue.enqueue((x,y))
  }
  def isEmpty: Boolean = queue.isEmpty
  
  def solve[T <: MonoSignature](sig: T): T = {
    val substituted: T = queue.foldLeft(sig)((result: MonoSignature,sub: Tuple2[SignatureVariable,MonoSignature]) => sub._1 match {
      case t: MonoType => sig.mapT((st: MonoType) => if(st == sub._1) sub._2.asInstanceOf[MonoType] else st).asInstanceOf[T]
      case e: MonoEffect => sig.mapE((se: MonoEffect) => if(se == sub._1) sub._2.asInstanceOf[MonoEffect] else se).asInstanceOf[T]
      case r: MonoRegion => sig.mapR((sr: MonoRegion) => if(sr == sub._1) sub._2.asInstanceOf[MonoRegion] else sr).asInstanceOf[T]
    })
    val typeBounded: T = substituted.mapT((sigprime: MonoType) => sigprime match {
      case bounded: BoundsVariable[MonoType] => {
        assert(!bounded.universal)
        bounded.signature
      }
      case _ => sigprime
    }).asInstanceOf[T]
    val effectBounded: T = typeBounded.mapE((sigprime: MonoEffect) => sigprime match {
      case bounded: BoundsVariable[MonoEffect] => {
        assert(!bounded.universal)
        bounded.signature
      }
      case _ => sigprime
    }).asInstanceOf[T]
    val regionBounded: T = effectBounded.mapR((sigprime: MonoRegion) => sigprime match {
      case bounded: BoundsVariable[MonoRegion] => {
        assert(!bounded.universal)
        bounded.signature
      }
      case _ => sigprime
    }).asInstanceOf[T]
    regionBounded
  }
}

class SignatureConstraints {
  val current = new Stack[InferenceConstraint]
  val polyHypotheses = new Queue[InferenceConstraint]
  
  def substitute(x: SignatureVariable,y: MonoSignature): Unit = {
    for(constraint <- current)
      constraint.substitute(x,y)
    for(hypoth <- polyHypotheses) {
      hypoth.substitute(x,y)
      (hypoth.alpha,hypoth.beta) match {
        case (vx: SignatureVariable,vy: SignatureVariable) => if(vx.isInstanceOf[BoundsVariable[_]] || vy.isInstanceOf[BoundsVariable[_]]) { current.push(hypoth); polyHypotheses.dequeueFirst(h => h == hypoth) }
        case _ => {
          current.push(hypoth)
          polyHypotheses.dequeueFirst(h => h == hypoth)
        }
      }
    }
  }
  
  def push(c: InferenceConstraint): Unit = (c.alpha,c.beta) match {
    case (vx: SignatureVariable,vy: SignatureVariable) => {
      if(!vx.isInstanceOf[BoundsVariable[_]] && !vy.isInstanceOf[BoundsVariable[_]])
        polyHypotheses.enqueue(c)
      else
        current.push(c)
    }
    case _ => current.push(c)
  }
  
  def pop: InferenceConstraint = {
    if(current.isEmpty)
      polyHypotheses.dequeue
    else
      current.pop
  }
  
  def isEmpty = current.isEmpty && polyHypotheses.isEmpty
}

class LatticeUnificationInstance(subst: Option[SignatureSubstitution] = None) {
  protected val constraints = new SignatureConstraints
  protected val result = subst match {
    case Some(s) => s
    case None => new SignatureSubstitution
  }
  
  def constrain(c: InferenceConstraint): Unit = constraints.push(c)
  
  def substitute(x: SignatureVariable,y: MonoSignature): Unit = {
    result.substitute(x,y)
    constraints.substitute(x,y)
  }
  
  def solve: SignatureSubstitution = {
    while(!constraints.isEmpty) {
      val constraint = constraints.pop
      val cs: Option[Set[InferenceConstraint]] = constraint match {
        case SubsumptionConstraint(x,y) => (x,y) match {
          case (tx: MonoType,ty: MonoType) => TypeRelation.lt(tx,ty)
          case (ex: MonoEffect,ey: MonoEffect) => EffectRelation.lt(ex,ey)
          case (rx: MonoRegion,ry: MonoRegion) => RegionRelation.lt(rx,ry)
        }
        case PhysicalSubtypingConstraint(x,y) => PhysicalTypeRelation.lt(x,y)
        case EqualityConstraint(x,y) => (x,y) match {
          case (tx: MonoType,ty: MonoType) => TypeRelation.equiv(tx,ty)
          case (ex: MonoEffect,ey: MonoEffect) => EffectRelation.equiv(ex,ey)
          case (rx: MonoRegion,ry: MonoRegion) => RegionRelation.equiv(rx,ry)
        }
      }
      for(c <- cs.getOrElse(throw new Exception("Unsatisfiable signature constraint: " + constraint.toString))) c match {
        case PhysicalSubtypingConstraint(vx: BoundsVariable[MonoType],vy: BoundsVariable[MonoType]) => {
          val meets = PhysicalTypeRelation.meet(vy.signature,vx.signature)
          val px = vx.meet(meets._1)
          val joins = PhysicalTypeRelation.join(vy.signature,vx.signature)
          val py = vy.join(joins._1)
          (meets._2 ++ joins._2 + px._1 + py._1).foreach(c => constrain(c))
          substitute(vx,px._2)
          substitute(vy,py._2)
        }
        case PhysicalSubtypingConstraint(vx: BoundsVariable[MonoType],ty) => {
          val meets = PhysicalTypeRelation.meet(vx.signature,ty)
          val pair = vx.meet(meets._1)
          (meets._2 + pair._1).foreach(c => constrain(c))
          substitute(vx,pair._2)
        }
        case PhysicalSubtypingConstraint(tx,vy: BoundsVariable[MonoType]) => {
          val joins = PhysicalTypeRelation.join(tx,vy.signature)
          val pair = vy.join(joins._1)
          (joins._2 + pair._1).foreach(c => constrain(c))
          substitute(vy,pair._2)
        }
        case PhysicalSubtypingConstraint(vx: TypeVariable,ty: MonoType) => substitute(vx,new BoundedTypeVariable(ty,MeetBound,false))
        case PhysicalSubtypingConstraint(tx: MonoType,vy: TypeVariable) => substitute(vy,new BoundedTypeVariable(tx,JoinBound,false))
        case PhysicalSubtypingConstraint(vx: SignatureVariable,vy: SignatureVariable) => constrain(c)
        case SubsumptionConstraint(vx: BoundsVariable[MonoSignature],vy: BoundsVariable[MonoSignature]) => {
          val meets = SignatureRelation.meet(vy.signature,vx.signature)
          val px = vx.meet(meets._1)
          val joins = SignatureRelation.join(vy.signature,vx.signature)
          val py = vy.join(joins._1)
          (joins._2 ++ meets._2 + px._1 + py._1).map(c => constrain(c))
          substitute(vx,px._2)
          substitute(vy,py._2)
        }
        case SubsumptionConstraint(vx: BoundsVariable[MonoSignature],_) => {
          val meets = SignatureRelation.meet(vx.signature,c.beta)
          val pair = vx.meet(meets._1)
          (meets._2 + pair._1).map(c => constrain(c))
          substitute(vx,pair._2)
        }
        case SubsumptionConstraint(_,vy: BoundsVariable[MonoSignature]) => {
          val joins = SignatureRelation.join(c.alpha,vy.signature)
          val pair = vy.join(joins._1)
          (joins._2 + pair._1).map(c => constrain(c))
          substitute(vy,pair._2)
        }
        case SubsumptionConstraint(vx: TypeVariable,ty: MonoType) => substitute(vx,new BoundedTypeVariable(ty,MeetBound,false))
        case SubsumptionConstraint(tx: MonoType,vy: TypeVariable) => substitute(vy,new BoundedTypeVariable(tx,JoinBound,false))
        case SubsumptionConstraint(vx: SignatureVariable,vy: SignatureVariable) => constrain(c)
        case EqualityConstraint(vx: SignatureVariable,vy: SignatureVariable) => substitute(vx,vy)
        case EqualityConstraint(vx: SignatureVariable,_) => substitute(vx,c.beta)
        case EqualityConstraint(_,vy: SignatureVariable) => substitute(vy,c.alpha)
      }
    }
    result
  }
}
