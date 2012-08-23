package org.deca.compiler.signature

import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.collection.mutable.Lattice
import org.deca.compiler.definition._

class SignatureSubstitution {
  protected val queue: Queue[(SignatureVariable,MonoSignature)] = new Queue[(SignatureVariable,MonoSignature)]
  
  def substitute(x: SignatureVariable,y: MonoSignature,specialize: Boolean = false): Unit = {
    assert(if(specialize) x.universal else !x.universal)
    queue.enqueue((x,y))
  }
  def isEmpty: Boolean = queue.isEmpty
  
  def solve[T <: MonoSignature](sig: T): T = {
    val substituted: T = queue.foldLeft(sig)((result: MonoSignature,sub: (SignatureVariable,MonoSignature)) => result.replace(sub._1,sub._2).asInstanceOf[T])
    val typeBounded: T = substituted.mapT((sigprime: MonoType) => sigprime match {
      case bounded: BoundsVariable[MonoType] => {
        assert(!bounded.universal)
        bounded.signature
      }
      /*case tvar: TypeVariable if !tvar.universal && solve(tvar) == tvar => {
        val result = new TypeVariable(true,tvar.name)
        substitute(tvar,result)
        result
      }*/
      case _ => sigprime
    }).asInstanceOf[T]
    val effectBounded: T = typeBounded.mapE((sigprime: MonoEffect) => sigprime match {
      case bounded: BoundsVariable[MonoEffect] => {
        assert(!bounded.universal)
        bounded.signature
      }
      /*case evar: EffectVariable if !evar.universal && solve(evar) == evar => {
        val result = new EffectVariable(true)
        substitute(evar,result)
        result
      }*/
      case _ => sigprime
    }).asInstanceOf[T]
    val regionBounded: T = effectBounded.mapR((sigprime: MonoRegion) => sigprime match {
      case bounded: BoundsVariable[MonoRegion] => {
        assert(!bounded.universal)
        bounded.signature
      }
      /*case rvar: RegionVariable if !rvar.universal && solve(rvar) == rvar => {
        val result = new RegionVariable(true)
        substitute(rvar,result)
        result
      }*/
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
    for(hypothesis <- polyHypotheses)
      hypothesis.substitute(x,y)
    polyHypotheses.dequeueAll(!_.polymorphic).map(current.push(_))
  }
  
  def push(c: InferenceConstraint): Unit =
    if(c.polymorphic)
      polyHypotheses.enqueue(c)
    else
      current.push(c)
  
  def pop: InferenceConstraint =
    if(current.isEmpty) {
      val sub = polyHypotheses.dequeue
      EqualityConstraint(sub.alpha,sub.beta)
    }
    else
      current.pop
  
  def isEmpty = current.isEmpty && polyHypotheses.isEmpty
}

class LatticeUnificationInstance(protected val result: SignatureSubstitution = new SignatureSubstitution) {
  val constraints = new SignatureConstraints
  
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
          case (mx: MonoMutability,my: MonoMutability) => MutabilityRelation.lt(mx,my)
        }
        case PhysicalSubtypingConstraint(x,y) => PhysicalTypeRelation.lt(x,y)
        case EqualityConstraint(x,y) => (x,y) match {
          case (tx: MonoType,ty: MonoType) => TypeRelation.equiv(tx,ty)
          case (ex: MonoEffect,ey: MonoEffect) => EffectRelation.equiv(ex,ey)
          case (rx: MonoRegion,ry: MonoRegion) => RegionRelation.equiv(rx,ry)
          case (mx: MonoMutability,my: MonoMutability) => MutabilityRelation.equiv(mx,my)
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
        case SubsumptionConstraint(vx: BoundsVariable[MonoSignature],vy: SignatureVariable) =>
          constrain(SubsumptionConstraint(vx.signature,vy))
        case SubsumptionConstraint(vx: BoundsVariable[MonoSignature],_) => {
          val meets = SignatureRelation.meet(vx.signature,c.beta)
          val pair = vx.meet(meets._1)
          (meets._2 + pair._1).map(c => constrain(c))
          substitute(vx,pair._2)
        }
        case SubsumptionConstraint(vx: SignatureVariable,vy: BoundsVariable[MonoSignature]) =>
          constrain(SubsumptionConstraint(vx,vy.signature))
        case SubsumptionConstraint(_,vy: BoundsVariable[MonoSignature]) => {
          val joins = SignatureRelation.join(c.alpha,vy.signature)
          val pair = vy.join(joins._1)
          (joins._2 + pair._1).map(c => constrain(c))
          substitute(vy,pair._2)
        }
        case SubsumptionConstraint(ex: MonoEffect,vy: EffectVariable) => substitute(vy,new BoundedEffectVariable(ex,JoinBound,false))
        case SubsumptionConstraint(vx: EffectVariable,ey: MonoEffect) => substitute(vx,new BoundedEffectVariable(ey,MeetBound,false))
        //case SubsumptionConstraint(vx: SignatureVariable,vy: SignatureVariable) => constrain(c)
        case SubsumptionConstraint(vx: TypeVariable,ty: MonoType) => substitute(vx,new BoundedTypeVariable(ty,MeetBound,false))
        case SubsumptionConstraint(tx: MonoType,vy: TypeVariable) => substitute(vy,new BoundedTypeVariable(tx,JoinBound,false))
        case EqualityConstraint(vx: SignatureVariable,vy: SignatureVariable) => (vx.universal,vy.universal) match {
          case (true, true) => if(vx != vy) throw new Exception("Cannot substitute from one universal variable to another: " + vx.toString + " =:= " + vy.toString)
          case (true,false) => substitute(vy,vx)
          case _ => substitute(vx,vy)
        }
        case EqualityConstraint(vx: SignatureVariable,_) => substitute(vx,c.beta)
        case EqualityConstraint(_,vy: SignatureVariable) => substitute(vy,c.alpha)
        /* DEBUG: If the inference engine seems to get stuck, I'm not properly handling a case that should occur down here in the inference
           section.  To handle it, I can comment this next line out, figure out how to handle it, and then uncomment this line. */
        case _ => constrain(c)
      }
    }
    result
  }
}
