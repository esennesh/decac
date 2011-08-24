package decac;

import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.collection.mutable.Lattice

class SignatureSubstitution {
  protected val queue: Queue[Tuple2[SignatureVariable,MonoSignature]] = new Queue[Tuple2[SignatureVariable,MonoSignature]]
  
  def substitute(x: SignatureVariable,y: MonoSignature): Unit = {
    assert(!x.universal)
    queue.enqueue((x,y))
  }
  def isEmpty: Boolean = queue.isEmpty
  
  def solve(sig: MonoSignature): MonoSignature = {
    val substituted = queue.foldLeft(sig)((result: MonoSignature,sub: Tuple2[SignatureVariable,MonoSignature]) => sub._1 match {
      case t: MonoType => sig.mapT((st: MonoType) => if(st == sub._1) sub._2.asInstanceOf[MonoType] else st)
      case e: MonoEffect => sig.mapE((se: MonoEffect) => if(se == sub._1) sub._2.asInstanceOf[MonoEffect] else se)
      case r: MonoRegion => sig.mapR((sr: MonoRegion) => if(sr == sub._1) sub._2.asInstanceOf[MonoRegion] else sr)
    })
    val typeBounded = substituted.mapT((sigprime: MonoType) => sigprime match {
      case bounded: BoundsVariable[MonoType] => {
        assert(!bounded.universal)
        bounded.signature
      }
      case _ => sigprime
    })
    //Build up effectBounded and regionBounded
    typeBounded
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

class LatticeUnificationInstance(subst: Option[SignatureSubstitution] = None)(implicit lat: Lattice[MonoSignature]) {
  protected val lattice = lat
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
        case PhysicalSubtypingConstraint(x,y) => TypeRelation.actuallt(x,y,true)
        case EqualityConstraint(x,y) => (x,y) match {
          case (tx: MonoType,ty: MonoType) => TypeRelation.equiv(tx,ty)
          case (ex: MonoEffect,ey: MonoEffect) => EffectRelation.equiv(ex,ey)
          case (rx: MonoRegion,ry: MonoRegion) => RegionRelation.equiv(rx,ry)
        }
      }
      for(c <- cs.getOrElse(throw new Exception("Unsatisfiable signature constraint: " + constraint.toString))) c match {
        case SubsumptionConstraint(vx: BoundsVariable[MonoSignature],vy: BoundsVariable[MonoSignature]) => {
          val px = vx.meet(lattice.meet(vy.signature,vx.signature))
          val py = vy.join(lattice.join(vy.signature,vx.signature))
          constrain(px._1)
          constrain(py._1)
          substitute(vx,px._2)
          substitute(vy,py._2)
        }
        case SubsumptionConstraint(vx: BoundsVariable[MonoSignature],_) => {
          val pair = vx.meet(lattice.meet(vx.signature,c.beta))
          constrain(pair._1)
          substitute(vx,pair._2)
        }
        case SubsumptionConstraint(_,vy: BoundsVariable[MonoSignature]) => {
          val pair = vy.join(lattice.join(c.alpha,vy.signature))
          constrain(pair._1)
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
