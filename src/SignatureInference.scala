package decac;

import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

class SignatureSubstitution[T] {
  protected val queue: Queue[Tuple2[SignatureVariable[T],T]] = new Queue[Tuple2[SignatureVariable[T],T]]
  
  def substitute(x: SignatureVariable[T],y: T): Unit = {
    assert(!x.universal)
    queue.enqueue((x,y))
  }
  def isEmpty: Boolean = queue.isEmpty
  
  def solve(sig: T): T = {
    val substituted = queue.foldLeft(sig)((result: T,sub: Tuple2[SignatureVariable,T]) => sig.replace(sub._1,sub._2))
    substituted.map(sigprime => sigprime match {
      case bounded: BoundsVariable[T] => {
        assert(!bounded.universal)
        bounded.signature
      }
      case _ => sigprime
    })
  }
}

class SignatureConstraints[T] {
  val current = new Stack[InferenceConstraint[T]]
  val polyHypotheses = new Queue[InferenceConstraint[T]]
  
  def substitute(x: SignatureVariable[T],y: T): Unit = {
    for(constraint <- current)
      constraint.substitute(x,y)
    for(hypoth <- polyHypotheses) {
      hypoth.substitute(x,y)
      (hypoth.x,hypoth.y) match {
        case (vx: SignatureVariable[T],vy: SignatureVariable[T]) => if(vx.isInstanceOf[BoundsVariable] || vy.isInstanceOf[BoundsVariable]) { current.push(hypoth) polyHypotheses.remove(hypoth) }
        case _ => {
          current.push(hypoth)
          polyHypotheses.remove(hypoth)
        }
      }
    }
  }
  
  def push(c: InferenceConstraint[T]): Unit = (c.x,c.y) match {
    case (vx: SignatureVariable[T],vy: SignatureVariable[T]) => {
      if(!vx.isInstanceOf[BoundsVariable[T]] && !vy.isInstanceOf[BoundsVariable[T]])
        polyHypotheses.enqueue(c)
      else
        current.push(c)
    }
    case _ => current.push(c)
  }
  
  def pop: InferenceConstraint[T] = {
    if(current.isEmpty)
      polyHypotheses.deque
    else
      current.pop
  }
  
  def isEmpty = current.isEmpty && polyHypotheses.isEmpty
}

class LatticeUnificationInstance[T](subst: Option[SignatureSubstitution[T]] = None)(implicit lat: Lattice[T]) {
  protected lattice = lat
  protected val constraints = new SignatureConstraints[T]
  protected val result = subst match {
    case Some(s) => s
    case None => new SignatureSubstitution[T]
  }
  
  def constrain(c: InferenceConstraint[T]): Unit = constraints.push(c)
  
  def substitute(x: SignatureVariable[T],y: T): Unit = {
    result.substitute(x,y)
    constraints.substitute(x,y)
  }
  
  def solve: SignatureSubstitution[T] = {
    while(!constraints.isEmpty) {
      val constraint = constraints.pop
      val cs = constraint match {
        case SubsumptionConstraint(x,y) => SignatureOrdering.lt(x,y)
        case PhysicalSubtypingConstraint(x,y) => TypeRelation.lt(x,y,true)
        case EqualityConstraint(x,y) => SignatureOrdering.equiv(x,y)
      }
      for(c <- cs.getOrElse(() => throw new SignatureException("Unsatisfiable signature constraint: " + constraint.toString)) c match {
        case SubsumptionConstraint(vx: SignatureVariable,vy: SignatureVariable) => constrain(c)
        case SubsumptionConstraint(vx: BoundsVariable,vy: BoundsVariable) => {
          val px = vx.meet(lattice.meet(vy.signature,vx.signature))
          val py = vy.join(lattice.join(vy.signature,vx.signature))
          constrain(px._1)
          constrain(py._1)
          substitute(vx,px._2)
          substitute(vy,py._2)
        }
        case SubsumptionConstraint(vx: BoundsVariable,_) => {
          val pair = vx.meet(lattice.meet(vx.signature,c.y))
          constrain(pair._1)
          substitute(vx,pair._2)
        }
        case SubsumptionConstraint(_,vy: BoundsVariable) => {
          val pair = vy.join(lattice.join(c.x,vy.signature))
          constrain(pair._1)
          substitute(vy,pair._2)
        }
        case SubsumptionConstraint(vx: TypeVariable,ty: MonoType) => substitute(vx,new BoundsType(ty,MeetBound,false))
        case SubsumptionConstraint(tx: MonoType,vy: TypeVariable) => substitute(vy,new BoundsType(tx,JoinBound,false))
        case EqualityConstraint(vx: SignatureVariable,vy: SignatureVariable) => substitute(vx,vy)
        case EqualityConstraint(vx: SignatureVariable,_) => substitute(vx,c.y)
        case EqualityConstraint(_,vy: SignatureVariable) => substitute(c.x,vy)
      }
    }
    result
  }
}
