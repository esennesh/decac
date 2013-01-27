package org.deca.compiler.signature

import scala.collection.mutable.{Map,HashMap,Queue,Stack,Lattice}
import org.deca.compiler.definition._

class SignatureSubstitution {
  protected val substitutions: Map[SignatureVariable,MonoSignature] = new HashMap[SignatureVariable,MonoSignature]
  
  def substitute(x: SignatureVariable,y: MonoSignature,specialize: Boolean = false): Unit = {
    assert(specialize == x.universal)
    
    if(x != y) substitutions.put(x,y) match {
      case Some(sig) => throw new Exception("Cannot substitute " + x.toString + " |--> " + y.toString + " when it already maps to " + sig.toString)
      case None =>
        for(substitution <- substitutions)
          substitutions.update(substitution._1,substitution._2.replace(x,y))
    }
  }
  
  def isEmpty: Boolean = substitutions.isEmpty
  
  def solve[T <: MonoSignature](sig: T): T = {
    sig.variables.foldLeft[T](sig)((prime: T,svar: SignatureVariable) => if(svar.universal) prime else {
      substitutions.get(svar) match {
        case None => {
          val solution = svar match {
            case tvar: TypeVariable => new TypeVariable(true,tvar.name)
            case evar: EffectVariable => new EffectVariable(true)
            case rvar: RegionVariable => new RegionVariable(true)
            case _ => throw new Exception("Cannot generalize signature variable: " + svar.toString)
          }
          prime.replace(svar,solution).asInstanceOf[T]
        }
        case Some(solved) => {
          val solution = solved match {
            case bounded: BoundsVariable[_] => {
              assert(!bounded.universal)
              bounded.signature
            }
            case _ => solved
          }
          prime.replace(svar,solution).asInstanceOf[T]
        }
      }
    })
  }
}

class SignatureConstraints extends Iterable[InferenceConstraint] {
  protected val current = new Stack[InferenceConstraint]
  protected val polyHypotheses = new Queue[InferenceConstraint]
  
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
  
  override def isEmpty = current.isEmpty && polyHypotheses.isEmpty
  
  override def iterator: Iterator[InferenceConstraint] = current.iterator ++ polyHypotheses.iterator
}

class LatticeUnificationInstance(protected val result: SignatureSubstitution = new SignatureSubstitution) {
  protected val constraints = new SignatureConstraints
  
  def constrain(c: InferenceConstraint): Unit = constraints.push(c)
  
  def substitute(x: SignatureVariable,y: MonoSignature): Unit = {
    result.substitute(x,y)
    constraints.substitute(x,y)
  }
  
  def solve: SignatureSubstitution = {
    for(c <- constraints)
      if(c.alpha.isInstanceOf[MonoType])
        System.err.println("Original constraint: " + c.toString)
    while(!constraints.isEmpty) {
      val constraint = constraints.pop
      val cs: Option[Set[InferenceConstraint]] = constraint match {
        case SubsumptionConstraint(x,y) => (x,y) match {
          case (tx: MonoType,ty: MonoType) => TypeRelation.lteq(tx,ty)
          case (ex: MonoEffect,ey: MonoEffect) => EffectRelation.lteq(ex,ey)
          case (rx: MonoRegion,ry: MonoRegion) => RegionRelation.lteq(rx,ry)
          case (mx: MonoMutability,my: MonoMutability) => MutabilityRelation.lteq(mx,my)
        }
        case PhysicalSubtypingConstraint(x,y) => PhysicalTypeRelation.lteq(x,y)
        case EqualityConstraint(x,y) => (x,y) match {
          case (tx: MonoType,ty: MonoType) => TypeRelation.equiv(tx,ty)
          case (ex: MonoEffect,ey: MonoEffect) => EffectRelation.equiv(ex,ey)
          case (rx: MonoRegion,ry: MonoRegion) => RegionRelation.equiv(rx,ry)
          case (mx: MonoMutability,my: MonoMutability) => MutabilityRelation.equiv(mx,my)
        }
      }
      for(c <- cs.getOrElse(throw new Exception("Unsatisfiable signature constraint: " + constraint.toString))) {
        if(c.alpha.isInstanceOf[MonoType] && c.beta.isInstanceOf[MonoType])
          System.err.println(c.toString)
        c match {
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
        case PhysicalSubtypingConstraint(vx: TypeVariable,ty: MonoType) => substitute(vx,new BoundedTypeVariable(ty,MeetBound,vx.universal,vx.name))
        case PhysicalSubtypingConstraint(tx: MonoType,vy: TypeVariable) => substitute(vy,new BoundedTypeVariable(tx,JoinBound,vy.universal,vy.name))
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
          substitute(vy,vx.clone(vx.signature,vx.bound,vx.universal,vy.name))
        case SubsumptionConstraint(vx: BoundsVariable[MonoSignature],_) => {
          val meets = SignatureRelation.meet(vx.signature,c.beta)
          val pair = vx.meet(meets._1)
          (meets._2 + pair._1).map(c => constrain(c))
          substitute(vx,pair._2)
        }
        case SubsumptionConstraint(vx: SignatureVariable,vy: BoundsVariable[MonoSignature]) =>
          substitute(vx,vy.clone(vy.signature,vy.bound,vy.universal,vx.name))
        case SubsumptionConstraint(_,vy: BoundsVariable[MonoSignature]) => {
          val joins = SignatureRelation.join(c.alpha,vy.signature)
          val pair = vy.join(joins._1)
          (joins._2 + pair._1).map(c => constrain(c))
          substitute(vy,pair._2)
        }
        case SubsumptionConstraint(vx: SignatureVariable,vy: SignatureVariable) => constrain(c)
        case SubsumptionConstraint(ex: MonoEffect,vy: EffectVariable) => substitute(vy,new BoundedEffectVariable(ex,JoinBound,vy.universal))
        case SubsumptionConstraint(vx: EffectVariable,ey: MonoEffect) => substitute(vx,new BoundedEffectVariable(ey,MeetBound,vx.universal))
        case SubsumptionConstraint(vx: TypeVariable,ty: MonoType) => substitute(vx,new BoundedTypeVariable(ty,MeetBound,vx.universal,vx.name))
        case SubsumptionConstraint(tx: MonoType,vy: TypeVariable) => substitute(vy,new BoundedTypeVariable(tx,JoinBound,vy.universal,vy.name))
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
      } }
    }
    result
  }
}
