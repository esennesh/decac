package org.deca.compiler.signature

import scala.collection.mutable.Lattice
import scala.collection.mutable.Stack
import scala.collection.immutable.Set
import scala.math.PartialOrdering

sealed abstract class InferenceConstraint {
  def substitute(vx: SignatureVariable,newY: MonoSignature): Unit
  def alpha: MonoSignature
  def beta: MonoSignature
}
case class SubsumptionConstraint(var x: MonoSignature,var y: MonoSignature) extends InferenceConstraint {
  override def alpha = x
  override def beta = y
  override def substitute(vx: SignatureVariable,newY: MonoSignature): Unit = {
    x = x.replace(vx,newY)
    y = y.replace(vx,newY)
  }
  override def toString = x.toString + " <: " + y.toString
}
case class PhysicalSubtypingConstraint(var x: MonoType,var y: MonoType) extends InferenceConstraint {
  override def alpha = x
  override def beta = y
  override def substitute(vx: SignatureVariable,newY: MonoSignature): Unit = {
    x = x.replace(vx,newY).asInstanceOf[MonoType]
    y = y.replace(vx,newY).asInstanceOf[MonoType]
  }
  override def toString = x.toString + " <@< " + y.toString
}
case class EqualityConstraint(var x: MonoSignature,var y: MonoSignature) extends InferenceConstraint {
  override def alpha = x
  override def beta = y
  override def substitute(vx: SignatureVariable,newY: MonoSignature): Unit = {
    x = x.replace(vx,newY)
    y = y.replace(vx,newY)
  }
  override def toString = x.toString + " =:= " + y.toString
}

trait InferenceOrdering[T <: MonoSignature] {
  protected val lattice: Lattice[T]
  def lt(x: T,y: T): Option[Set[InferenceConstraint]]
  def equiv(x: T,y: T): Option[Set[InferenceConstraint]]
  def join(x: T,y: T): (T,Set[InferenceConstraint]) = {
    val tau = lattice.join(x,y)
    (tau,lt(x,tau).get ++ lt(y,tau).get)
  }
  def meet(x: T,y: T): (T,Set[InferenceConstraint]) = {
    val tau = lattice.meet(x,y)
    (tau,lt(tau,x).get ++ lt(tau,y).get)
  }
}

object SignatureRelation extends InferenceOrdering[MonoSignature] {
  override protected val lattice = null
  override def lt(x: MonoSignature,y: MonoSignature): Option[Set[InferenceConstraint]] = (x,y) match {
    case (tx: MonoType,ty: MonoType) => TypeRelation.lt(tx,ty)
    case (rx: MonoRegion,ry: MonoRegion) => RegionRelation.lt(rx,ry)
    case (ex: MonoEffect,ey: MonoEffect) => EffectRelation.lt(ex,ey)
    case (mx: MonoMutability,my: MonoMutability) => MutabilityRelation.lt(mx,my)
    case _ => throw new Exception("Mismatched signatures: " + x.toString + " <: " + y.toString)
  }
  override def equiv(x: MonoSignature,y: MonoSignature): Option[Set[InferenceConstraint]] = (x,y) match {
    case (tx: MonoType,ty: MonoType) => TypeRelation.equiv(tx,ty)
    case (rx: MonoRegion,ry: MonoRegion) => RegionRelation.equiv(rx,ry)
    case (ex: MonoEffect,ey: MonoEffect) => EffectRelation.equiv(ex,ey)
    case (mx: MonoMutability,my: MonoMutability) => MutabilityRelation.equiv(mx,my)
    case _ => throw new Exception("Mismatched signatures: " + x.toString + " = " + y.toString)
  }
}

object SignatureOrdering extends PartialOrdering[MonoSignature] {
  override def lt(x: MonoSignature,y: MonoSignature): Boolean = SignatureRelation.lt(x,y) match {
    case Some(constraints) => constraints.isEmpty
    case None => false
  }
  override def equiv(x: MonoSignature,y: MonoSignature): Boolean = SignatureRelation.equiv(x,y) match {
    case Some(constraints) => constraints.isEmpty
    case None => false
  }
  override def gt(x: MonoSignature,y: MonoSignature): Boolean = lt(y,x)
  override def lteq(x: MonoSignature,y: MonoSignature): Boolean = equiv(x,y) || lt(x,y)
  override def gteq(x: MonoSignature,y: MonoSignature): Boolean = equiv(x,y) || gt(x,y)
  override def tryCompare(x: MonoSignature,y: MonoSignature): Option[Int] = {
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
