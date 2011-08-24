package decac;

import scala.collection.mutable.Lattice
import scala.collection.mutable.GraphLattice
import scala.collection.mutable.Stack
import scala.collection.Set
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
  def lt(x: T,y: T): Option[Set[InferenceConstraint]]
  def equiv(x: T,y: T): Option[Set[InferenceConstraint]]
}

object SignatureOrdering extends PartialOrdering[MonoSignature] {
  override def lt(x: MonoSignature,y: MonoSignature): Boolean = (x,y) match {
    case (tx: MonoType,ty: MonoType) => TypeOrdering.lt(tx,ty)
    case (rx: MonoRegion,ry: MonoRegion) => RegionOrdering.lt(rx,ry)
    case (ex: MonoEffect,ey: MonoEffect) => EffectOrdering.lt(ex,ey)
    case _ => throw new Exception("Mismatched signatures: " + x.toString + " < " + y.toString)
  }
  override def equiv(x: MonoSignature,y: MonoSignature): Boolean = (x,y) match {
    case (tx: MonoType,ty: MonoType) => TypeOrdering.equiv(tx,ty)
    case (rx: MonoRegion,ry: MonoRegion) => RegionOrdering.equiv(rx,ry)
    case (ex: MonoEffect,ey: MonoEffect) => EffectOrdering.equiv(ex,ey)
    case _ => throw new Exception("Mismatched signatures: " + x.toString + " < " + y.toString)
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
