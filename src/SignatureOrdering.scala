package decac;

import scala.collection.mutable.Lattice
import scala.collection.mutable.GraphLattice
import scala.collection.mutable.Stack
import scala.collection.Set

sealed abstract case class InferenceConstraint[T] {
  def substitute(vx: SignatureVariable[T],newY: T): Unit
  val x: T
  val y: Y
}
case class SubsumptionConstraint[T](var x: T,var y: T) extends InferenceConstraint[T] {
  override val x = x
  override val y = y
  override def substitute(vx: SignatureVariable[T],newY: T)(implicit signatures: MonoSignatures[T]): Unit = {
    x = signatures.replace(x,vx,newY)
    y = signatures.replace(y,vx,newY)
  }
  override def toString = x.toString + " <: " + y.toString
}
case class PhysicalSubtypingConstraint(var x: MonoType,var y: MonoType) extends InferenceConstraint[MonoType] {
  override val x = x
  override val y = y
  override def substitute(vx: TypeVariable,newY: MonoType): Unit = {
    x = x.replace(vx,newY)
    y = y.replace(vx,newY)
  }
  override def toString = x.toString + " <@< " + y.toString
}
case class EqualityConstraint[T](var x: T,var y: T) extends InferenceConstraint[T] {
  override def substitute(vx: SignatureVariable[T],newY: T): Unit = {
    x = signatures.replace(x,vx,newY)
    y = signatures.replace(y,vx,newY)
  }
  override def toString = x.toString + " =:= " + y.toString
}

trait InferenceOrdering[T] {
  def lt(x: T,y: T): Option[Set[InferenceConstraint[T]]]
  def equiv(x: T,y: T): Option[Set[InferenceConstraint[T]]]
}
