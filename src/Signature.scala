package decac

import jllvm._
import scala.collection.immutable.Set
import scala.collection.immutable.HashSet

trait MonoSignature {
  def map(f: (MonoType) => MonoType): MonoSignature
  def map(f: (MonoRegion) => MonoRegion): MonoSignature
  def map(f: (MonoEffect) => MonoEffect): MonoSignature
  def replace[T <: MonoSignature](from: T,to: T): MonoSignature
  def variables: Set[SignatureVariable[T]]
}

class SignatureVariable[T <: MonoSignature] extends T {
  val universal = false
  override def variables: Set[SignatureVariable[T]] = HashSet.empty.add(this)
}

sealed abstract case class SignatureBound
case class JoinBound extends SignatureBound
case class MeetBound extends SignatureBound

class BoundsVariable[T <: MonoSignature](tau: T,bnd: SignatureBound,univ: Boolean) extends SignatureVariable[T] {
  val signature: T = tau
  assert(!signature.isInstanceOf[SignatureVariable[T]])
  val bound: SignatureBound = bnd
  override val universal = univ
  assert(!universal || signature.variables.forall(svar => svar.universal))
  def join(above: T): Tuple2[InferenceConstraint[T],BoundsVariable[T]]
  def meet(below: T): Tuple2[InferenceConstraint[T],BoundsVariable[T]]
}

abstract case class MonoRegion extends MonoSignature {
  override def map(f: (MonoEffect) => MonoEffect): MonoRegion = this
  override def map(f: (MonoType) => MonoType): MonoRegion = this
  override def map(f: (MonoRegion) => MonoRegion): MonoRegion = f(this)
}

abstract class MonoEffect extends MonoSignature {
  override def map(f: (MonoEffect) => MonoEffect): MonoEffect = f(this)
  override def map(f: (MonoRegion) => MonoRegion): MonoEffect = this
  override def map(f: (MonoType) => MonoType): MonoEffect = this
}

abstract class MonoType extends MonoSignature {
  override def map(f: (MonoRegion) => MonoRegion): MonoType = this
  override def map(f: (MonoEffect) => MonoEffect): MonoType = this
  override def map(f: (MonoType) => MonoType): MonoType = f(this)
  def compile: LLVMType
  def tagged: Boolean
  def sizeOf: Int = {
    val target = new LLVMTargetData("i686-pc-linux-gnu")
    target.storeSizeOfType(compile).toInt
  }
}
