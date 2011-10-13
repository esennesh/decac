package org.deca.compiler

import org.jllvm._
import scala.collection.immutable.Set
import scala.collection.immutable.HashSet

trait MonoSignature {
  def mapT(f: (MonoType) => MonoType): MonoSignature
  def mapR(f: (MonoRegion) => MonoRegion): MonoSignature
  def mapE(f: (MonoEffect) => MonoEffect): MonoSignature
  def filterT(pred: MonoType => Boolean): Set[MonoType]
  def filterR(pred: MonoRegion => Boolean): Set[MonoRegion]
  def filterE(pred: MonoEffect => Boolean): Set[MonoEffect]
  def replace(from: MonoSignature,to: MonoSignature): MonoSignature = from match {
    case t: MonoType => mapT((sig: MonoType) => if(sig == from) to.asInstanceOf[MonoType] else sig)
    case e: MonoEffect => mapE((sig: MonoEffect) => if(sig == from) to.asInstanceOf[MonoEffect] else sig)
    case r: MonoRegion => mapR((sig: MonoRegion) => if(sig == from) to.asInstanceOf[MonoRegion] else sig)
  }
  def variables: Set[SignatureVariable]
}

trait SignatureVariable extends MonoSignature {
  val universal = false
  override def variables: Set[SignatureVariable] = HashSet.empty[SignatureVariable] + this
}

sealed abstract class SignatureBound
case object JoinBound extends SignatureBound
case object MeetBound extends SignatureBound

trait BoundsVariable[T <: MonoSignature] extends SignatureVariable {
  val signature: T
  assert(!signature.isInstanceOf[SignatureVariable])
  val bound: SignatureBound
  assert(!universal || signature.variables.forall(svar => svar.universal))
  def join(above: T): Tuple2[InferenceConstraint,BoundsVariable[T]]
  def meet(below: T): Tuple2[InferenceConstraint,BoundsVariable[T]]
}

abstract class MonoRegion extends MonoSignature {
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = {
    if(pred(this))
      HashSet.empty[MonoRegion] + this
    else
      HashSet.empty[MonoRegion]
  }
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = HashSet.empty
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = HashSet.empty
  override def mapE(f: (MonoEffect) => MonoEffect): MonoRegion = this
  override def mapT(f: (MonoType) => MonoType): MonoRegion = this
  override def mapR(f: (MonoRegion) => MonoRegion): MonoRegion = f(this)
  
  def <(sig: MonoRegion)(implicit ordering: PartialOrdering[MonoRegion]) = ordering.lt(this,sig)
  def >(sig: MonoRegion)(implicit ordering: PartialOrdering[MonoRegion]) = ordering.gt(this,sig)
  def <=(sig: MonoRegion)(implicit ordering: PartialOrdering[MonoRegion]) = ordering.lteq(this,sig)
  def >=(sig: MonoRegion)(implicit ordering: PartialOrdering[MonoRegion]) = ordering.gteq(this,sig)
}

abstract class MonoEffect extends MonoSignature {
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = {
    if(pred(this))
      HashSet.empty[MonoEffect] + this
    else
      HashSet.empty[MonoEffect]
  }
  override def mapE(f: (MonoEffect) => MonoEffect): MonoEffect = f(this)
  override def mapR(f: (MonoRegion) => MonoRegion): MonoEffect = this
  override def mapT(f: (MonoType) => MonoType): MonoEffect = this
  
  def <(sig: MonoEffect)(implicit ordering: PartialOrdering[MonoEffect]) = ordering.lt(this,sig)
  def >(sig: MonoEffect)(implicit ordering: PartialOrdering[MonoEffect]) = ordering.gt(this,sig)
  def <=(sig: MonoEffect)(implicit ordering: PartialOrdering[MonoEffect]) = ordering.lteq(this,sig)
  def >=(sig: MonoEffect)(implicit ordering: PartialOrdering[MonoEffect]) = ordering.gteq(this,sig)
}

abstract class MonoType extends MonoSignature {
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = {
    if(pred(this))
      HashSet.empty[MonoType] + this
    else
      HashSet.empty[MonoType]
  }
  override def mapR(f: (MonoRegion) => MonoRegion): MonoType = this
  override def mapE(f: (MonoEffect) => MonoEffect): MonoType = this
  override def mapT(f: (MonoType) => MonoType): MonoType = f(this)
  def compile: LLVMType
  def sizeOf: Int = {
    val target = new LLVMTargetData("i686-pc-linux-gnu")
    target.storeSizeOfType(compile).toInt
  }
  
  def <(sig: MonoType)(implicit ordering: PartialOrdering[MonoType]) = ordering.lt(this,sig)
  def >(sig: MonoType)(implicit ordering: PartialOrdering[MonoType]) = ordering.gt(this,sig)
  def <=(sig: MonoType)(implicit ordering: PartialOrdering[MonoType]) = ordering.lteq(this,sig)
  def >=(sig: MonoType)(implicit ordering: PartialOrdering[MonoType]) = ordering.gteq(this,sig)
}
