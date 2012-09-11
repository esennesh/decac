package org.deca.compiler.signature

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
  def replace[T <: MonoSignature](from: T,to: T): MonoSignature = from match {
    case t: MonoType => mapT((sig: MonoType) => if(sig == from) to.asInstanceOf[MonoType] else sig)
    case e: MonoEffect => mapE((sig: MonoEffect) => if(sig == from) to.asInstanceOf[MonoEffect] else sig)
    case r: MonoRegion => mapR((sig: MonoRegion) => if(sig == from) to.asInstanceOf[MonoRegion] else sig)
  }
  def variables: Set[SignatureVariable]
}

trait SignatureVariable extends MonoSignature {
  val universal = false
  val name: Option[String] = None
  override def variables: Set[SignatureVariable] = Set.empty[SignatureVariable] + this
}

sealed abstract class SignatureBound
case object JoinBound extends SignatureBound
case object MeetBound extends SignatureBound

abstract class BoundsVariable[T <: MonoSignature](val signature: T,val bound: SignatureBound,override val universal: Boolean) extends SignatureVariable {
  assert(!signature.isInstanceOf[SignatureVariable])
  assert(!universal || signature.variables.forall(svar => svar.universal))
  
  def clone(sig: T,bnd: SignatureBound,univ: Boolean,nm: Option[String] = None): BoundsVariable[T]
  override def toString: String = {
    val suffix = "(" + super.toString + "," + signature.toString + "," + universal.toString + ")"
    bound match {
      case JoinBound => "Join" + suffix
      case MeetBound => "Meet" + suffix
    }
  }
  
  def join(above: T): (InferenceConstraint,BoundsVariable[T]) = {
    val constraint = bound match {
      case JoinBound => SubsumptionConstraint(signature,above)
      case MeetBound => SubsumptionConstraint(above,signature)
    }
    (constraint,clone(above,JoinBound,universal))
  }
  def meet(below: T): (InferenceConstraint,BoundsVariable[T]) = bound match {
    case JoinBound => (SubsumptionConstraint(signature,below),this)
    case MeetBound => (SubsumptionConstraint(below,signature),clone(below,MeetBound,universal))
  }
}

trait MonoRegion extends MonoSignature {
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

trait MonoEffect extends MonoSignature {
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
  
  def ++(eff: MonoEffect): SetEffect = (this,eff) match {
    case (SetEffect(ex),SetEffect(ey)) => SetEffect(ex ++ ey)
    case (_,SetEffect(effects)) => SetEffect(effects + this)
    case (SetEffect(effects),_) => SetEffect(effects + eff)
    case (_,_) => SetEffect(Set.empty[MonoEffect] + this + eff)
  }
}

trait MonoType extends MonoSignature {
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = {
    if(pred(this))
      Set.empty[MonoType] + this
    else
      Set.empty[MonoType]
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

trait MonoMutability extends MonoSignature {
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = HashSet.empty
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = HashSet.empty
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = HashSet.empty
  override def mapE(f: (MonoEffect) => MonoEffect): MonoMutability = this
  override def mapT(f: (MonoType) => MonoType): MonoMutability = this
  override def mapR(f: (MonoRegion) => MonoRegion): MonoMutability = this
  
  def <(sig: MonoMutability)(implicit ordering: PartialOrdering[MonoMutability]) = ordering.lt(this,sig)
  def >(sig: MonoMutability)(implicit ordering: PartialOrdering[MonoMutability]) = ordering.gt(this,sig)
  def <=(sig: MonoMutability)(implicit ordering: PartialOrdering[MonoMutability]) = ordering.lteq(this,sig)
  def >=(sig: MonoMutability)(implicit ordering: PartialOrdering[MonoMutability]) = ordering.gteq(this,sig)
}
