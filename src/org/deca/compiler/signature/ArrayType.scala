package org.deca.compiler.signature

import org.jllvm.{LLVMType,LLVMArrayType}
import org.deca.compiler.definition._

class ArrayType(val target: MonoType,val mutability: MonoMutability,val length: Option[Int]) extends MonoType {
  val dynamic: Boolean = length == None
  override def compile: LLVMType = new LLVMArrayType(target.compile,0)
  override def toString: String = {
    target.toString + "[" + (length match {
      case Some(i) => i.toString
      case None => ""
    }) + "]"
  }
  override def variables: Set[SignatureVariable] = target.variables
  override def mapR(f: (MonoRegion) => MonoRegion): ArrayType =
    new ArrayType(target.mapR(f),mutability.mapR(f),length)
  override def mapE(f: (MonoEffect) => MonoEffect): ArrayType =
    new ArrayType(target.mapE(f),mutability.mapE(f),length)
  override def mapT(f: (MonoType) => MonoType): MonoType = f(new ArrayType(target.mapT(f),mutability.mapT(f),length))
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = {
    val subs = target.filterT(pred)
    if(pred(this))
      subs + this
    else
      subs
  }
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = target.filterE(pred)
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = target.filterR(pred)
}
