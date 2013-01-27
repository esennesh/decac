package org.deca.compiler.signature

import org.jllvm.{LLVMType,LLVMPointerType}
import org.deca.compiler.definition._

class PointerType(val target: MonoType,val region: MonoRegion,val mutability: MonoMutability) extends MonoType {
  override def compile: LLVMType = new LLVMPointerType(target.compile,0)
  override def toString: String = region.toString + "|-" + target.toString + "*"
  override def variables: Set[SignatureVariable] = target.variables ++ region.variables
  override def mapR(f: (MonoRegion) => MonoRegion): PointerType =
    new PointerType(target.mapR(f),region.mapR(f),mutability.mapR(f))
  override def mapE(f: (MonoEffect) => MonoEffect): PointerType =
    new PointerType(target.mapE(f),region.mapE(f),mutability.mapE(f))
  override def mapT(f: (MonoType) => MonoType): MonoType = f(new PointerType(target.mapT(f),region.mapT(f),mutability.mapT(f)))
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = {
    val subs = target.filterT(pred) ++ region.filterT(pred)
    if(pred(this))
      subs + this
    else
      subs
  }
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] =
    target.filterE(pred) ++ region.filterE(pred)
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] =
    target.filterR(pred) ++ region.filterR(pred)
}
