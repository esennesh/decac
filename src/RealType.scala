package decac

import scala.Math
import jllvm.LLVMType
import jllvm.LLVMRealType
import jllvm.LLVMFloatType
import jllvm.LLVMDoubleType
import jllvm.LLVMFP128Type

abstract class RealGamma(n: String,p: Option[RealGamma]) extends NumericalGamma(n,p) {
  override val parent: Option[RealGamma] = p

  val compiledType: LLVMRealType
  
  override def signed: Boolean = true
  override def compile: LLVMRealType = compiledType
}

object FP128Gamma extends RealGamma("quadruple",None) {
  override val compiledType = new LLVMFP128Type
}

object DoubleGamma extends RealGamma("double",Some(FP128Gamma)) {
  override val compiledType = new LLVMDoubleType
}

object FloatGamma extends RealGamma("float",Some(DoubleGamma)) {
  override val compiledType = new LLVMFloatType
}
