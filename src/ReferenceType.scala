package decac;

import jllvm.LLVMType
import jllvm.LLVMPointerType

class ReferenceGamma(gamma: GammaType,optional: Boolean) extends PrimitiveGamma {
  val target: GammaType = gamma
  val nullable: Boolean = optional
  
  override def subtypes(tau: TauType,possibly: Boolean): Boolean = tau match {
    case ref: ReferenceGamma => target.subtypes(ref.target,possibly)
    case range: GammaRange => subtypes(range.lowerBound,possibly)
    case tvar: TauVariable => possibly
    case _ => false
  }
  
  override def compile: LLVMType = new LLVMPointerType(target.compile,0)
}
