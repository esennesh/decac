package decac;

import jllvm.LLVMType
import jllvm.LLVMPointerType

class ReferenceGamma(gamma: GammaType,optional: Boolean) extends PrimitiveGamma {
  val target: GammaType = gamma
  val nullable: Boolean = optional
  
  override def subtypes(tau: TauType): Boolean = tau match {
    case ref: ReferenceGamma => target.subtypes(ref.target)
    case range: GammaRange => subtypes(range.lowerBound)
    case bvar: BetaVariable => true
    case tvar: TauVariable => false
    case _ => false
  }
  
  override def compile: LLVMType = new LLVMPointerType(target.compile,0)
}
