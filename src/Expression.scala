package decac

import jllvm.LLVMValue
import jllvm.LLVMBasicBlock
import jllvm.LLVMFunction

abstract class Expression(tau: TauType) {
  protected var exprType: SigmaType = tau
  def children(): List[Expression]
  def expressionType: SigmaType = exprType
  def originalType: TauType = tau
  def compile(specialization: List[RhoType],block: Option[LLVMBasicBlock]): Option[LLVMValue]
  def substituteTypes(substitution: TauSubstitution,generalize: Boolean): SigmaType = {
    if(generalize)
      exprType = substitution.generalize(exprType)
    children.map(child => child.substituteTypes(substitution,false))
    expressionType
  }
}
