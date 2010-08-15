package decac

import jllvm.LLVMValue
import jllvm.LLVMBasicBlock
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder

abstract class UninferredExpression(tau: TauType,s: Scope) {
  val expressionType: TauType = tau
  val scope: Scope = s
  def children: List[UninferredExpression]
  protected def substituteTypes(substitution: SigmaSubstitution,generalize: Boolean): Tuple2[SigmaType,List[Expression]] = {
    val exprType = if(generalize) substitution.generalize(substitution.solve(expressionType)) else substitution.solve(expressionType)
    (exprType,children.map(child => child.substitute(substitution,false)))
  }
  def substitute(substitution: SigmaSubstitution,generalize: Boolean): Expression
  def constrain(rui: RangeUnificationInstance): RangeUnificationInstance
}

abstract class Expression(sigma: SigmaType,s: Scope) {
  val expressionType: SigmaType = sigma
  val scope: Scope = s
  def children: List[Expression]
  def specialize(specialization: List[RhoType]): SpecializedExpression
}

abstract class SpecializedExpression(rho: RhoType,s: Scope) {
  val expressionType: RhoType = rho
  val scope: Scope = s
  def children: List[SpecializedExpression]
  def compile(builder: LLVMInstructionBuilder): LLVMValue
}
