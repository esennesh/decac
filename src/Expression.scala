package decac

import jllvm.LLVMValue
import jllvm.LLVMBasicBlock
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder

abstract class UninferredExpression(tau: TauType,s: Scope) {
  val expressionType: TauType = tau
  val scope: Scope = s
  def children: List[UninferredExpression]
  protected def substituteTypes(substitution: TauSubstitution): Tuple2[TauType,List[Expression]] = {
    (substitution.solve(expressionType),children.map(child => child.substitute(substitution)))
  }
  def substitute(substitution: TauSubstitution): Expression
  def constrain(rui: RangeUnificationInstance): RangeUnificationInstance
}

abstract class Expression(exprType: TauType,s: Scope) {
  val scope: Scope = s
  val expressionType = exprType
  def children: List[Expression]
  def specialize(specialization: SigmaSubstitution): SpecializedExpression
}

abstract class SpecializedExpression(gamma: GammaType,s: Scope) {
  val expressionType: GammaType = gamma
  val scope: Scope = s
  def children: List[SpecializedExpression]
  def compile(builder: LLVMInstructionBuilder): LLVMValue
}
