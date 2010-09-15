package decac

import jllvm.LLVMValue
import jllvm.LLVMBasicBlock
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder

abstract class UninferredExpression(tau: TauType) {
  val expressionType: TauType = tau
  def children: List[UninferredExpression]
  protected def substituteTypes(substitution: TauSubstitution): Tuple2[TauType,List[Expression]] = {
    (substitution.solve(expressionType),children.map(child => child.substitute(substitution)))
  }
  def substitute(substitution: TauSubstitution): Expression
  def constrain(rui: RangeUnificationInstance): RangeUnificationInstance
}

abstract class Expression(exprType: TauType) {
  val expressionType = exprType
  def children: List[Expression]
  def specialize(specialization: BetaSpecialization): SpecializedExpression
}

abstract class SpecializedExpression(gamma: GammaType) {
  val expressionType: GammaType = gamma
  def children: List[SpecializedExpression]
  def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue
}
