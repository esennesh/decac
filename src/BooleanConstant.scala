package decac

import jllvm.LLVMValue
import jllvm.LLVMInstructionBuilder
import jllvm.LLVMConstantBoolean

class UninferredBoolean(yes: Boolean) extends UninferredExpression(BuiltInSums.BooleanGamma) {
  val value = yes
  
  override def children: List[UninferredExpression] = Nil
  
  override def substitute(substitution: TauSubstitution): Expression = new BooleanConstant(value)
  def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    rui
  }
}

object UninferredTrue extends UninferredBoolean(true)
object UninferredFalse extends UninferredBoolean(false)

class BooleanConstant(yes: Boolean) extends Expression(BuiltInSums.BooleanGamma) {
  val value: Boolean = yes
  def children: List[Expression] = Nil
  def specialize(specialization: BetaSpecialization): SpecializedExpression = new SpecializedBoolean(value)
}

class SpecializedBoolean(yes: Boolean) extends SpecializedExpression(BuiltInSums.BooleanGamma) {
  val value: Boolean = yes
  def children: List[SpecializedExpression] = Nil
  def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = new LLVMConstantBoolean(value)
}
