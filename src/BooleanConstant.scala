package decac

import jllvm.LLVMValue
import jllvm.LLVMInstructionBuilder
import jllvm.LLVMConstantBoolean

class BooleanLiteralExpression(yes: Boolean) extends ConstantExpression {
  val value = yes
  override val expressionType = BuiltInSums.BooleanGamma
  override def children: List[ConstantExpression] = Nil
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = new LLVMConstantBoolean(value)
}
