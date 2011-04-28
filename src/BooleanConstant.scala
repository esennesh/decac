package decac

import jllvm.LLVMConstantBoolean

class BooleanLiteralExpression(yes: Boolean) extends ConstantExpression {
  val value = yes
  override val expressionType = BuiltInSums.BooleanGamma
  override val children: List[ConstantExpression] = Nil
  override val writable = false
  override def build: LLVMConstantBoolean = new LLVMConstantBoolean(value)
}
