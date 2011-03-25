package decac

import jllvm.LLVMConstantBoolean

class BooleanLiteralExpression(yes: Boolean) extends ConstantExpression {
  val value = yes
  override val expressionType = BuiltInSums.BooleanGamma
  override def children: List[ConstantExpression] = Nil
  override def build: LLVMConstantBoolean = new LLVMConstantBoolean(value)
}
