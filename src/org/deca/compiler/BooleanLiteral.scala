package org.deca.compiler

import org.jllvm.LLVMConstantBoolean

class BooleanLiteralExpression(val value: Boolean) extends ConstantExpression {
  expType = BuiltInSums.BooleanSum.represent(Nil)
  override val children: List[ConstantExpression] = Nil
  override val writable = false
  override def build(scope: Scope,instantiation: Module) = new LLVMConstantBoolean(value)
  override def check(scs: SignatureConstraints): Unit = Unit
}
