package org.deca.compiler.expression

import org.jllvm.LLVMConstantBoolean
import org.deca.compiler.definition._
import org.deca.compiler.signature._

class BooleanLiteralExpression(val value: Boolean) extends ConstantExpression {
  expType = BuiltInSums.BooleanSum.represent(Nil)
  override val children: List[ConstantExpression] = Nil
  override val writable = false
  override def build(scope: Scope,instantiation: Module) = new LLVMConstantBoolean(value)
  override def check(lui: LatticeUnificationInstance): Unit = Unit
}
