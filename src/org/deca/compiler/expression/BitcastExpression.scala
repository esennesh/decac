package org.deca.compiler.expression

import org.jllvm._
import org.deca.compiler.signature._
import org.deca.compiler.definition._

class BitcastExpression(val expression: Expression,val tau: MonoType) extends Expression {
  expType = tau
  override val children = List(expression)
  
  def constrain(lui: LatticeUnificationInstance): Unit = expression.constrain(lui)
  def check(lui: LatticeUnificationInstance): Unit = expression.check(lui)
  
  override def substitute(sub: SignatureSubstitution): Unit = {
    super.substitute(sub)
    expression.substitute(sub)
  }
  def specialize(spec: SignatureSubstitution,specScope: Scope): BitcastExpression =
    new BitcastExpression(expression.specialize(spec,specScope),spec.solve(expType))
  
  override def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue = {
    val exp = expression.compile(builder,scope,instantiation)
    val pResult = new LLVMStackAllocation(builder,exp.typeOf,LLVMConstantInteger.constantInteger(Nat.compile,1,false),"cast_alloc")
    new LLVMStoreInstruction(builder,exp,pResult)
    val pCasted = new LLVMBitCast(builder,pResult,expType.compile,"pointer_cast")
    new LLVMLoadInstruction(builder,pCasted,"bit_casted")
  }
}
