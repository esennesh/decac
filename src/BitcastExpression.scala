package decac

import jllvm.LLVMValue
import jllvm.LLVMInstructionBuilder
import jllvm.LLVMStackAllocation
import jllvm.LLVMLoadInstruction
import jllvm.LLVMStoreInstruction
import jllvm.LLVMConstantInteger
import jllvm.LLVMBitCast

class UninferredBitcast(expr: UninferredExpression,tau: TauType) extends UninferredExpression {
  override val expressionType: TauType = tau
  override val children: List[UninferredExpression] = (expr :: Nil)
  override val writable = false
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = rui
  override def substitute(substitution: TauSubstitution): Expression = {
    val pair = substituteTypes(substitution)
    new BitcastExpression(pair._2.apply(0),pair._1)
  }
}

class BitcastExpression(expr: Expression,sigma: TauType) extends Expression {
  override val expressionType: TauType = sigma
  override val children: List[Expression] = (expr :: Nil)
  override def specialize(specialization: BetaSpecialization): SpecializedExpression = {
    val child = children.apply(0).specialize(specialization)
    val gamma = specialization.solve(expressionType)
    new SpecializedBitcast(child,gamma)
  }
}

class SpecializedBitcast(expr: SpecializedExpression,gamma: GammaType) extends SpecializedExpression {
  override val expressionType: GammaType = gamma
  override val children: List[SpecializedExpression] = (expr :: Nil)
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = {
    val child = children.apply(0).compile(builder,scope)
    val pResult = new LLVMStackAllocation(builder,child.typeOf,LLVMConstantInteger.constantInteger(Nat.compile,1,false),"cast_alloc")
    new LLVMStoreInstruction(builder,child,pResult)
    val pCasted = new LLVMBitCast(builder,pResult,expressionType.compile,"pointer_cast")
    new LLVMLoadInstruction(builder,pCasted,"bit_casted")
  }
}
