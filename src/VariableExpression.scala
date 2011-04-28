package decac

import jllvm.LLVMValue
import jllvm.LLVMBasicBlock
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder
import jllvm.LLVMLoadInstruction

class UninferredVariable(n: List[String],scope: Scope[VariableBinding]) extends UninferredExpression {
  override val expressionType: TauType = scope.typedLookup(n).variableType
  override val children: List[UninferredExpression] = Nil
  override val writable = true
  val name = n
  override def substitute(substitution: TauSubstitution): Expression = {
    new VariableExpression(name,substitution.solve(expressionType))
  }
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = rui
}

class VariableExpression(n: List[String],t: TauType) extends Expression {
  override val expressionType: TauType = t
  override val children: List[Expression] = Nil
  val name = n
  override def specialize(specialization: BetaSpecialization): SpecializedExpression = {
    new SpecializedVariable(name,specialization.solve(expressionType))
  }
}

class SpecializedVariable(n: List[String],g: GammaType) extends WritableExpression {
  val name = n
  override val expressionType: GammaType = g
  override val children: List[SpecializedExpression] = Nil
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = {
    val binding = scope.lookup(name) match {
      case variable: SpecializedVariableBinding => variable
      case _ => throw new Exception("Name '" + name.head + name.tail.map(str => "." + str) + "' used in variable expression not bound to a variable.")
    }
    binding.load(builder)
  }
  override def store(builder: LLVMInstructionBuilder,scope: Scope[_],value: LLVMValue): LLVMValue = {
    val binding = scope.lookup(name) match {
      case variable: SpecializedVariableBinding => variable
      case _ => throw new Exception("Name '" + name.head + name.tail.map(str => "." + str) + "' used in variable expression not bound to a variable.")
    }
    binding.store(builder,value)
  }
}
