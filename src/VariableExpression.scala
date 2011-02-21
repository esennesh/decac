package decac

import jllvm.LLVMValue
import jllvm.LLVMBasicBlock
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder
import jllvm.LLVMLoadInstruction

class UninferredVariable(n: List[String],scope: Scope[VariableBinding]) extends UninferredExpression(scope.typedLookup(n).variableType) {
  override def children: List[UninferredExpression] = Nil
  val name = n
  override def substitute(substitution: TauSubstitution): Expression = {
    new VariableExpression(name,substitution.solve(expressionType))
  }
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = rui
}

class VariableExpression(n: List[String],t: TauType) extends Expression(t) {
  override def children: List[Expression] = Nil
  val name = n
  override def specialize(specialization: BetaSpecialization): SpecializedExpression = {
    new SpecializedVariable(name,specialization.solve(expressionType))
  }
}

class SpecializedVariable(n: List[String],g: GammaType) extends SpecializedExpression(g) {
  val name = n
  protected var scope: Option[Scope[SpecializedVariableBinding]] = None
  override def children: List[SpecializedExpression] = Nil
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = {
    val binding = scope.lookup(name) match {
      case variable: SpecializedVariableBinding => variable
      case _ => throw new Exception("Name '" + name.head + name.tail.map(str => "." + str) + "' used in variable expression not bound to a variable.")
    }
    binding.load(builder)
  }
}
