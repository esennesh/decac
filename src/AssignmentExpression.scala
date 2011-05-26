package decac

import jllvm.LLVMValue
import jllvm.LLVMInstructionBuilder

class UninferredAssignment(st: UninferredExpression,expr: UninferredExpression) extends UninferredExpression {
  val storage = st
  val expression = expr
  assert(storage.writable)
  override val expressionType: TauType = expression.expressionType
  override val children: List[UninferredExpression] = List(storage,expression)
  override val writable: Boolean = false
  override def substitute(substitution: TauSubstitution): AssignmentExpression = {
    val sides = substituteTypes(substitution)._2
    new AssignmentExpression(sides.apply(0),sides.apply(1))
  }
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    children.foreach(child => child.constrain(rui))
    rui.constrain(new LesserEq(expression.expressionType,storage.expressionType))
    rui
  }
}

class AssignmentExpression(st: Expression,expr: Expression) extends Expression {
  val storage = st
  val expression = expr
  override val expressionType = expression.expressionType
  override val children: List[Expression] = List(storage,expression)
  override def specialize(specialization: BetaSpecialization): SpecializedAssignment = {
    val left = storage.specialize(specialization).asInstanceOf[WritableExpression]
    val right = expression.specialize(specialization)
    new SpecializedAssignment(left,right)
  }
}

class SpecializedAssignment(st: WritableExpression,expr: SpecializedExpression) extends SpecializedExpression {
  val storage = st
  val expression = expr
  override val expressionType = expr.expressionType
  override val children: List[SpecializedExpression] = List(storage,expression)
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = {
    val value = expression.compile(builder,scope)
    storage.store(builder,scope,value)
    value
  }
}
