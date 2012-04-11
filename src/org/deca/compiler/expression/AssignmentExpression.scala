package org.deca.compiler.expression

import org.jllvm.LLVMValue
import org.jllvm.LLVMInstructionBuilder
import org.deca.compiler.definition._
import org.deca.compiler.signature._

class AssignmentExpression(val slot: WritableExpression,val value: Expression) extends Expression {
  expType = slot.expType
  expEffect = value.expEffect
  
  override val children: List[Expression] = List(slot,value)
  override def constrain(scs: SignatureConstraints): Unit = {
    slot.constrain(scs)
    value.constrain(scs)
    scs.push(new SubsumptionConstraint(value.expType,slot.expType))
  }
  def check(scs: SignatureConstraints): Unit = {
    slot.constrain(scs)
    value.constrain(scs)
  }
  
  def substitute(sub: SignatureSubstitution): Unit = {
    slot.substitute(sub)
    value.substitute(sub)
  }
  def specialize(spec: SignatureSubstitution,specScope: Scope): Expression = 
    new AssignmentExpression(slot.specialize(spec,specScope),value.specialize(spec,specScope))
  /* When scope enclosedIn instantiation, this expression is being built in the same module that defines it.
   * When !(scope enclosedIn instantiation), this expression was imported, and references to outside
   * variables and functions will need to be emitted into instantiation.compiledModule as externals. */
  def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue =
    slot.store(builder,scope,instantiation,value.compile(builder,scope,instantiation))
}
