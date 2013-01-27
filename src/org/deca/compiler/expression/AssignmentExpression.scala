package org.deca.compiler.expression

import org.jllvm.LLVMValue
import org.jllvm.LLVMInstructionBuilder
import org.jllvm.LLVMStoreInstruction
import org.deca.compiler.definition._
import org.deca.compiler.signature._

class AssignmentExpression(val slot: WritableExpression,val value: Expression) extends Expression {
  expType = slot.expType
  expEffect = value.expEffect ++ EffectPair(WriteEffect(slot.region),PureEffect)
  
  override val children: List[Expression] = List(slot,value)
  override def constrain(lui: LatticeUnificationInstance): Unit = {
    slot.constrain(lui)
    value.constrain(lui)
    lui.constrain(new SubsumptionConstraint(value.expType,slot.expType))
  }
  override def check(lui: LatticeUnificationInstance): Unit = {
    slot.check(lui)
    value.check(lui)
  }
  
  override def substitute(sub: SignatureSubstitution): Unit = {
    super.substitute(sub)
    slot.substitute(sub)
    value.substitute(sub)
  }
  def specialize(spec: SignatureSubstitution,specScope: Scope): Expression = 
    new AssignmentExpression(slot.specialize(spec,specScope),value.specialize(spec,specScope))
  /* When scope enclosedIn instantiation, this expression is being built in the same module that defines it.
   * When !(scope enclosedIn instantiation), this expression was imported, and references to outside
   * variables and functions will need to be emitted into instantiation.compiledModule as externals. */
  def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue =
    new LLVMStoreInstruction(builder,value.compile(builder,scope,instantiation),slot.pointer(builder,scope,instantiation))
}
