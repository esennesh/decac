package org.deca.compiler.expression

import org.jllvm._
import org.deca.compiler.definition._
import org.deca.compiler.signature._

class VariableExpression(val name: List[String],val scope: Scope) extends WritableExpression {
  val binding = scope.typedLookup[VariableBinding](name)
  expType = binding.variableType
  expEffect = EffectPair(ReadEffect(scope.region),PureEffect)
  override val writable: Boolean = true
  override val children: List[Expression] = Nil
  override def substitute(sub: SignatureSubstitution): Unit = {
    expType = sub.solve(expType).asInstanceOf[MonoType]
    expEffect = EffectPair(sub.solve(expEffect.positive).asInstanceOf[MonoEffect],sub.solve(expEffect.negative).asInstanceOf[MonoEffect])
  }
  override def specialize(spec: SignatureSubstitution): VariableExpression =
    //TODO: Figure out how to perform type-specialization on scope
    new VariableExpression(name,scope)
  override def constrain(scs: SignatureConstraints): Unit = Unit
  override def check(scs: SignatureConstraints): Unit = Unit

  override def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue =
    binding.load(builder,instantiation)
  override def store(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module,value: LLVMValue): LLVMValue =
    binding.store(builder,value,instantiation)
}
