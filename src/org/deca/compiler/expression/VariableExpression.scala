package org.deca.compiler.expression

import org.jllvm._
import org.deca.compiler.definition._
import org.deca.compiler.signature._

class VariableExpression(val name: List[String], val scope: Scope) extends WritableExpression {
  val binding = scope.typedLookup[VariableBinding](name)
  expType = binding.variableType
  expEffect = EffectPair(PureEffect,PureEffect)
  override val region: MonoRegion = binding.scope.region
  override val mutability: MonoMutability = binding.mutability
  override val children: List[Expression] = Nil
  override def substitute(sub: SignatureSubstitution): Unit = {
    expType = sub.solve[MonoType](expType)
    expEffect = EffectPair(sub.solve(expEffect.positive), sub.solve(expEffect.negative))
  }
  override def specialize(spec: SignatureSubstitution,specScope: Scope): VariableExpression =
    new VariableExpression(name,specScope)
  override def constrain(lui: LatticeUnificationInstance): Unit = Unit
  override def check(lui: LatticeUnificationInstance): Unit = Unit

  override def pointer(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue =
    binding.pointer(builder,instantiation)
}

class ImplicitResolutionExpression(val tau: MonoType) extends Expression {
  expType = tau
  expEffect = EffectPair(PureEffect, PureEffect)
  override val children: List[Expression] = Nil
  override def substitute(sub: SignatureSubstitution): Unit = {
    expType = sub.solve[MonoType](expType)
    expEffect = EffectPair(sub.solve[MonoEffect](expEffect.positive),sub.solve[MonoEffect](expEffect.negative))
  }
  override def specialize(spec: SignatureSubstitution, specScope: Scope): ImplicitResolutionExpression =
    new ImplicitResolutionExpression(spec.solve[MonoType](tau))
  override def constrain(lui: LatticeUnificationInstance): Unit = Unit
  override def check(lui: LatticeUnificationInstance): Unit = Unit
  
  override def compile(builder: LLVMInstructionBuilder, scope: Scope, instantiation: Module): LLVMValue = {
    val binding = scope.implicitLookup(tau)
    new LLVMLoadInstruction(builder, binding.pointer(builder, instantiation), "implicit_load")
  }
}
