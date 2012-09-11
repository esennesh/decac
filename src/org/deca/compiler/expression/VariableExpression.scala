package org.deca.compiler.expression

import org.jllvm._
import org.deca.compiler.definition._
import org.deca.compiler.signature._

class VariableExpression(val name: List[String],val scope: Scope) extends WritableExpression {
  val binding = scope.typedLookup[VariableBinding](name)
  expType = binding.variableType
  expEffect = EffectPair(PureEffect,PureEffect)
  override val region: MonoRegion = scope.region
  override val children: List[Expression] = Nil
  override def substitute(sub: SignatureSubstitution): Unit = {
    expType = sub.solve[MonoType](expType)
    expEffect = EffectPair(sub.solve(expEffect.positive).asInstanceOf[MonoEffect],sub.solve(expEffect.negative).asInstanceOf[MonoEffect])
  }
  override def specialize(spec: SignatureSubstitution,specScope: Scope): VariableExpression =
    new VariableExpression(name,specScope)
  override def constrain(lui: LatticeUnificationInstance): Unit = Unit
  override def check(lui: LatticeUnificationInstance): Unit = Unit

  override def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue =
    binding.load(builder,instantiation)
  override def pointer(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue =
    binding.pointer(builder,instantiation)
}

class ImplicitResolutionExpression(val tau: MonoType,val scope: Scope) extends Expression {
  val binding: VariableBinding = scope.implicitLookup(tau)
  expType = tau
  expEffect = EffectPair(ReadEffect(scope.region),PureEffect)
  override val children: List[Expression] = Nil
  override def substitute(sub: SignatureSubstitution): Unit = {
    expType = sub.solve[MonoType](expType)
    expEffect = EffectPair(sub.solve[MonoEffect](expEffect.positive),sub.solve[MonoEffect](expEffect.negative))
  }
  override def specialize(spec: SignatureSubstitution,specScope: Scope): ImplicitResolutionExpression =
    new ImplicitResolutionExpression(spec.solve[MonoType](tau),specScope)
  override def constrain(lui: LatticeUnificationInstance): Unit = Unit
  override def check(lui: LatticeUnificationInstance): Unit = Unit
  
  override def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue =
    binding.load(builder,instantiation)
}
