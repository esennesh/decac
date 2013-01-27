package org.deca.compiler.expression

import org.jllvm._
import org.deca.compiler.signature._
import org.deca.compiler.definition._

case class EffectPair(positive: MonoEffect,negative: MonoEffect) {
  def safe(previous: MonoEffect): Boolean =
    positive == PureEffect || !SetEffect(Set.empty + negative + previous).contains(positive)
  def ++(pair: EffectPair) = EffectPair(positive ++ pair.positive,negative ++ pair.negative)
  def map(f: MonoEffect => MonoEffect): EffectPair = EffectPair(f(positive),f(negative))
}

trait Expression {
  var expType: MonoType = BottomType
  val writable: Boolean = false
  val children: List[Expression]
  var expEffect: EffectPair = EffectPair(PureEffect,PureEffect)
  
  def constrain(lui: LatticeUnificationInstance): Unit
  def check(lui: LatticeUnificationInstance): Unit
  
  def substitute(sub: SignatureSubstitution): Unit = {
    expType = sub.solve(expType)
    expEffect = EffectPair(sub.solve(expEffect.positive),sub.solve(expEffect.negative))
  }
  def specialize(spec: SignatureSubstitution,specScope: Scope): Expression
  /* When scope enclosedIn instantiation, this expression is being built in the same module that defines it.
   * When !(scope enclosedIn instantiation), this expression was imported, and references to outside
   * variables and functions will need to be emitted into instantiation.compiledModule as externals. */
  def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue
  
  def dump: Unit = {
    for(child <- children)
      child.dump
    System.err.println(toString + ": " + expType.toString + " !: " + expEffect.toString)
  }
}

trait WritableExpression extends Expression {
  override val writable: Boolean = true
  def region: MonoRegion
  def specialize(spec: SignatureSubstitution,specScope: Scope): WritableExpression
  def pointer(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue
}

trait ConstantExpression extends Expression {
  override val writable: Boolean = false
  override def constrain(lui: LatticeUnificationInstance): Unit = Unit
  override def check(lui: LatticeUnificationInstance): Unit = Unit
  override def substitute(substitution: SignatureSubstitution): Unit = Unit
  override def specialize(spec: SignatureSubstitution,specScope: Scope): ConstantExpression = this
  override def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMConstant = build(scope,instantiation)
  def build(scope: Scope,instantiation: Module): LLVMConstant
}
