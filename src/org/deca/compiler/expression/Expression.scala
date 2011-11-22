package org.deca.compiler.expression

import org.jllvm._
import org.deca.compiler.signature._
import org.deca.compiler.definition._

case class EffectPair(positive: MonoEffect,negative: MonoEffect) {
  def safe(previous: MonoEffect): Boolean = !SetEffect(Set.empty + negative + previous).contains(positive) || positive == PureEffect
}

trait Expression {
  var expType: MonoType = BottomType
  var expEffect: EffectPair = EffectPair(PureEffect,PureEffect)
  val writable: Boolean = false
  val children: List[Expression]
  
  def constrain(scs: SignatureConstraints): Unit
  def check(scs: SignatureConstraints): Unit
  
  def substitute(sub: SignatureSubstitution): Unit
  def specialize(spec: SignatureSubstitution): Expression
  /* When scope enclosedIn instantiation, this expression is being built in the same module that defines it.
   * When !(scope enclosedIn instantiation), this expression was imported, and references to outside
   * variables and functions will need to be emitted into instantiation.compiledModule as externals. */
  def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue
}

trait WritableExpression extends Expression {
  def store(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module,value: LLVMValue): LLVMValue
}

trait ConstantExpression extends Expression {
  override def constrain(rui: SignatureConstraints): Unit = Unit
  override def substitute(substitution: SignatureSubstitution): Unit = Unit
  override def specialize(specialization: SignatureSubstitution): ConstantExpression = this
  override def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMConstant = build(scope,instantiation)
  def build(scope: Scope,instantiation: Module): LLVMConstant
}
