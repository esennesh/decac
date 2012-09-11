package org.deca.compiler.expression

import org.jllvm.LLVMValue
import org.jllvm.LLVMBasicBlock
import org.jllvm.LLVMFunction
import org.jllvm.LLVMInstructionBuilder
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import org.deca.compiler.definition._
import org.deca.compiler.signature._

class BlockExpression(val steps: List[Expression]) extends Expression {
  assert(steps.length >= 1)
  expType = steps.last.expType
  expEffect = EffectPair(new EffectVariable(false),new EffectVariable(false))
  override val children = steps
  
  override def constrain(lui: LatticeUnificationInstance): Unit =
    for(step <- steps) {
      step.constrain(lui)
      lui.constrain(new SubsumptionConstraint(step.expEffect.positive,expEffect.positive))
      lui.constrain(new SubsumptionConstraint(step.expEffect.negative,expEffect.negative))
    }
  override def check(lui: LatticeUnificationInstance): Unit = assert(expEffect.safe(PureEffect))
  override def substitute(sub: SignatureSubstitution): Unit = {
    for(step <- steps)
      step.substitute(sub)
    expType = sub.solve(expType).asInstanceOf[MonoType]
    expEffect = EffectPair(sub.solve[MonoEffect](expEffect.positive),sub.solve[MonoEffect](expEffect.negative))
  }
  override def specialize(spec: SignatureSubstitution,specScope: Scope): BlockExpression =
    new BlockExpression(steps.map(_.specialize(spec,specScope)))
  override def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue = 
    steps.map(_.compile(builder,scope,instantiation)).last
    
  override def toString: String =
    "{\n" + steps.foldRight("")((step,result) => step.toString + ";\n" + result) + "}"
}
