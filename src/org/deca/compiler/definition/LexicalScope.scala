package org.deca.compiler.definition

import org.jllvm._
import org.jllvm.bindings._
import org.deca.compiler.signature._
import org.deca.compiler.expression._

abstract class LexicalBinding(override val name: String,
                              override val scope: LexicalScope,
                              tau: MonoType,
                              mut: MonoMutability) extends VariableBinding {
  override var variableType: MonoType = tau
  override var mutability: MonoMutability = mut
  protected var compiled: Option[LLVMValue] = None
  override def substitute(sub: SignatureSubstitution): Unit = {
    variableType = sub.solve(variableType)
    mutability = sub.solve(mutability)
  }
  def specialize(spec: SignatureSubstitution): LexicalBinding
  override def compile(builder: LLVMInstructionBuilder,instantiation: Module): LLVMValue = compiled match {
    case Some(alloc) => alloc
    case None => {
      val result = if(mutability == MutableMutability) {
        val alloc = new LLVMStackAllocation(builder,variableType.compile,null,name)
        new LLVMStoreInstruction(builder,initialize(builder,instantiation),alloc)
        alloc
      }
      else
        initialize(builder,instantiation)
      compiled = Some(result)
      result
    }
  }
  def initialize(builder: LLVMInstructionBuilder,instantiation: Module): LLVMValue
  override def load(builder: LLVMInstructionBuilder,instantiation: Module): LLVMValue = {
    if(mutability == MutableMutability)
      new LLVMLoadInstruction(builder,compile(builder,instantiation),"load" + name)
    else
      compile(builder,instantiation)
  }
  def store(builder: LLVMInstructionBuilder,value: LLVMValue,instantiation: Module): LLVMValue = {
    assert(mutability == MutableMutability)
    new LLVMStoreInstruction(builder,value,compile(builder,instantiation))
  }
}

class ArgumentBinding(name: String,scope: LexicalScope,tau: MonoType,mut: MonoMutability) extends LexicalBinding(name,scope,tau,mut) {
  protected var arg: Option[LLVMArgument] = None
  def argument = arg.get
  def setArgument(a: LLVMArgument) = {
    assert(arg == None)
    arg = Some(a)
  }
  override def initialize(builder: LLVMInstructionBuilder,instantiation: Module): LLVMValue = argument
  override def specialize(spec: SignatureSubstitution): ArgumentBinding = {
    assert(arg == None)
    new ArgumentBinding(name,scope,spec.solve(variableType),spec.solve(mutability))
  }
}

class LetBinding(name: String,scope: LexicalScope,var initializer: Expression,mut: MonoMutability) extends LexicalBinding(name,scope,initializer.expType,mut) {
  override def substitute(sub: SignatureSubstitution): Unit = {
    initializer.substitute(sub)
    super.substitute(sub)
  }
  override def initialize(builder: LLVMInstructionBuilder,instantiation: Module): LLVMValue =
    initializer.compile(builder,scope.parent.get,instantiation)
  override def specialize(spec: SignatureSubstitution): LetBinding = 
    new LetBinding(name,scope,initializer.specialize(spec),mutability)
}
