package org.deca.compiler.definition

import org.jllvm._
import org.jllvm.bindings._
import org.deca.compiler.signature._
import org.deca.compiler.expression._

import scala.collection.mutable.{Map,HashMap}
import scala.collection.immutable
import scala.util.Memoize2

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
  protected val build = Memoize2((builder: LLVMInstructionBuilder,instantiation: Module) => {
    if(mutability == MutableMutability) {
      val alloc = new LLVMStackAllocation(builder,variableType.compile,null,name)
      new LLVMStoreInstruction(builder,initialize(builder,instantiation),alloc)
      alloc
    }
    else
      initialize(builder,instantiation)
  })
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
  override def pointer(builder: LLVMInstructionBuilder,instantiation: Module): LLVMValue = {
    assert(mutability == MutableMutability)
    compile(builder,instantiation)
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

class LetBinding(name: String,scope: LexicalScope,tau: Option[MonoType],var initializer: Expression,mut: MonoMutability) extends LexicalBinding(name,scope,tau getOrElse initializer.expType,mut) {
  override def substitute(sub: SignatureSubstitution): Unit = {
    initializer.substitute(sub)
    super.substitute(sub)
  }
  override def initialize(builder: LLVMInstructionBuilder,instantiation: Module): LLVMValue =
    initializer.compile(builder,scope.parent.get,instantiation)
  override def specialize(spec: SignatureSubstitution): LetBinding = 
    new LetBinding(name,scope,Some(variableType),initializer.specialize(spec,scope),mutability)
}

class LexicalScope(par: Scope,arguments: Iterable[(String,MonoType)]) extends Scope(Some(par)) {
  val bindings: Map[String,LexicalBinding] = new HashMap[String,LexicalBinding]
  for(arg <- arguments) {
    val binding = new ArgumentBinding(arg._1,this,arg._2,ImmutableMutability)
    bindings.put(arg._1,binding)
    declare(binding)
  }
  def let(name: String,mu: MonoMutability,tau: Option[MonoType],initializer: Expression): LetBinding = {
    val binding = new LetBinding(name,this,tau,initializer,mu)
    bindings.put(name,binding)
    declare(binding)
    binding
  }
  def setArguments(args: immutable.Map[String,LLVMArgument]): Unit =
    for(binding <- bindings) binding match {
      case (name: String,arg: ArgumentBinding) => {
        assert(name == arg.name)
        arg.setArgument(args(name))
      }
      case _ => Unit
    }
  def substitute(sub: SignatureSubstitution): Unit =
    for(binding <- bindings.values)
      binding.substitute(sub)
  def specialize(spec: SignatureSubstitution): LexicalScope = {
    val result = new LexicalScope(parent.get,arguments.map(arg => (arg._1,spec.solve(arg._2))))
    for(binding <- bindings) binding match {
      case letBinding: LetBinding => result.let(letBinding.name,
                                                spec.solve(letBinding.mutability),
                                                Some(spec.solve(letBinding.variableType)),
                                                letBinding.initializer.specialize(spec,parent.get))
    }
    result
  }
  def compile(builder: LLVMInstructionBuilder,instantiation: Module): List[LLVMValue] =
    bindings.values.map(_.compile(builder,instantiation)).toList
}
