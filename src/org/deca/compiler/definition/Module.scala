package org.deca.compiler.definition

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.immutable.Set
import scala.util.Memoize1
import org.jllvm._
import org.jllvm.bindings._
import org.deca.compiler.signature._
import org.deca.compiler.expression.{Expression,ConstantExpression}

trait Definition extends Scopeable {
  override val scope: Module
  val build: Memoize1[Module,Set[LLVMValue]]
}

class VariableDefinition(override val scope: Module,override val name: String,val value: ConstantExpression,override var variableType: MonoType,override var mutability: MonoMutability) extends Definition with VariableBinding {
  assert(TypeOrdering.lteq(value.expType,variableType))
  scope.define(this)
  override def substitute(sub: SignatureSubstitution): Unit = Unit
  override def specialize(spec: SignatureSubstitution): VariableDefinition = this
  val declare: Memoize1[Module,LLVMValue] = Memoize1(instantiation => {
    val global = instantiation.compiledModule.addGlobal(variableType.compile,name)
    if(instantiation == scope)
      global.setInitializer(value.build(scope,instantiation))
    else
      global.setLinkage(LLVMLinkage.LLVMExternalLinkage)
    global.setThreadLocal(mutability == MutableMutability)
    global.setConstant(mutability != MutableMutability)
    global
  })
  override val build = Memoize1((instantiation: Module) => Set.empty + declare(instantiation))
  override def compile(builder: LLVMInstructionBuilder,instantiation: Module): LLVMValue =
    declare(instantiation)
  override def load(builder: LLVMInstructionBuilder,instantiation: Module): LLVMValue = 
    new LLVMLoadInstruction(builder,declare(instantiation),"load")
  override def pointer(builder: LLVMInstructionBuilder,instantiation: Module): LLVMValue = declare(instantiation)
}

class Module(val name: String,p: Module = GlobalScope) extends Scope(Some(p)) with Definition {
  override val parent: Option[Module] = Some(p)
  override val scope: Module = p
  override val build: Memoize1[Module,Set[LLVMValue]] = Memoize1((mod: Module) => Set.empty)
  val compiledModule: LLVMModule = new LLVMModule(name)
  protected var compiled = false
  
  protected var fPath: Option[String] = None
  def path: String = fPath.get
  def setPath(str: String) = {
    fPath = Some(str)
  }
  
  def define(d: Definition) = declare(d)
  override def lookup(name: String): Definition = super.lookup(name).asInstanceOf[Definition]
  override def lookup(name: List[String]): Definition = super.lookup(name).asInstanceOf[Definition]
  
  def compile: LLVMModule = {
    for(definition <- symbols.values) {
      definition match {
        case function: FunctionDefinition => function.build(this)
        //case defin: TypeDefinition => defin.getSpecializations.foreach(tau => compiledModule.addTypeName(name,tau.compile))
        case typeDefinition: TypeDefinition => Unit
        case global: VariableDefinition => global.build(this)
        //Modules defined in this namespace may not be child modules, but possibly imports.
        case module: Module => if(module.parent == Some(this)) module.compile
      }
    }
    compiledModule
  }
  
  def writeBitcode: Unit = compile.writeBitcodeToFile(path + name + ".bc")
}

object GlobalScope extends Module("") {
  override val parent: Option[Module] = None
  override val region = GlobalRegion
}

object StandardLibrary extends Module("std",GlobalScope)
