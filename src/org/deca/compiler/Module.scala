package org.deca.compiler

import scala.util.Memoize1
import org.jllvm._
import org.jllvm.bindings._

class VariableDefinition(override val scope: Module,override val name: String,val value: ConstantExpression,override val mutability: MonoMutability) extends Definition with VariableBinding {
  {
    variableType = value.expType
  }
  override def substitute(sub: SignatureSubstitution): Unit = Unit
  override def specialize(spec: SignatureSubstitution): VariableDefinition = this
  val build: (Module => LLVMGlobalVariable) = Memoize1(instantiation => {
    val global = instantiation.compiledModule.addGlobal(variableType.compile,name)
    if(instantiation == scope)
      global.setInitializer(value.build(scope,instantiation))
    else
      global.setLinkage(LLVMLinkage.LLVMExternalLinkage)
    global.setThreadLocal(mutability == MutableMutability)
    global.setConstant(mutability != MutableMutability)
    global
  })
  override def compile(builder: LLVMInstructionBuilder,instantiation: Module): LLVMValue = build(instantiation)
  override def load(builder: LLVMInstructionBuilder,instantiation: Module): LLVMValue = 
    new LLVMLoadInstruction(builder,build(instantiation),"load")
  override def store(builder: LLVMInstructionBuilder,value: LLVMValue,instantiation: Module): LLVMValue =
    new LLVMStoreInstruction(builder,value,build(instantiation))
}

class Module(val name: String,p: Module = GlobalScope) extends Scope(Some(p)) {
  override val parent: Option[Module] = Some(p)
  val compiledModule: LLVMModule = new LLVMModule(name)
  protected var compiled = false
  
  protected var fPath: Option[String] = None
  def path: String = fPath.get
  def setPath(str: String) = {
    fPath = Some(str)
  }
  
  def define(d: Definition) = declare(d)
  override def lookup(name: String): Definition = typedLookup[Definition](name)
  override def lookup(name: List[String]): Definition = typedLookup[Definition](name)
  
  def compile: LLVMModule = {
    if(!compiled) {
      for(definition <- symbols.values) definition match {
        /* case function: FunctionDefinition => function.specialized.foreach(func => func.compile)
        case defin: TypeDefinition => defin.getSpecializations.foreach(tau => compiledModule.addTypeName(name,tau.compile)) */
        case global: VariableDefinition => global.build 
        //TODO: Add code for constant expressions, and use it to set the initializer on global variables.
        //Modules defined in this namespace may not be child modules, but possibly imports.
        case module: Module => if(module.parent == Some(this)) module.compile
      }
      compiled = true
    }
    compiledModule
  }
  
  def writeBitcode: Unit = (new LLVMBitWriter(compile)).writeBitcodeToFile(path + name + ".bc")
}

class LexicalScope(parent: Scope) extends Scope(Some(parent))
object GlobalScope extends Module("") {
  override val parent: Option[Module] = None
  override val region = GlobalRegion
}
