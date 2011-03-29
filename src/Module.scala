package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import jllvm.LLVMModule
import jllvm.LLVMInstructionBuilder

abstract trait Definition extends Scopeable {
  override def scope: Module
}

class Module(m: Module,n: String) extends Scope[Definition](m) with Definition {
  override val parent: Module = m
  val name = n
  val compiledModule: LLVMModule = new LLVMModule(name)
  var compiled = false
  override def scope: Module = parent
  protected var fPath: Option[String] = None
  def path: String = fPath.get
  def setPath(str: String) = {
    fPath = Some(str)
  }
  
  override def lookup(name: String): Definition = symbols.get(name) match {
    case Some(result) => result
    case None => if(parent != null) parent.lookup(name) else throw new UndeclaredIdentifierException(name)
  }
  
  override def lookup(name: List[String]): Definition = name.tail match {
    case Nil => lookup(name.head)
    case _ => lookup(name.head) match {
      case mod: Module => mod.lookup(name.tail)
      case _ => throw new Exception("Name of non-module " + name.head + " used as module identifier in qualified identifier.")
    }
  }
  
  def define(obj: Definition) = declare(obj)
  
  def compile: LLVMModule = {
    if(!compiled) {
      for(definition <- symbols.values) definition match {
        case function: FunctionDefinition => {
          val builder = new LLVMInstructionBuilder
          function.specialized.foreach(func => func.compile(builder))
        }
        case defin: TypeDefinition => defin.getSpecializations.foreach(gamma => compiledModule.addTypeName(name,gamma.compile))
        case global: ModuleVariableDefinition => global.build 
          //TODO: Add code for constant expressions, and use it to set the initializer on global variables.
        //TODO: add case for modules, and change Module so that it can distinguish between imported modules and inner modules.
      }
      compiled = true
    }
    compiledModule
  }
  
  override def scopeType: ScopeType = new GlobalScopeType(Some(this))
}

object GlobalScope extends Module(null,"")
