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
  override def scope: Module = parent
  
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
}

object GlobalScope extends Module(null,"")
