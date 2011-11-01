package org.deca.compiler

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import org.jllvm.LLVMInstructionBuilder
import org.jllvm.LLVMArgument
import org.jllvm.LLVMValue

trait Scopeable {
  val name: String
  val scope: Scope
}

trait Definition extends Scopeable {
  override val scope: Module
}

trait VariableBinding extends Scopeable {
  var variableType: MonoType = BottomType
  val mutability: MonoMutability
  def compile(builder: LLVMInstructionBuilder,instantiation: Module): LLVMValue
  def load(builder: LLVMInstructionBuilder,instantiation: Module): LLVMValue
  def store(builder: LLVMInstructionBuilder,value: LLVMValue,instantiation: Module): LLVMValue
  def substitute(sub: SignatureSubstitution): Unit
  def specialize(spec: SignatureSubstitution): VariableBinding
}

abstract class Scope(val parent: Option[Scope]) {
  val symbols: Map[String,Scopeable] = new HashMap[String,Scopeable]()
  val region = ScopeRegion(this)

  def lookup(name: String): Scopeable = symbols.get(name) match {
    case Some(result) => result
    case None => parent match {
      case Some(p) => p.lookup(name)
      case None => throw new UndeclaredIdentifierException(name)
    }
  }
  
  def lookup(name: List[String]): Scopeable = name.tail match {
    case Nil => lookup(name.head)
    case _ => lookup(name.head) match {
      case scope: Scope => scope.lookup(name.tail)
      case _ => throw new Exception("Name of non-scope " + name.head + " used as scope identifier in qualified identifier.")
    }
  }
  
  def typedLookup[T <: Scopeable](name: String): T = {
    lookup(name).asInstanceOf[T]
  }
  
  def typedLookup[T <: Scopeable](name: List[String]): T = name.tail match {
    case Nil => typedLookup(name.head)
    case _ => lookup(name.head) match {
      case scope: Scope => scope.typedLookup(name.tail).asInstanceOf[T]
      case _ => throw new Exception("Name of non-scope or incorrectly-typed scope " + name.head + " used as scope identifier in qualified identifier.")
    }
  }
  
  protected def declare(obj: Scopeable) = {
    symbols.put(obj.name,obj)
  }
  
  def enclosedIn(s: Scope): Boolean = parent match {
    case Some(p) => p == s || p.enclosedIn(s)
    case None => false
  }
  
  override def toString: String = "Scope { " + symbols.toString + " }"
}

class UndeclaredIdentifierException(name: String) extends Exception("Undeclared identifier exception: " + name)
