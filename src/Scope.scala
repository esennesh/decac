package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

trait Scopeable {
  def name(): String
  def scope(): Scope[_]
}

abstract class Scope[T <: Scopeable](p: Scope[_]) {
  val symbols: Map[String,T] = new HashMap[String,T]()
  
  def lookup(name: String): Scopeable = {
    val result = symbols.get(name)
    if(result.isDefined)
      result.get
    else
      if(parent != null) parent.lookup(name) else throw new UndeclaredIdentifierException(name)
  }
  
  def lookup(name: List[String]): Scopeable = name.tail match {
    case Nil => lookup(name.head)
    case _ => lookup(name.head) match {
      case mod: Module => mod.lookup(name.tail)
      case _ => throw new Exception("Name of non-module " + name.head + " used as module identifier in qualified identifier.")
    }
  }
  
  protected def declare(obj: T) = {
    symbols.put(obj.name,obj)
  }
  
  val parent: Scope[_] = p
  
  def enclosed(s: Scope[_]): Boolean = parent == s || parent.enclosed(s)
}
