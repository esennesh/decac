package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

trait Scopeable {
  def name(): String
  def scope(): Scope
}

class Scope(p: Scope) {
  protected var symbols: Map[String,Scopeable] = new HashMap[String,Scopeable]()
  
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
  
  def declare(obj: Scopeable) = {
    symbols.put(obj.name,obj)
  }
  
  val parent: Scope = p
  
  def encloses(s: Scope): Boolean = {
    var next = s
    while(next != null) {
      if(next == this)
        return true
      next = next.parent
    }
    return false
  }
}
