package decac

abstract trait Definition extends Scopeable {
  override def scope: Module
}

case class Module(m: Module,n: String) extends Scope(m) with Definition {
  override val parent: Module = m
  val name = n
  override def scope: Module = parent
  override def declare(obj: Scopeable) = obj match {
    case obj: Definition => super.declare(obj)
    case _ => throw new Exception("Cannot declare anything other than a definition in a module scope.")
  }
}

object GlobalScope extends Module(null,"")
