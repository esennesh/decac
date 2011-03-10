package decac

class TypeBinding(t: TauType,n: String,s: TypeBindingScope) extends Scopeable {
  override val name: String = n
  override val scope: TypeBindingScope = { s.bind(this) ; s }
  val tau = t
}

class TypeBindingScope(p: Module) extends Scope[TypeBinding](p) {
  override val parent: Module = p
  def bind(b: TypeBinding) = declare(b)
  override def scopeType = parent.scopeType
}
