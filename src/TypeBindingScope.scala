package decac

class TypeBinding(t: TauType,n: String,s: TypeBindingScope) extends Scopeable {
  override val name: String = n
  override val scope: TypeBindingScope = { s.bind(this) ; s }
  val tau = t
}

class TypeBindingScope(p: Module) extends Scope[TypeBinding](Some(p)) {
  override val parent: Option[Module] = Some(p)
  val owner: Module = p
  def bind(b: TypeBinding) = declare(b)
  override def scopeType = owner.scopeType
}
