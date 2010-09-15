package decac

trait Binding extends Scopeable {
  val symbolType: SigmaType
  def substitute(substitution: TauSubstitution): Unit
}

class ModuleVariableDefinition(m: Module,n: String,value: SpecializedExpression) extends Definition {
  override val name: String = n
  override val scope: Module = { m.define(this) ; m}
  val initialValue: SpecializedExpression = value
  val variableType: GammaType = value.expressionType
}
