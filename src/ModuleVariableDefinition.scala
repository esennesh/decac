package decac

class ModuleVariableDefinition(m: Module,n: String,rho: RhoType,value: Expression) extends Definition {
  override val name: String = n
  override val scope: Module = { m.declare(this) ; m}
  val initialValue: Expression = value
  val variableType: RhoType = rho
}
