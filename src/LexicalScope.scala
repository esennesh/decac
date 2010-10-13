package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

class LexicalBinding(n: String,s: LexicalScope,t: TauType) extends Scopeable {
  override val name = n
  override val scope = { s.bind(this) ; s }
  protected var tau: TauType = t
  
  def variableType = tau
  def generalize(substitution: TauSubstitution): Unit = {
    tau = substitution.solve(tau)
  }
}

class LexicalScope(p: Scope[_]) extends Scope[LexicalBinding](p) {  
  def bind(binding: LexicalBinding) = declare(binding)
  def generalize(substitution: TauSubstitution): Unit = {
    symbols.values.foreach(binding => binding.generalize(substitution))
  }
  def specialize(substitution: BetaSpecialization): LexicalScope = {
    val result = new LexicalScope(parent)
    symbols.values.foreach(binding => new LexicalBinding(binding.name,result,substitution.solve(binding.variableType)))
    result
  }
}

object GlobalLexicalScope extends LexicalScope(GlobalScope)
