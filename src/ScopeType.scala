package decac

abstract class ScopeType {
  def equals(scope: ScopeType): Boolean = scope == this
  def subtypes(scope: ScopeType): Boolean
}

class GlobalScopeType(m: Option[Module]) extends ScopeType {
  val scope: Option[Module] = m
  
  override def subtypes(scope: ScopeType): Boolean = scope match {
    case mod: GlobalScopeType => (this.scope,mod.scope) match {
      case (Some(modX),Some(modY)) => modX == modY || modX.enclosed(modY)
      case _ => true
    }
    case _ => false
  }
  
  override def equals(scope: ScopeType): Boolean = scope match {
    case mod: GlobalScopeType => (this.scope,mod.scope) match {
      case (Some(modX),Some(modY)) => modX == modY
      case _ => true
    }
    case _ => false
  }
}

object RegionalScopeType extends GlobalScopeType(None)

/* Why do I even have different scope-types for lexical scopes and module scopes?  They should both just be considered the scope-type of a known Scope object.*/
class LexicalScopeType(s: AnyLexicalScope[_]) extends ScopeType {
  val scope: AnyLexicalScope[_] = s
  
  override def subtypes(scope: ScopeType): Boolean = scope match {
    case mod: GlobalScopeType => true
    case lexical: LexicalScopeType => this.scope == lexical.scope || this.scope.enclosed(lexical.scope)
  }
}

class ArgumentScopeType(func: ExpressionFunction,caller: Option[ScopeType]) extends LexicalScopeType(func.uninferred.scope) {
  val callerScope: Option[ScopeType] = caller
  val function: ExpressionFunction = func
  
  override def subtypes(scope: ScopeType): Boolean = scope match {
    case mod: GlobalScopeType => true
    case lexical: LexicalScopeType => lexical.scope == this.scope || (callerScope match {
      case Some(callerTau) => callerTau.subtypes(lexical)
      case None => true
    })
    case _ => false
  }
}
