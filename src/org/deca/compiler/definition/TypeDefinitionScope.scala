package org.deca.compiler.definition

import scala.collection.immutable.Set
import org.deca.compiler.signature._

class TypeBinding(override val name: String,
                  override val scope: TypeDefinitionScope,
                  t: Option[MonoType]) extends Scopeable {
  val tau = t getOrElse new TypeVariable(true,Some(name))
}

class TypeDefinitionScope(params: List[String],val owner: Module) extends Scope(Some(owner)) {
  protected var bindingSet: Set[TypeBinding] = Set.empty[TypeBinding]
  params.foreach(bind(_,None))
  def bind(name: String,tau: Option[MonoType]) = {
    val binding = new TypeBinding(name,this,tau)
    bindingSet = bindingSet + binding
    declare(binding)
  }
  def bindings: Set[TypeBinding] = bindingSet
}
