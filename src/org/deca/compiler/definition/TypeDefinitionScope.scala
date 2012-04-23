package org.deca.compiler.definition

import org.deca.compiler.signature._

class TypeBinding(override val name: String,
                  override val scope: TypeDefinitionScope,
                  t: Option[MonoType]) extends Scopeable {
  val tau = t getOrElse new TypeVariable(true,Some(name))
}

class TypeDefinitionScope(params: List[String],val owner: Module) extends Scope(Some(owner)) {
  def bind(name: String,tau: Option[MonoType]) = declare(new TypeBinding(name,this,tau))
  params.foreach(bind(_,None))
}
