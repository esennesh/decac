package decac

import decasyntax.nodes._

object ASTProcessor {
  def processQualifiedIdentifier(name: PQualifiedIdentifier): List[String] = name match {
    case simple: ASimpleQualifiedIdentifier => simple.getUnqualifiedIdentifier.getText() :: Nil
    case imported: AImportedQualifiedIdentifier => processQualifiedIdentifier(imported.getQualifiedIdentifier).append(imported.getUnqualifiedIdentifier.getText() :: Nil)
  }
  def processDefinition(adef: PDefinition,scope: Scope): Definition = adef match {
    case amoddef: AModuledefDefinition => {
      val moddef: AModuleDefinition = amoddef.getModuleDefinition()
      val result = new Module(scope match {case modscope: Module => modscope case _ => null },moddef.getName().getText())
      moddef.getImports().map(imp: AImportDeclaration => result.declare(scope.lookup(processQualifiedIdentifier(imp.getName()))))
      moddef.getDefinitions.map(definition: PDefinition => processDefinition(definition,result)
      return result
    }
    case afuncdef: AFundefDefinition => {
    }
    case atypedef: ATypedefDefinition => {
    }
    case avardef: AGlobaldefDefinition => {
    }
  }
}
