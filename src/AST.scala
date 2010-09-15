package decac

import decasyntax.parser._
import decasyntax.lexer._
import decasyntax.node._
import java.util.LinkedList

object ASTProcessor {
  def convertList[T](list: LinkedList[T]): List[T] = {
    (list.toArray(new Array[T](3,4))).toList
  }
  def processQualifiedIdentifier(name: PQualifiedIdentifier): List[String] = name match {
    case simple: ASimpleQualifiedIdentifier => simple.getUnqualifiedIdentifier.getText() :: Nil
    case imported: AImportedQualifiedIdentifier => processQualifiedIdentifier(imported.getQualifiedIdentifier) ++ (imported.getUnqualifiedIdentifier.getText() :: Nil)
  }
  def declareImportDeclaration(where: Module,imp: PImportDeclaration): Option[Scopeable] = imp match {
    case imp: AImportDeclaration => where.define(where.lookup(processQualifiedIdentifier(imp.getName())))
  }
  def processDefinition(adef: PDefinition,scope: Module): Definition = adef match {
    case amoddef: AModuledefDefinition => {
      val moddef: AModuleDefinition = amoddef.getModuleDefinition() match {case real: AModuleDefinition => real}
      val result = new Module(scope,moddef.getName().getText())
      convertList(moddef.getImports()).map(imp => declareImportDeclaration(result,imp))
      convertList(moddef.getDefinitions).map(definition => processDefinition(definition,result))
      return result
    }
    case afuncdef: AFundefDefinition => {
      null
    }
    case atypedef: ATypedefDefinition => {
      null
    }
    case avardef: AGlobaldefDefinition => {
      null
    }
  }
}
