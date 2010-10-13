package decac

import decasyntax.parser._
import decasyntax.lexer._
import decasyntax.node._
import java.util.LinkedList
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

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
  
  def processTypeParameters(parameters: PIdentifierList): Map[String,TauVariable] = parameters match {
    case one: AOneIdentifierList => {
      val result = new HashMap[String,TauVariable]()
      result.put(one.getUnqualifiedIdentifier.getText,new TauVariable)
      result
    }
    case many: AManyIdentifierList => {
      val result = processTypeParameters(many.getIdentifierList)
      result.put(many.getUnqualifiedIdentifier.getText,new TauVariable)
      result
    }
  }
  def processTypeAnnotation(annotation: PTypeForm,typeParams: Map[String,TauVariable],scope: Module): TauType = annotation match {
    case function: AFunctionTypeForm => function.getFunctionTypeForm match {
      case one: AOneFunctionTypeForm => {
        val node = new AOthersTypeForm
        node.setLowerTypeForm(one.getArgument)
        val argument = processTypeAnnotation(node,typeParams,scope)
        val range = processTypeAnnotation(one.getResult,typeParams,scope)
        new FunctionArrow(argument :: Nil,range)
      }
    }
    /*case alias: AAliasTypeForm
    case aclass: AClassTypeForm
    case subrange: ASubrangeTypeForm
    case enum: AEnumTypeForm
    case exception: AExceptionTypeForm*/
    case lower: AOthersTypeForm => lower.getLowerTypeForm match {
      case named: ANamedLowerTypeForm => {
        val name = processQualifiedIdentifier(named.getTypename)
        if(name.length > 1)
          scope.lookup(name) match {
            case TypeDefinition(gamma,_,_) => gamma
            case _ => throw new Exception("Used an identifier in a type annotation that referred to a non-type definition.")
          }
        else
          typeParams.get(name.apply(0)) match {
            case Some(tvar) => tvar
            case None => scope.lookup(name) match {
              case TypeDefinition(gamma,_,_) => gamma
              case _ => throw new Exception("Used an identifier in a type annotation that referred to a non-type definition.")
            }
          }
      }
    }
  }
  
  def processArgument(arg: AArgument,typeParams: Map[String,TauVariable],scope: Module): Tuple2[String,TauType] = {
    val name = arg.getName.getText
    val argType = arg.getType match {
      case null => new TauVariable
      case annotation: ATypeAnnotation => processTypeAnnotation(annotation.getType,typeParams,scope)
    }
    (name,argType)
  }
  def processArguments(args: PArgumentList,typeParams: Map[String,TauVariable],scope: Module): List[Tuple2[String,TauType]] = args match {
    case one: AOneArgumentList => processArgument(one.getArgument match {case real: AArgument => real},typeParams,scope) :: Nil
    case many: AManyArgumentList => processArgument(many.getArgument match {case real: AArgument => real},typeParams,scope) :: processArguments(many.getArgumentList,typeParams,scope)
  }
  
  def processBlock(expression: PBlockExpression,scope: LexicalScope): UninferredBlock
  
  def processDefinition(adef: PDefinition,scope: Module): Definition = adef match {
    case amoddef: AModuledefDefinition => {
      val moddef: AModuleDefinition = amoddef.getModuleDefinition() match {case real: AModuleDefinition => real}
      val result = new Module(scope,moddef.getName().getText())
      convertList(moddef.getImports()).map(imp => declareImportDeclaration(result,imp))
      convertList(moddef.getDefinitions).map(definition => processDefinition(definition,result))
      return result
    }
    case afuncdef: AFundefDefinition => {
      afuncdef.getFunctionDefinition() match {
        case normal: AFunctionFunctionDefinition => {
          val name = normal.getName.getText
          val typeParameters = processTypeParameters(normal.getParameters match { case params: ATypeFormArguments => params.getArguments })
          val arguments = processArguments(normal.getArguments,typeParameters,scope)
          val uninferred = new UninferredFunction(scope,name,arguments,lexical => processBlock(normal.getBody,lexical))
          uninferred.infer
        }
        case constructor: AConstructorFunctionDefinition => null
        case destructor: ADestructorFunctionDefinition => null
      }
    }
    case atypedef: ATypedefDefinition => {
      null
    }
    case avardef: AGlobaldefDefinition => {
      null
    }
  }
}
