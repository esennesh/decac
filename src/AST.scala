package decac

import decasyntax.parser._
import decasyntax.lexer._
import decasyntax.node._
import java.util.LinkedList
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

object ASTProcessor {
  def convertList[T](list: java.util.List[T]): List[T] = {
    if(list == null || list.size == 0)
      Nil
    else
      list.get(0) :: convertList(list.subList(1,list.size))
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
  def processLowerTypeForm(form: PLowerTypeForm,scope: Module): TauType = form match {
    case named: ANamedLowerTypeForm => {
      val name = processQualifiedIdentifier(named.getTypename)
      scope.lookup(name) match {
        case TypeDefinition(gamma,_,_) => gamma
        case _ => throw new Exception("Used an identifier in a type annotation that referred to a non-type definition.")
      }
    }
  }
  def processTypeAnnotation(annotation: PTypeForm,scope: Module): TauType = annotation match {
    case function: AFunctionTypeForm => function.getFunctionTypeForm match {
      case one: AOneFunctionTypeForm => {
        val node = new AOthersTypeForm
        node.setLowerTypeForm(one.getArgument)
        val argument = processTypeAnnotation(node,scope)
        val range = processTypeAnnotation(one.getResult,scope)
        new FunctionArrow(argument :: Nil,range)
      }
    }
    case alias: AAliasTypeForm => {
      val nullable = alias.getNullableAliasClause != null
      val form = processLowerTypeForm(alias.getLowerTypeForm,scope)
      new ReferenceRho(form,nullable,new GlobalScopeType(Some(scope)))
    }
    /*case aclass: AClassTypeForm
    case subrange: ASubrangeTypeForm
    case enum: AEnumTypeForm
    case exception: AExceptionTypeForm*/
    case lower: AOthersTypeForm => processLowerTypeForm(lower.getLowerTypeForm,scope)
  }
  
  def processArgument(arg: AArgument,scope: Module): Tuple2[String,TauType] = {
    val name = arg.getName.getText
    val argType = arg.getType match {
      case null => new TauVariable
      case annotation: ATypeAnnotation => processTypeAnnotation(annotation.getType,scope)
    }
    (name,argType)
  }
  def processArguments(args: PArgumentList,scope: Module): List[Tuple2[String,TauType]] = args match {
    case one: AOneArgumentList => processArgument(one.getArgument match {case real: AArgument => real},scope) :: Nil
    case many: AManyArgumentList => processArgument(many.getArgument match {case real: AArgument => real},scope) :: processArguments(many.getArgumentList,scope)
  }
  
  def processExp1(exp: PExp1,scope: UninferredLexicalScope): UninferredExpression = exp match {
    case variable: AIdentifierExp1 => new UninferredVariable(processQualifiedIdentifier(variable.getQualifiedIdentifier),scope)
  }
  def processExp2(exp: PExp2,scope: UninferredLexicalScope): UninferredExpression = exp match {
    case others: AOthersExp2 => processExp1(others.getExp1,scope)
  }
  def processExp3(exp: PExp3,scope: UninferredLexicalScope): UninferredExpression = exp match {
    case others: AOthersExp3 => processExp2(others.getExp2,scope)
  }
  def processExp4(exp: PExp4,scope: UninferredLexicalScope): UninferredExpression = exp match {
    case others: AOthersExp4 => processExp3(others.getExp3,scope)
  }
  def processExp5(exp: PExp5,scope: UninferredLexicalScope): UninferredExpression = exp match {
    case others: AOthersExp5 => processExp4(others.getExp4,scope)
  }
  def processIf(ifexpr: PIfExpression,scope: UninferredLexicalScope): UninferredIf = ifexpr match {
    case one: AOneIfExpression => {
      val condition = processExpression(one.getCondition,scope)
      val body = processExpression(one.getBody,scope)
      new UninferredIf((condition,body) :: Nil)
    }
    case many: AManyIfExpression => {
      val possibilities = convertList(many.getCases).map(clause => clause match {
        case acase: ACaseIfCaseClause => (processExpression(acase.getCondition,scope),processExpression(acase.getBody,scope))
        case anelse: AElseIfCaseClause => (UninferredTrue,processExpression(anelse.getElseCaseClause.asInstanceOf[AElseCaseClause].getBody,scope))
      })
      new UninferredIf(possibilities)
    }
  }
  def processExpression(expression: PExpression,scope: UninferredLexicalScope): UninferredExpression = expression match {
    case blockexp: ABlockexpExpression => processBlock(blockexp.getBlockExpression,scope)
    case exp5: AOthersExpression => processExp5(exp5.getExp5,scope)
    case condexp: ACondexpExpression => processIf(condexp.getIfExpression,scope)
  }
  
  def processBlockContents(contents: PBlockContents): List[PExpression] = contents match {
    case one:  AOneBlockContents => one.getExpression :: Nil
    case many: AManyBlockContents => processBlockContents(many.getBlockContents) ++ (many.getExpression :: Nil)
  }  
  def processBlock(expr: PBlockExpression,scope: UninferredLexicalScope): UninferredBlock = {
    val expression = expr match { case expression: ABlockExpression => expression }
    new UninferredBlock(processBlockContents(expression.getBlockContents).map(expr => processExpression(expr,scope)))
  }
  
  def processDefinition(adef: PDefinition,scope: Module): Definition = adef match {
    case amoddef: AModuledefDefinition => {
      val moddef: AModuleDefinition = amoddef.getModuleDefinition() match {case real: AModuleDefinition => real}
      val result = new Module(scope,moddef.getName().getText())
      convertList(moddef.getImports()).foreach(imp => declareImportDeclaration(result,imp))
      convertList(moddef.getDefinitions).foreach(definition => processDefinition(definition,result))
      return result
    }
    case afuncdef: AFundefDefinition => {
      afuncdef.getFunctionDefinition() match {
        case normal: AFunctionFunctionDefinition => {
          val name = normal.getName.getText
          val arguments = processArguments(normal.getFunctionArguments match {case args: AFunctionArguments => args.getArguments},scope)
          val uninferred = new UninferredFunction(scope,name,arguments,lexical => processBlock(normal.getBody,lexical))
          uninferred.infer
        }
        case method: AMethodFunctionDefinition => null
        case over: AOverrideFunctionDefinition => null
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
