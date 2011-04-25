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
  def processTupleComponent(component: PTupleComponent,scope: TypeBindingScope): RecordMember = component match {
    case form: ATypeTupleComponent => RecordMember(None,processTypeForm(form.getTypeForm,scope))
    case binding: ABindingTupleComponent => {
      val annotation = binding.getTypeAnnotation.asInstanceOf[ATypeAnnotation].getType
      RecordMember(Some(binding.getName.getText()),processTypeForm(annotation,scope))
    }
  }
  def processTupleComponents(components: PTupleComponentList,scope: TypeBindingScope): List[RecordMember] = components match {
    case one: AOneTupleComponentList => processTupleComponent(one.getTupleComponent,scope) :: Nil
    case many: AManyTupleComponentList => processTupleComponents(many.getTupleComponentList,scope) ++ (processTupleComponent(many.getTupleComponent,scope) :: Nil)
  }
  def processTupleForm(form: ATupleForm,scope: TypeBindingScope): RecordProduct = {
    new RecordProduct(processTupleComponents(form.getTupleComponentList,scope))
  }
  def processVariantComponents(components: List[PVariantComponent],scope: TypeBindingScope): List[TaggedProduct] = {
    components.map(_.asInstanceOf[AVariantComponent]).map(component => {
      val constructor = DataConstructor(Some(component.getUnqualifiedIdentifier.getText),None)
      val record = if(component.getVariantCaseContents != null) {
        val members = processTupleComponents(component.getVariantCaseContents.asInstanceOf[AVariantCaseContents].getTupleComponentList,scope)
        new RecordProduct(members)
      }
      else
        EmptyRecord
      TaggedProduct(constructor,record)
    })
  }
  def processLowerTypeForm(form: PLowerTypeForm,scope: TypeBindingScope): TauType = form match {
    case named: ANamedLowerTypeForm => {
      val name = processQualifiedIdentifier(named.getTypename)
      scope.lookup(name) match {
        //TODO: Add support for using type arguments
        case defin: TypeDefinition => {
          if(named.getTypeParameterization != null) {
            val args = processTypeForms(named.getTypeParameterization.asInstanceOf[ATypeParameterization].getArguments,scope)
            defin.sigma.instantiate(args)
          }
          else
            defin.sigma.freshlyInstantiate
        }
        case binding: TypeBinding => binding.tau
        case _ => throw new Exception("Used an identifier in a type annotation that referred to a non-type definition.")
      }
    }
    case tuple: ATupleLowerTypeForm => processTupleForm(tuple.getTupleForm.asInstanceOf[ATupleForm],scope)
    case array: AArrayLowerTypeForm => {
      val element = processLowerTypeForm(array.getLowerTypeForm,scope)
      val length = if(array.getIntegerConstant != null) Some(array.getIntegerConstant.getText.toInt) else None
      new ArrayType(element,length)
    }
  }
  def processTypeForms(forms: PTypeFormList,scope: TypeBindingScope): List[TauType] = forms match {
    case one: AOneTypeFormList => processTypeForm(one.getTypeForm,scope) :: Nil
    case many: AManyTypeFormList => processTypeForms(many.getTypeFormList,scope) ++ (processTypeForm(many.getTypeForm,scope) :: Nil)
  }
  def processTypeForm(annotation: PTypeForm,scope: TypeBindingScope): TauType = annotation match {
    case function: AFunctionTypeForm => function.getFunctionTypeForm match {
      case one: AOneFunctionTypeForm => {
        val node = new AOthersTypeForm
        node.setLowerTypeForm(one.getArgument)
        val argument = processTypeForm(node,scope)
        val range = processTypeForm(one.getResult,scope)
        new ClosureArrow(argument :: Nil,range)
      }
      case many: AManyFunctionTypeForm => {
        val formals: List[TauType] = if(many.getFunctionArgumentsTypeForm != null) {
          val args = many.getFunctionArgumentsTypeForm.asInstanceOf[AFunctionArgumentsTypeForm]
          processTypeForm(args.getTypeForm,scope) :: processTypeForms(args.getTypeFormList,scope)
        }
        else
          Nil
        val result = processTypeForm(many.getResult,scope)
        new ClosureArrow(formals,result)
      }
    }
    case scopedPointer: AScopedPointerTypeForm => {
      val form = processLowerTypeForm(scopedPointer.getLowerTypeForm,scope)
      new ScopedPointer(form,new GlobalScopeType(Some(scope.parent)))
    }
    case variant: AVariantTypeForm => {
      val componentForms = convertList(variant.getVariantComponent)
      new SumType(processVariantComponents(componentForms,scope))
    }
    /*case aclass: AClassTypeForm
    case subrange: ASubrangeTypeForm
    case enum: AEnumTypeForm
    case exception: AExceptionTypeForm*/
    case lower: AOthersTypeForm => processLowerTypeForm(lower.getLowerTypeForm,scope)
  }
  
  def processArgument(arg: AArgument,scope: TypeBindingScope): Tuple2[String,TauType] = {
    val name = arg.getName.getText
    val argType = arg.getType match {
      case null => new TauVariable
      case annotation: ATypeAnnotation => processTypeForm(annotation.getType,scope)
    }
    (name,argType)
  }
  def processArguments(args: PArgumentList,scope: TypeBindingScope): List[Tuple2[String,TauType]] = args match {
    case one: AOneArgumentList => processArgument(one.getArgument match {case real: AArgument => real},scope) :: Nil
    case many: AManyArgumentList => processArguments(many.getArgumentList,scope) ++ (processArgument(many.getArgument match {case real: AArgument => real},scope) :: Nil)
    case null => Nil
  }
  
  def processLiteral(exp: PLiteralExpression,scope: UninferredLexicalScope): UninferredExpression = exp match {
    case integer: AIntegerLiteralExpression => new UninferredInteger(integer.getIntegerConstant.getText.toInt)
    case bool: ABooleanLiteralExpression => new BooleanLiteralExpression(bool.getBooleanConstant.getText == "true")
  }
  
  def processExp1(exp: PExp1,scope: UninferredLexicalScope): UninferredExpression = exp match {
    case variable: AIdentifierExp1 => new UninferredVariable(processQualifiedIdentifier(variable.getQualifiedIdentifier),scope)
    case literal: ALiteralExp1 => processLiteral(literal.getLiteralExpression,scope)
    case parens: AParentheticalExp1 => processExpression(parens.getParentheticalExpression.asInstanceOf[AParentheticalExpression].getExpression,scope)
    case call: ACallExp1 => processCallExpression(call.getFunctionCallExpression,scope)
    case tuple: ATupleExp1 => new UninferredTuple(processExpressionList(tuple.getExpressionList,scope))
    case field: AFieldExp1 => new UninferredMember(processExp1(field.getExp1,scope),field.getMemberSelector match {
      case name: ANameMemberSelector => NameSelector(name.getUnqualifiedIdentifier.getText)
      case index: AIndexMemberSelector => IndexSelector(index.getIntegerConstant.getText.toInt)
    })
  }
  def processExp2(exp: PExp2,scope: UninferredLexicalScope): UninferredExpression = exp match {
    case minus: AMinusExp2 => new UninferredOperator(new UninferredInteger(0),processExp2(minus.getExp2,scope),Subtract)
    case others: AOthersExp2 => processExp1(others.getExp1,scope)
  }
  def processExp3(exp: PExp3,scope: UninferredLexicalScope): UninferredExpression = exp match {
    case mult: AMultiplyExp3 => new UninferredOperator(processExp3(mult.getExp1,scope),processExp2(mult.getExp2,scope),Multiply)
    case divide: ADivisionExp3 => new UninferredOperator(processExp3(divide.getExp1,scope),processExp2(divide.getExp2,scope),Divide)
    case others: AOthersExp3 => processExp2(others.getExp2,scope)
  }
  def processExp4(exp: PExp4,scope: UninferredLexicalScope): UninferredExpression = exp match {
    case add: APlusExp4 => new UninferredOperator(processExp4(add.getExp1,scope),processExp3(add.getExp2,scope),Add)
    case sub: AMinusExp4 => new UninferredOperator(processExp4(sub.getExp1,scope),processExp3(sub.getExp2,scope),Subtract)
    case others: AOthersExp4 => processExp3(others.getExp3,scope)
  }
  def processExp5(exp: PExp5,scope: UninferredLexicalScope): UninferredExpression = exp match {
    case greater: AGreaterExp5 => new UninferredComparison(GreaterComp(false),processExp4(greater.getExp1,scope),processExp4(greater.getExp2,scope))
    case greatereq: AGreatereqExp5 => new UninferredComparison(GreaterComp(true),processExp4(greatereq.getExp1,scope),processExp4(greatereq.getExp2,scope))
    case lesser: ALessExp5 => new UninferredComparison(LesserComp(false),processExp4(lesser.getExp1,scope),processExp4(lesser.getExp2,scope))
    case lessereq: ALessereqExp5 => new UninferredComparison(LesserComp(true),processExp4(lessereq.getExp1,scope),processExp4(lessereq.getExp2,scope))
    case equals: AEqualsExp5 => new UninferredComparison(EqualComp,processExp4(equals.getExp1,scope),processExp4(equals.getExp2,scope))
    case different: ADifferentExp5 => new UninferredComparison(DifferentComp,processExp4(different.getExp1,scope),processExp4(different.getExp2,scope))
    case others: AOthersExp5 => processExp4(others.getExp4,scope)
  }
  def processCallExpression(call: PFunctionCallExpression,scope: UninferredLexicalScope): UninferredCall = call match {
    case named: AVariableFunctionCallExpression => {
      val name = processQualifiedIdentifier(named.getFunction)
      val arguments = if(named.getArguments != null) processExpressionList(named.getArguments,scope) else Nil
      scope.lookup(name) match {
        case func: FunctionDefinition => new UninferredDefinitionCall(func,arguments)
        case binding: UninferredLexicalBinding => new UninferredExpressionCall(new UninferredVariable(name,scope),arguments)
      }
    }
    case expr: AParensFunctionCallExpression => {
      val arguments = processExpressionList(expr.getArguments,scope)
      val func = processExpression(expr.getFunction.asInstanceOf[AParentheticalExpression].getExpression,scope)
      new UninferredExpressionCall(func,arguments)
    }
  }
  def processIfThen(ifthen: AIfwithoutelseexpExpression,scope: UninferredLexicalScope): UninferredIf = {
    val condition = processExpression(ifthen.getCondition,scope)
    val body = processExpression(ifthen.getThenbody,scope)
    new UninferredIf(condition,body,None)
  }
  def processIfElse(ifelse: AIfwithelseexpExpression,scope: UninferredLexicalScope): UninferredIf = {
    val condition = processExpression(ifelse.getCondition,scope)
    val body = processExpressionWithElse(ifelse.getThenbody,scope)
    val otherwise = processExpression(ifelse.getElseClause.asInstanceOf[AElseClause].getElseBody,scope)
    new UninferredIf(condition,body,Some(otherwise))
  }
  def processIfElseWithElse(ifelse: AIfwithelseexpExpression,scope: UninferredLexicalScope): UninferredIf = {
    val condition = processExpression(ifelse.getCondition,scope)
    val body = processExpressionWithElse(ifelse.getThenbody,scope)
    val otherwise = processExpression(ifelse.getElseClause.asInstanceOf[AElseClause].getElseBody,scope)
    new UninferredIf(condition,body,Some(otherwise))
  }
  def processIfElseWithElseWithElse(ifelse: AIfwithelseexpExpressionWithElse,scope: UninferredLexicalScope): UninferredIf = {
    val condition = processExpression(ifelse.getCondition,scope)
    val body = processExpressionWithElse(ifelse.getThenbody,scope)
    val otherwise = processExpressionWithElse(ifelse.getElsebody,scope)
    new UninferredIf(condition,body,Some(otherwise))
  }
  def processExpressionWithElse(expression: PExpressionWithElse,scope: UninferredLexicalScope): UninferredExpression = expression match {
    case blockexp: ABlockexpExpressionWithElse => processBlock(blockexp.getBlockExpression,scope)
    case exp5: AOthersExpressionWithElse => processExp5(exp5.getExp5,scope)
    case ifelse: AIfwithelseexpExpressionWithElse => processIfElseWithElseWithElse(ifelse,scope)
    case cast: ACastexpExpressionWithElse => {
      var tscope: Scope[_] = scope
      while(!tscope.isInstanceOf[Module] && tscope.parent != null)
        tscope = tscope.parent
      val tau = processTypeForm(cast.getTypeForm,new TypeBindingScope(tscope.asInstanceOf[Module]))
      val expr = processExpression(cast.getExpression,scope)
      new UninferredBitcast(expr,tau)
    }
  }
  def processExpression(expression: PExpression,scope: UninferredLexicalScope): UninferredExpression = expression match {
    case blockexp: ABlockexpExpression => processBlock(blockexp.getBlockExpression,scope)
    case exp5: AOthersExpression => processExp5(exp5.getExp5,scope)
    case ifthen: AIfwithoutelseexpExpression => processIfThen(ifthen,scope)
    case ifelse: AIfwithelseexpExpression => processIfElse(ifelse,scope)
    case cast: ACastexpExpression => {
      var tscope: Scope[_] = scope
      while(!tscope.isInstanceOf[Module] && tscope.parent != null)
        tscope = tscope.parent
      val tau = processTypeForm(cast.getTypeForm,new TypeBindingScope(tscope.asInstanceOf[Module]))
      val expr = processExpression(cast.getExpression,scope)
      new UninferredBitcast(expr,tau)
    }
  }
  
  def processExpressionList(exprs: PExpressionList,scope: UninferredLexicalScope): List[UninferredExpression] = exprs match {
    case one: AOneExpressionList => processExpression(one.getExpression,scope) :: Nil
    case many: AManyExpressionList => processExpressionList(many.getExpressionList,scope) ++ (processExpression(many.getExpression,scope) :: Nil)
  }
  def processBlockSteps(contents: PBlockStepsList): List[PExpression] = contents match {
    case one: AOneBlockStepsList => one.getExpression :: Nil
    case many: AManyBlockStepsList => many.getExpression :: processBlockSteps(many.getBlockStepsList)
  }
  def processBlockContents(contents: PBlockContents): List[PExpression] = contents match {
    case one:  AOneBlockContents => one.getExpression :: Nil
    case many: AManyBlockContents => many.getExpression :: processBlockSteps(many.getBlockStepsList)
  }  
  def processBlock(expr: PBlockExpression,scope: UninferredLexicalScope): UninferredBlock = {
    val expression = expr match { case expression: ABlockExpression => expression }
    new UninferredBlock(processBlockContents(expression.getBlockContents).map(expr => processExpression(expr,scope)))
  }
  
  def processFunctionDefinition(func: PFunctionDefinition,scope: Module): Definition = func match {
    case normal: AFunctionFunctionDefinition => {
      val name = normal.getName.getText
      val tscope = new TypeBindingScope(scope)
      if(normal.getTypeFormArguments != null) {
       val params = processTypeParameters(normal.getTypeFormArguments.asInstanceOf[ATypeFormArguments].getArguments)
        for((argName,tau) <- params)
          new TypeBinding(tau,argName,tscope)
      }
      val arguments = processArguments(normal.getFunctionArguments match {case args: AFunctionArguments => args.getArguments},tscope).map(arg => (arg._1,UninferredArgument(arg._2)))
      val resultType = if(normal.getType != null) Some(processTypeForm(normal.getType.asInstanceOf[ATypeAnnotation].getType,tscope)) else None
      val function = new ExpressionFunction(tscope,name,arguments,resultType,lexical => processBlock(normal.getBody,lexical))
      function.infer
      function
    }
    case method: AMethodFunctionDefinition => null
    case over: AOverrideFunctionDefinition => null
    case external: AExternalFunctionDefinition => {
      val name = external.getName.getText
      val tscope = new TypeBindingScope(scope)
      val arguments = processArguments(external.getFunctionArguments match {case args: AFunctionArguments => args.getArguments},tscope).map(arg => (arg._1,arg._2.asInstanceOf[GammaType]))
      assert(arguments.forall(arg => arg match {
        case rho: RhoType => rho.filter(tau => !tau.isInstanceOf[GammaType]) == Nil
        case gamma: GammaType => true
        case _ => false
      }))
      val resultType = processTypeForm(external.getType.asInstanceOf[ATypeAnnotation].getType,tscope).asInstanceOf[GammaType]
      val function = new ExternalFunction(tscope,name,arguments,resultType)
      function
    }
  }
  
  def processDefinition(adef: PDefinition,scope: Module): Definition = adef match {
    case amoddef: AModuledefDefinition => {
      val moddef: AModuleDefinition = amoddef.getModuleDefinition() match {case real: AModuleDefinition => real}
      val result = new Module(scope,moddef.getName().getText())
      convertList(moddef.getImports()).foreach(imp => declareImportDeclaration(result,imp))
      convertList(moddef.getDefinitions).foreach(definition => processDefinition(definition,result))
      return result
    }
    case afuncdef: AFundefDefinition => processFunctionDefinition(afuncdef.getFunctionDefinition(),scope)
    case atypedef: ATypedefDefinition => {
      val name = atypedef.getUnqualifiedIdentifier.getText
      val tscope = new TypeBindingScope(scope)
      if(atypedef.getParameters != null) {
        val params = processTypeParameters(atypedef.getParameters.asInstanceOf[ATypeFormArguments].getArguments)
        for((argName,tau) <- params)
          new TypeBinding(tau,argName,tscope)
      }
      val alpha = new TauVariable
      new TypeBinding(alpha,name,tscope)
      val sigma = processTypeForm(atypedef.getTypeForm,tscope) match {
        case rho: RhoType => {
          val rhomu = if(rho.filter(tau => tau == alpha) != Nil) new RecursiveMu(rho,UnrecursiveAlpha(alpha)) else rho
          rhomu.generalize(new TauSubstitution)
        }
        case gamma: GammaType => gamma
        case _ => throw new Exception("Type definition has not resulted in a sigma type.")
      }
      val result = new TypeDefinition(sigma,name,scope)
      sigma.define(result)
      //Remember to create constructors for variants.
      sigma.body match {
        case sum: SumType => {
          for(addend <- sum.sumCases)
            if(TauOrdering.equiv(addend.record,EmptyRecord))
              new ModuleVariableDefinition(scope,addend.name.name.get,new EnumerationValue(addend),false)
            else
              new DefaultConstructor(scope,addend)
        }
        case _ => Unit
      }
      result
    }
    case avardef: AGlobaldefDefinition => {
      null
    }
  }
}
