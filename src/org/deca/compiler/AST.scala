package org.deca.compiler

import org.deca.compiler.parser.parser._
import org.deca.compiler.parser.lexer._
import org.deca.compiler.parser.node._
import org.deca.compiler.definition._
import org.deca.compiler.expression._
import org.deca.compiler.signature._
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
  
  def processTypeParameters(parameters: PIdentifierList): List[String] = parameters match {
    case one: AOneIdentifierList => one.getUnqualifiedIdentifier.getText :: Nil
    case many: AManyIdentifierList => many.getUnqualifiedIdentifier.getText :: processTypeParameters(many.getIdentifierList)
  }
  def processSlotMutability(mutability: TSlotMutability): MonoMutability = mutability.getText match {
    case "val" => ReadOnlyMutability
    case "var" => MutableMutability
    case "constant" => ImmutableMutability
  }
  def processTupleComponent(component: PTupleComponent,scope: TypeDefinitionScope): RecordMember = component match {
    case form: ATypeTupleComponent => {
      val mutability = processSlotMutability(form.getSlotMutability)
      RecordMember(None,mutability,processTypeForm(form.getTypeForm,scope))
    }
    case binding: ABindingTupleComponent => {
      val annotation = binding.getTypeAnnotation.asInstanceOf[ATypeAnnotation].getType
      val mutability = processSlotMutability(binding.getSlotMutability)
      RecordMember(Some(binding.getName.getText()),mutability,processTypeForm(annotation,scope))
    }
  }
  def processTupleComponents(components: PTupleComponentList,scope: TypeDefinitionScope): List[RecordMember] = components match {
    case one: AOneTupleComponentList => processTupleComponent(one.getTupleComponent,scope) :: Nil
    case many: AManyTupleComponentList => processTupleComponent(many.getTupleComponent,scope) :: processTupleComponents(many.getTupleComponentList,scope)
  }
  def processSlotDeclaration(decl: PSlotDeclaration,scope: TypeDefinitionScope): (String,MonoType,MonoMutability) = {
    val adecl = decl.asInstanceOf[ASlotDeclaration]
    val name = adecl.getUnqualifiedIdentifier.getText
    val tau = if(adecl.getTypeAnnotation != null)
      processTypeForm(adecl.getTypeAnnotation.asInstanceOf[ATypeAnnotation].getType,scope)
    else
      new TypeVariable(false,None)
    (name,tau,processSlotMutability(adecl.getSlotMutability))
  }
  def processMemberAssignment(decl: PMemberAssignment,scope: TypeDefinitionScope): MemberDeclaration = {
    val slot: (String,MonoType,MonoMutability) = processSlotDeclaration(decl.asInstanceOf[AMemberAssignment].getSlotDeclaration,scope)
    val builder = (lexical: LexicalScope) => processExpression(decl.asInstanceOf[AMemberAssignment].getExpression,lexical)
    new MemberDeclaration(slot._1,slot._3,slot._2,builder)
  }
  def processExtensionClause(extension: PExtensionClause, scope: Module): ClassDefinition =
    scope.lookup(processQualifiedIdentifier(extension.asInstanceOf[AExtensionClause].getQualifiedIdentifier)).asInstanceOf[ClassDefinition]
  def processStructuralExtension(extension: PStructuralExtension, scope: TypeDefinitionScope): RecordMember = extension match {
    case field: AFieldStructuralExtension => {
      val tau = processTypeForm(field.getTypeAnnotation.asInstanceOf[ATypeAnnotation].getType, scope)
      val mut = processSlotMutability(field.getSlotMutability)
      RecordMember(Some(field.getName.getText), mut, tau)
    }
    case method: AMethodStructuralExtension => throw new Exception("TODO: method structural extension ASTs and semantic support")
  }
  def processStructuralExtensionList(list: PStructuralExtensionList, scope: TypeDefinitionScope): List[RecordMember] = list match {
    case one: AOneStructuralExtensionList => processStructuralExtension(one.getStructuralExtension, scope) :: Nil
    case many: AManyStructuralExtensionList =>
      processStructuralExtension(many.getStructuralExtension, scope) :: processStructuralExtensionList(many.getStructuralExtensionList, scope)
  }
  def processBasicTypeForm(form: PBasicTypeForm,scope: TypeDefinitionScope): MonoType = form match {
    case named: ANamedBasicTypeForm => {
      val name = processQualifiedIdentifier(named.getTypename)
      scope.lookup(name) match {
        case defin: TypeDefinition => {
          if(named.getTypeParameterization != null) {
            val args = processTypeForms(named.getTypeParameterization.asInstanceOf[ATypeParameterization].getArguments,scope)
            defin.constructor.represent(args)
          }
          else
            defin.constructor.freshlyRepresent
        }
        case binding: TypeBinding => binding.tau
        case _ => throw new Exception("Used an identifier in a type annotation that referred to a non-type definition.")
      }
    }
    case wildcard: AWildcardBasicTypeForm => new TypeVariable(false,None)
  }
  def processLowerTypeForm(form: PLowerTypeForm,scope: TypeDefinitionScope): MonoType = form match {
    case tuple: ATupleLowerTypeForm => new RecordType(processTupleComponents(tuple.getTupleComponentList,scope))
    case array: AArrayLowerTypeForm => {
      val element = processBasicTypeForm(array.getBasicTypeForm,scope)
      val length = if(array.getIntegerConstant != null) Some(array.getIntegerConstant.getText.toInt) else None
      val mutability = array.getSlotMutability match {
        case null => ReadOnlyMutability
        case mut: TSlotMutability => processSlotMutability(mut)
      }
      new ArrayType(element,mutability,length)
    }
    case basic: AOthersLowerTypeForm => processBasicTypeForm(basic.getBasicTypeForm,scope)
  }
  def processTypeForms(forms: PTypeFormList,scope: TypeDefinitionScope): List[MonoType] = forms match {
    case null => Nil
    case one: AOneTypeFormList => processTypeForm(one.getTypeForm,scope) :: Nil
    case many: AManyTypeFormList => processTypeForms(many.getTypeFormList,scope) ++ (processTypeForm(many.getTypeForm,scope) :: Nil)
  }
  def processEffectForm(form: PEffectForm,scope: TypeDefinitionScope): MonoEffect = form match {
    case regional: ARegionalEffectForm => {
      val module: Module = scope.typedLookup[Module](regional.getEffectRegion.asInstanceOf[AEffectRegion].getUnqualifiedIdentifier.getText)
      regional.getRegionalEffectName.getText match {
        case "!read" => ReadEffect(module.region)
        case "!write" => WriteEffect(module.region)
        case "!destroy" => DestroyEffect(module.region)
        case "!call" => CallEffect(module.region)
      }
    }
    case exceptional: AExceptionalEffectForm => ThrowEffect(processTypeForm(exceptional.getTypeForm,scope))
  }
  def processEffectForms(forms: PEffectFormList,scope: TypeDefinitionScope): List[MonoEffect] = forms match {
    case null => Nil
    case one: AOneEffectFormList => processEffectForm(one.getEffectForm,scope) :: Nil
    case many: AManyEffectFormList => processEffectForms(many.getEffectFormList,scope) ++ List(processEffectForm(many.getEffectForm,scope))
  }
  def processEffectSet(set: PEffectSet, scope: TypeDefinitionScope): List[MonoEffect] = set match {
    case nonempty: ANonemptyEffectSet => processEffectForms(nonempty.getEffectFormList, scope)
    case empty: AEmptyEffectSet => Nil
  }
  def processEffectAnnotation(effect: PEffectSignature,scope: TypeDefinitionScope): (MonoEffect,MonoEffect) = effect match {
    case plus: APlusEffectSignature => {
      val effectForms = processEffectSet(plus.getPositiveEffect.asInstanceOf[APositiveEffect].getEffectSet, scope)
      (SetEffect(effectForms.toSet),PureEffect)
    }
    case minus: AMinusEffectSignature => {
      val effectForms = processEffectSet(minus.getNegativeEffect.asInstanceOf[APositiveEffect].getEffectSet, scope)
      (PureEffect,SetEffect(effectForms.toSet))
    }
    case both: ABothEffectSignature => {
      val plusForms = processEffectSet(both.getPositiveEffect.asInstanceOf[APositiveEffect].getEffectSet, scope)
      val minusForms = processEffectSet(both.getNegativeEffect.asInstanceOf[APositiveEffect].getEffectSet, scope)
      (SetEffect(plusForms.toSet),SetEffect(minusForms.toSet))
    }
  }
  def processTypeForm(annotation: PTypeForm, scope: TypeDefinitionScope): MonoType = annotation match {
    case function: AFunctionTypeForm => function.getFunctionTypeForm match {
      case p: APointerFunctionTypeForm => {
        val pointer = p.getFunctionPointerForm.asInstanceOf[AFunctionPointerForm]
        val formals = {
          val args = pointer.getFormals.asInstanceOf[AArgumentsForm].getTypeFormList
          processTypeForms(args,scope)
        }
        val implicits = {
          val args = pointer.getImplicits.asInstanceOf[AArgumentsForm].getTypeFormList
          processTypeForms(args,scope)
        }
        val effect = processEffectAnnotation(pointer.getEffect.asInstanceOf[AEffectAnnotation].getEffectSignature,scope)
        val result = processTypeForm(pointer.getResult,scope)
        new FunctionPointer(formals ++ implicits,result,effect._1,effect._2)
      }
      case c: AFirstclassFunctionTypeForm => {
        val closure = c.getFirstclassFunctionForm.asInstanceOf[AFirstclassFunctionForm]
        val effect = processEffectAnnotation(closure.getEffect.asInstanceOf[AEffectAnnotation].getEffectSignature,scope)
        val result = processTypeForm(closure.getResult,scope)
        val formals = processTypeForms(closure.getFormals.asInstanceOf[AArgumentsForm].getTypeFormList,scope)
        val implicits = processTypeForms(closure.getImplicits.asInstanceOf[AArgumentsForm].getTypeFormList,scope)
        ClosureTypes.apply(new FunctionPointer(formals ++ implicits,result,effect._1,effect._2))
      }
    }
    case pointer: APointerTypeForm => {
      val pointerForm: (MonoMutability,MonoType) = pointer.getPointerTypeForm match {
        case simple: ASimplePointerTypeForm => (ReadOnlyMutability,processLowerTypeForm(simple.getLowerTypeForm,scope))
        case complex: AComplexPointerTypeForm => (processSlotMutability(complex.getSlotMutability),processTypeForm(complex.getTypeForm,scope))
      }
      //TODO: Shouldn't this region be a region variable?
      new PointerType(pointerForm._2, scope.owner.region, pointerForm._1)
    }
    case brandType: AClassTypeForm => {
      val brand = scope.owner.lookup(processQualifiedIdentifier(brandType.getQualifiedIdentifier)).asInstanceOf[ClassDefinition].brand
      scope.bind("#" + brand.toString, Some(new OpaqueType))
      val extension: RecordType = if(brandType.getStructuralExtensionForm != null) {
          val members = processStructuralExtensionList(brandType.getStructuralExtensionForm.asInstanceOf[AStructuralExtensionForm].getStructuralExtensionList, scope)
          new RecordType(members)
        }
        else
          EmptyRecord
      //TODO: Enable methodwise structural extension of brands, see: TypeSignature.scala
      new BrandType(brand, extension)
    }
    //case exception: AExceptionTypeForm
    case lower: AOthersTypeForm => processLowerTypeForm(lower.getLowerTypeForm,scope)
  }
  
  def processArgument(arg: AArgument,scope: TypeDefinitionScope): Tuple2[String,MonoType] = {
    val name = arg.getName.getText
    val argType = arg.getType match {
      case null => new TypeVariable(false,Some("'" + name))
      case annotation: ATypeAnnotation => processTypeForm(annotation.getType,scope)
    }
    (name,argType)
  }
  def processArguments(args: PArgumentList,scope: TypeDefinitionScope): List[(String,MonoType)] = args match {
    case null => Nil
    case one: AOneArgumentList => processArgument(one.getArgument.asInstanceOf[AArgument],scope) :: Nil
    case many: AManyArgumentList => processArgument(many.getArgument.asInstanceOf[AArgument],scope) :: processArguments(many.getArgumentList,scope)
  }
  
  def processLiteral(exp: PLiteralExpression,scope: LexicalScope): Expression = exp match {
    case integer: AIntegerLiteralExpression => new IntegerLiteralExpression(integer.getIntegerConstant.getText.toInt)
    case bool: ABooleanLiteralExpression => new BooleanLiteralExpression(bool.getBooleanConstant.getText == "true")
  }
  def processActualImplicit(impl: PActualImplicit,scope: LexicalScope): Option[Expression] = impl match {
    case explicit: AExplicitActualImplicit => Some(processExpression(explicit.getExpression,scope))
    case scoped: AImplicitActualImplicit => None
  }
  def processActualImplicitList(implicits: PActualImplicitList,scope: LexicalScope): List[Option[Expression]] = implicits match {
    case one: AOneActualImplicitList => processActualImplicit(one.getActualImplicit,scope) :: Nil
    case many: AManyActualImplicitList => processActualImplicit(many.getActualImplicit,scope) :: processActualImplicitList(many.getActualImplicitList,scope)
  }
  def processCallExpression(call: PFunctionCallExpression,scope: LexicalScope): CallExpression = call match {
    case named: ANamedFunctionCallExpression => {
      val name = processQualifiedIdentifier(named.getFunction)
      val actualParameters = if(named.getActualParameters != null) {
        val actuals = named.getActualParameters.asInstanceOf[AActualParameters]
        val arguments = if(actuals.getArguments != null) Some(actuals.getArguments) else None
        val implicits = if(actuals.getActualImplicitParameters != null) Some(actuals.getActualImplicitParameters.asInstanceOf[AActualImplicitParameters].getActualImplicitList) else None
        (arguments,implicits)
      }
      else
        (None,None)
      val arguments = actualParameters._1 match {
        case Some(args) => processExpressionList(args,scope)
        case None => Nil
      }
      scope.lookup(name) match {
        case func: FunctionDefinition => {
          val implicits: List[Option[Expression]] = actualParameters._2 match {
            case Some(impls) => processActualImplicitList(impls,scope)
            case None => func.signature.implicits.map(impl => None)
          }
          new DefinitionCall(func,arguments,(implicits,scope))
        }
        case binding: LexicalBinding => new ExpressionCall(new VariableExpression(name,scope), arguments)
      }
    }
    case expr: AExprFunctionCallExpression => {
      val arguments = if(expr.getActualParameters != null) {
        val actuals = expr.getActualParameters.asInstanceOf[AActualParameters]
        if(actuals.getArguments != null) processExpressionList(actuals.getArguments,scope) else Nil
      }
      else
        Nil
      val func = processExpression(expr.getFunction.asInstanceOf[AParentheticalExpression].getExpression,scope)
      new ExpressionCall(func, arguments)
    }
  }
  def processIfThen(ifthen: AIfwithoutelseexpExpression,scope: LexicalScope): IfExpression = {
    val condition = processExpression(ifthen.getCondition,scope)
    val body = processExpression(ifthen.getThenbody,scope)
    new IfExpression(condition,body,None)
  }
  def processIfElse(ifelse: AIfwithelseexpExpression,scope: LexicalScope): IfExpression = {
    val condition = processExpression(ifelse.getCondition,scope)
    val body = processExpressionWithElse(ifelse.getThenbody,scope)
    val otherwise = processExpression(ifelse.getElseClause.asInstanceOf[AElseClause].getElseBody,scope)
    new IfExpression(condition,body,Some(otherwise))
  }
  def processIfElseWithElse(ifelse: AIfwithelseexpExpression,scope: LexicalScope): IfExpression = {
    val condition = processExpression(ifelse.getCondition,scope)
    val body = processExpressionWithElse(ifelse.getThenbody,scope)
    val otherwise = processExpression(ifelse.getElseClause.asInstanceOf[AElseClause].getElseBody,scope)
    new IfExpression(condition,body,Some(otherwise))
  }
  def processIfElseWithElseWithElse(ifelse: AIfwithelseexpExpressionWithElse,scope: LexicalScope): IfExpression = {
    val condition = processExpression(ifelse.getCondition,scope)
    val body = processExpressionWithElse(ifelse.getThenbody,scope)
    val otherwise = processExpressionWithElse(ifelse.getElsebody,scope)
    new IfExpression(condition,body,Some(otherwise))
  }
  def processExpressionWithElse(expression: PExpressionWithElse,scope: LexicalScope): Expression = expression match {
    case assignment: AAssignmentexpExpressionWithElse => {
      val left: WritableExpression = processExp1(assignment.getExp1,scope).asInstanceOf[WritableExpression]
      val right = processExpressionWithElse(assignment.getExpressionWithElse,scope)
      new AssignmentExpression(left,right)
    }
    case blockexp: ABlockexpExpressionWithElse => processBlock(blockexp.getBlockExpression,scope)
    case exp5: AOthersExpressionWithElse => processExp5(exp5.getExp5,scope)
    case ifelse: AIfwithelseexpExpressionWithElse => processIfElseWithElseWithElse(ifelse,scope)
  }
  
  def processExpressionList(exprs: PExpressionList,scope: LexicalScope): List[Expression] = exprs match {
    case one: AOneExpressionList => processExpression(one.getExpression,scope) :: Nil
    case many: AManyExpressionList => processExpressionList(many.getExpressionList,scope) ++ (processExpression(many.getExpression,scope) :: Nil)
  }
  
  //TODO: enable syntactic processing of effect annotations
  def processFunctionDefinition(func: PFunctionDefinition,scope: Module): Definition = func match {
    case normal: AFunctionFunctionDefinition => {
      val name = normal.getName.getText
      val typeParameters: List[String] = if(normal.getTypeFormArguments != null)
        processTypeParameters(normal.getTypeFormArguments.asInstanceOf[ATypeFormArguments].getArguments)
      else
        Nil
      val tscope = new TypeDefinitionScope(typeParameters,scope)
      val arguments: List[(String,MonoType)] = processArguments(normal.getFunctionArguments.asInstanceOf[AFunctionArguments].getArguments, tscope)
      val implicits = Option.apply(normal.getFunctionArguments.asInstanceOf[AFunctionArguments].getImplicitArguments) match {
        case Some(implArgs) => processArguments(implArgs.asInstanceOf[AImplicitArguments].getImplicits, tscope)
        case None => Nil
      }
      val resultType = Option.apply(processTypeForm(normal.getType.asInstanceOf[ATypeAnnotation].getType,tscope)) getOrElse (new TypeVariable(false, None))
      val body = (sig: FunctionSignature) => new ExpressionBody(sig, scope, (lexical: LexicalScope) => processExpression(normal.getBody, lexical))
      new FunctionDefinition(name, scope, FunctionSignature(arguments, implicits, resultType), Some(body))
    }
    case external: AExternalFunctionDefinition => {
      val name = external.getName.getText
      val tscope = new TypeDefinitionScope(Nil, scope)
      val arguments = processArguments(external.getFunctionArguments.asInstanceOf[AFunctionArguments].getArguments, tscope)
      assert(arguments.forall(arg => arg._2.variables.isEmpty))
      val resultType = processTypeForm(external.getType.asInstanceOf[ATypeAnnotation].getType, tscope)
      assert(resultType.variables.isEmpty)
      new FunctionDefinition(name, scope, FunctionSignature(arguments, Nil, resultType), None)
    }
  }
  def processExp1(exp: PExp1,scope: LexicalScope): Expression = exp match {
    case variable: AIdentifierExp1 => new VariableExpression(processQualifiedIdentifier(variable.getQualifiedIdentifier),scope)
    case literal: ALiteralExp1 => processLiteral(literal.getLiteralExpression,scope)
    case parens: AParentheticalExp1 => processExpression(parens.getParentheticalExpression.asInstanceOf[AParentheticalExpression].getExpression,scope)
    case call: ACallExp1 => processCallExpression(call.getFunctionCallExpression,scope)
    case tuple: ATupleExp1 => new TupleExpression(processExpressionList(tuple.getExpressionList,scope))
    case field: AFieldExp1 => new MemberExpression(processExp1(field.getExp1,scope),field.getMemberSelector match {
      case name: ANameMemberSelector => NameSelector(name.getUnqualifiedIdentifier.getText)
      case index: AIndexMemberSelector => IndexSelector(index.getIntegerConstant.getText.toInt)
    })
    case cast: ACastExp1 => {
      var tscope: Scope = scope
      while(!tscope.isInstanceOf[Module] && tscope.parent != None)
        tscope = tscope.parent.get
      val tau = processTypeForm(cast.getTypeForm,new TypeDefinitionScope(Nil,tscope.asInstanceOf[Module]))
      val expr = processExpression(cast.getExpression,scope)
      new BitcastExpression(expr,tau)
    }
  }
  def processExp2(exp: PExp2,scope: LexicalScope): Expression = exp match {
    case minus: AMinusExp2 => new ArithmeticOperatorExpression('-',new IntegerLiteralExpression(0),processExp2(minus.getExp2,scope))
    case others: AOthersExp2 => processExp1(others.getExp1,scope)
  }
  def processExp3(exp: PExp3,scope: LexicalScope): Expression = exp match {
    case mult: AMultiplyExp3 => new ArithmeticOperatorExpression('*',processExp3(mult.getExp1,scope),processExp2(mult.getExp2,scope))
    case divide: ADivisionExp3 => new ArithmeticOperatorExpression('/',processExp3(divide.getExp1,scope),processExp2(divide.getExp2,scope))
    case others: AOthersExp3 => processExp2(others.getExp2,scope)
  }
  def processExp4(exp: PExp4,scope: LexicalScope): Expression = exp match {
    case add: APlusExp4 => new ArithmeticOperatorExpression('+',processExp4(add.getExp1,scope),processExp3(add.getExp2,scope))
    case sub: AMinusExp4 => new ArithmeticOperatorExpression('-',processExp4(sub.getExp1,scope),processExp3(sub.getExp2,scope))
    case others: AOthersExp4 => processExp3(others.getExp3,scope)
  }
  def processExp5(exp: PExp5,scope: LexicalScope): Expression = exp match {
    case greater: AGreaterExp5 => new ComparisonExpression(OrdinalComparison(true,false),processExp4(greater.getExp1,scope),processExp4(greater.getExp2,scope))
    case greatereq: AGreatereqExp5 => new ComparisonExpression(OrdinalComparison(true,true),processExp4(greatereq.getExp1,scope),processExp4(greatereq.getExp2,scope))
    case lesser: ALessExp5 => new ComparisonExpression(OrdinalComparison(false,false),processExp4(lesser.getExp1,scope),processExp4(lesser.getExp2,scope))
    case lessereq: ALessereqExp5 => new ComparisonExpression(OrdinalComparison(false,true),processExp4(lessereq.getExp1,scope),processExp4(lessereq.getExp2,scope))
    case equals: AEqualsExp5 => new ComparisonExpression(IdentityComparison(true),processExp4(equals.getExp1,scope),processExp4(equals.getExp2,scope))
    case different: ADifferentExp5 => new ComparisonExpression(IdentityComparison(false),processExp4(different.getExp1,scope),processExp4(different.getExp2,scope))
    case others: AOthersExp5 => processExp4(others.getExp4,scope)
  }
  def processBlockSteps(contents: LinkedList[PBlockStep]): List[PExpression] = convertList(contents).map(step => step.asInstanceOf[ABlockStep].getExpression)
  def processBlockContents(contents: PBlockExpression): List[PExpression] = contents match {
    case one:  AOneBlockExpression => List(one.getExpression)
    case many: AManyBlockExpression => processBlockSteps(many.getBlockStep)
  }
  
  def processBlock(expr: PBlockExpression,scope: LexicalScope): BlockExpression = {
    val exprs: List[Expression] = processBlockContents(expr).map(processExpression(_,scope))
    new BlockExpression(exprs)
  }
  
  def processExpression(expression: PExpression,scope: LexicalScope): Expression = expression match {
    case assignment: AAssignmentexpExpression => {
      val left = processExp1(assignment.getExp1,scope).asInstanceOf[WritableExpression]
      val right = processExpression(assignment.getExpression,scope)
      new AssignmentExpression(left,right)
    }
    case blockexp: ABlockexpExpression => processBlock(blockexp.getBlockExpression,scope)
    case exp5: AOthersExpression => processExp5(exp5.getExp5,scope)
    case ifthen: AIfwithoutelseexpExpression => processIfThen(ifthen,scope)
    case ifelse: AIfwithelseexpExpression => processIfElse(ifelse,scope)
  }
  
  def processClassDefinition(classDef: PClassDefinition, scope: Module): ClassDefinition = classDef match {
    case variant: AVariantClassDefinition => {
      val isFinal = variant.getFinal != null
      val tscope = new TypeDefinitionScope(Nil, scope)
      val parent: Option[ClassDefinition] = Option.apply(variant.getExtensionClause).map((ext: PExtensionClause) => processExtensionClause(ext, scope))
      val root: ClassDefinition = new ClassDefinition(scope, variant.getUnqualifiedIdentifier.getText, Nil, Nil, parent, Nil, Nil)
      for(varCase <- scala.collection.JavaConversions.collectionAsScalaIterable(variant.getVariantCase)) {
        val name = varCase.asInstanceOf[AVariantCase].getUnqualifiedIdentifier.getText
        val args: List[RecordMember] = Option.apply(varCase.asInstanceOf[AVariantCase].getVariantCaseParameters).map(vcp => 
          processTupleComponents(vcp.asInstanceOf[AVariantCaseParameters].getTupleComponentList, tscope)) getOrElse Nil
        new DataConstructorDefinition(scope, name, args.map(arg => (arg.name.get, arg.mutable, arg.tau)), root)
      }
      root
    }
  }
  
  def processModuleDefinition(amoddef: AModuledefDefinition,scope: Module): Module = {
    val moddef: AModuleDefinition = amoddef.getModuleDefinition() match {case real: AModuleDefinition => real}
    val result = new Module(moddef.getName().getText(),scope)
    convertList(moddef.getImports()).foreach(imp => declareImportDeclaration(result,imp))
    convertList(moddef.getDefinitions).foreach(definition => processDefinition(definition,result))
    result
  }
  
  def processDefinition(adef: PDefinition,scope: Module): Definition = adef match {
    case amoddef: AModuledefDefinition => processModuleDefinition(amoddef,scope)
    case afuncdef: AFundefDefinition => processFunctionDefinition(afuncdef.getFunctionDefinition(),scope)
    case atypedef: ATypedefDefinition => {
      val name = atypedef.getUnqualifiedIdentifier.getText
      val params = if(atypedef.getParameters != null)
        processTypeParameters(atypedef.getParameters.asInstanceOf[ATypeFormArguments].getArguments)
      else
        Nil
      val tscope = new TypeDefinitionScope(params,scope)
      val tparams: List[TypeVariable] = tscope.bindings.map(_.tau.asInstanceOf[TypeVariable]).toList
      val alpha = new TypeVariable(false,Some(name))
      tscope.bind(name,Some(alpha))
      val sigma = processTypeForm(atypedef.getTypeForm,tscope)
      new TypeDefinition(new TypeExpressionConstructor(tparams, sigma), name, scope)
    }
    case avardef: AGlobaldefDefinition => {
      val slot = processSlotDeclaration(avardef.getSlotDeclaration,new TypeDefinitionScope(Nil,scope))
      val initializer = processExpression(avardef.getExpression,new LexicalScope(scope,Nil)).asInstanceOf[ConstantExpression]
      new VariableDefinition(scope,slot._1,initializer,slot._2,slot._3)
    }
  }
}
