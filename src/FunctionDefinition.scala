package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import jllvm.llvm.LLVMLinkage
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder
import jllvm.LLVMReturnInstruction
import jllvm.LLVMIntegerToFloatCast
import jllvm.LLVMExtendCast

trait FunctionDefinition extends Definition {
  def signature: SigmaType
  
  def specializeScope(caller: Scope[_]): FunctionArrow
  def specialize(specialization: List[GammaType]): SpecializedFunction
  def specialized: Iterable[SpecializedFunction]
}

trait SpecializedFunction {
  val signature: FunctionArrow
  val generic: FunctionDefinition
  
  def compile: LLVMFunction
}

class ExpressionFunction(s: TypeBindingScope,n: String,args: List[Tuple2[String,UninferredArgument]],r: Option[TauType],b: (UninferredLexicalScope) => UninferredBlock) extends FunctionDefinition {
  override val name: String = n
  override val scope: Module = { s.owner.define(this) ; s.owner }
  protected var inferred: Option[GeneralizedExpressionFunction] = None
  val uninferred = new UninferredExpressionFunction(new UninferredLexicalScope(scope,args),r match { case Some(tau) => tau case None => new TauVariable })
  uninferred.generateBody(b)

  def infer: GeneralizedExpressionFunction = {
    val rui = new RangeUnificationInstance(Some(scope))
    if(uninferred.signature.range != UnitGamma)
      rui.constrain(new LesserEq(uninferred.body.expressionType,uninferred.signature.range))
    uninferred.body.constrain(rui)
    uninferred.body.check(rui)
    val result = new GeneralizedExpressionFunction(uninferred,rui.solve)
    inferred = Some(result)
    result.signature match {
      case gammaArrow: FunctionArrow => result.specialize(this,Nil)
      case beta: BetaType => {
        val arrow = beta.body.asInstanceOf[FunctionArrow]
	arrow.range match {
          case rho: RhoType => assert(rho.filter(tau => tau.isInstanceOf[BetaVariable]).forall(bvar => arrow.domain.find(arg => arg match { case rhoarg: RhoType => rho.filter(t => t == bvar) != None case _ => arg == bvar }) != None))
	  case _ => {}
	}
      }
    }
    result
  }
  
  override def signature: SigmaType = inferred match {
    case Some(func) => func.signature
    case None => uninferred.signature
  }
  
  override def specializeScope(caller: Scope[_]): FunctionArrow = inferred match {
    case Some(general) => general.specializeScope(caller)
    case None => {
      val general = infer
      general.specializeScope(caller)
    }
  }
  
  override def specialize(specialization: List[GammaType]): SpecializedFunction = inferred match {
    case Some(general) => general.specialize(this,specialization)
    case None => {
      val general = infer
      general.specialize(this,specialization)
    }
  }
  
  override def specialized: Iterable[SpecializedFunction] = inferred match {
    case Some(general) => general.specialized
    case None => {
      val general = infer
      general.specialized
    }
  }
}

class UninferredExpressionFunction(fScope: UninferredLexicalScope,range: TauType) {
  val scope = fScope
  val arguments = fScope.bindings
  val signature = new FunctionArrow(arguments.map(arg => arg.variableType),range)
  protected var bodyBlock: Option[UninferredBlock] = None
  def generateBody(genBody: (UninferredLexicalScope) => UninferredBlock): Unit = {
    bodyBlock = Some(genBody(scope))
  }
  def body: UninferredBlock = bodyBlock.get
}

class GeneralizedExpressionFunction(uninferred: UninferredExpressionFunction,substitution: TauSubstitution) {
  val signature: SigmaType = uninferred.signature.generalize(substitution)
  val scope = uninferred.scope.infer(substitution)
  val body = uninferred.body.substitute(substitution)
  protected val specializations = new HashMap[List[GammaType],SpecializedExpressionFunction]()
  
  def specializeScope(caller: Scope[_]): FunctionArrow = {
    assert(signature.body.isInstanceOf[FunctionArrow])
    signature.body.asInstanceOf[FunctionArrow].scopeMap(fScope => fScope match {
      case args: ArgumentScopeType => new ArgumentScopeType(args.function,Some(caller.scopeType))
      case _ => fScope
    }).asInstanceOf[FunctionArrow]
  }
  
  def specialize(definition: ExpressionFunction,specialization: List[GammaType]): SpecializedExpressionFunction = {
    for((key,value) <- specializations)
      if(specialization.zip(key).forall(pair => TauOrdering.equiv(pair._1,pair._2)))
        return value
    val specializer = signature.specialize(specialization)
    val result = new SpecializedExpressionFunction(definition,this,specializer)
    specializations.put(specialization,result)
    result.specializeBody
    result
  }

  def specialized: Iterable[SpecializedFunction] = specializations.values.map(func => func.asInstanceOf[SpecializedFunction])
}

class SpecializedExpressionFunction(definition: ExpressionFunction,general: GeneralizedExpressionFunction,specializer: BetaSpecialization) extends SpecializedFunction {
  override val generic = definition
  override val signature: FunctionArrow = specializer.solve(general.signature.body) match {
    case frho: FunctionArrow => frho
    case _ => throw new Exception("Specializing a function's type should never yield anything but an arrow type.")
  }
  val function = {
    val f = new LLVMFunction(definition.scope.compiledModule,definition.name + signature.toString,signature.compile)
    if(!specializer.isEmpty)
      f.setLinkage(LLVMLinkage.LLVMWeakODRLinkage)
    f
  }
  protected var compiled: Boolean = false
  val fScope = general.scope.specialize(specializer,Some(function.getParameters.toList))
  protected var bodyBlock: Option[SpecializedBlock] = None
  def specializeBody: Unit = {
    bodyBlock = Some(general.body.specialize(specializer))
  }
  def body: SpecializedBlock = bodyBlock.get
  
  override def compile: LLVMFunction = {
    if(compiled == false) {
      //Make this assignment here at the top to prevent infinite recursion when compiling recursive functions.
      compiled = true
      val entry = function.appendBasicBlock("entry")
      val builder = new LLVMInstructionBuilder
      builder.positionBuilderAtEnd(entry)
      fScope.compile(builder)
      val result = (new ImplicitUpcast(body,signature.range.asInstanceOf[GammaType])).compile(builder,fScope)
      new LLVMReturnInstruction(builder,result)
    }
    function
  }
}
