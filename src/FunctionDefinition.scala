package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
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
  
  def compile(builder: LLVMInstructionBuilder): LLVMFunction
}

class ExpressionFunction(m: Module,n: String,args: List[Tuple2[String,UninferredArgument]],r: Option[TauType],b: (UninferredLexicalScope) => UninferredBlock) extends FunctionDefinition {
  override val name: String = n
  override val scope: Module = { m.define(this) ; m }
  protected var inferred: Option[GeneralizedExpressionFunction] = None
  val uninferred = new UninferredExpressionFunction(new UninferredLexicalScope(scope,args),r match { case Some(tau) => tau case None => new TauVariable })
  uninferred.generateBody(b)

  def infer: GeneralizedExpressionFunction = {
    val rui = new RangeUnificationInstance(Some(scope))
    rui.constrain(new LesserEq(uninferred.body.expressionType,uninferred.signature.range))
    uninferred.body.constrain(rui)
    val result = new GeneralizedExpressionFunction(uninferred,rui.solve)
    inferred = Some(result)
    result.signature match {
      case gammaArrow: FunctionArrow => result.specialize(this,Nil)
      case _ => {}
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
    bodyBlock = Some(genBody(fScope))
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
  def specialize(definition: ExpressionFunction,specialization: List[GammaType]): SpecializedFunction = specializations.get(specialization) match {
    case Some(sf) => sf
    case None => {
      val specializer = signature.specialize(specialization)
      val result = new SpecializedExpressionFunction(definition,this,specializer)
      specializations.put(specialization,result)
      result.specializeBody
      result
    }
  }
  def specialized: Iterable[SpecializedFunction] = specializations.values.map(func => func.asInstanceOf[SpecializedFunction])
}

class SpecializedExpressionFunction(definition: ExpressionFunction,general: GeneralizedExpressionFunction,specializer: BetaSpecialization) extends SpecializedFunction {
  override val signature: FunctionArrow = specializer.solve(general.signature.body) match {
    case frho: FunctionArrow => frho
    case _ => throw new Exception("Specializing a function's type should never yield anything but an arrow type.")
  }
  val function = new LLVMFunction(definition.scope.compiledModule,definition.name + signature.toString,signature.compile)
  protected var compiled: Boolean = false
  val fScope = general.scope.specialize(specializer,Some(function.getParameters.toList))
  protected var bodyBlock: Option[SpecializedBlock] = None
  def specializeBody: Unit = {
    bodyBlock = Some(general.body.specialize(specializer))
  }
  def body: SpecializedBlock = bodyBlock.get
  
  override def compile(builder: LLVMInstructionBuilder): LLVMFunction = {
    if(compiled == false) {
      //Make this assignment here at the top to prevent infinite recursion when compiling recursive functions.
      compiled = true
      val entry = function.appendBasicBlock("entry")
      builder.positionBuilderAtEnd(entry)
      fScope.compile(builder)
      val result = body.compile(builder,fScope)
      //THIS KLUDGE IS BAD, AND I SHOULD FEEL BAD.
      //WHAT I REALLY NEED IS A "CAST" OPERATION IMPLEMENTED AS PART OF MY ORDERING ON TYPES.
      //Actually, what I really need is an implicit-cast expression that upcasts subtypes to the expected supertypes.
      val castedResult = signature.range match {
        case real: RealGamma => {
          val doubled = body.expressionType match {
            case unsigned: UnsignedIntegerGamma => new LLVMIntegerToFloatCast(builder,result,DoubleGamma.compile,"cast",LLVMIntegerToFloatCast.IntCastType.UNSIGNED)
            case signed: IntegerGamma => new LLVMIntegerToFloatCast(builder,result,DoubleGamma.compile,"cast",LLVMIntegerToFloatCast.IntCastType.SIGNED)
            case floating: RealGamma => result
          }
          new LLVMExtendCast(LLVMExtendCast.ExtendType.FLOAT,builder,doubled,real.compile,"cast")
        }
        case _ => result
      }
      new LLVMReturnInstruction(builder,castedResult)
    }
    function
  }
}
