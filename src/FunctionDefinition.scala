package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder
import jllvm.LLVMReturnInstruction
import jllvm.LLVMIntegerToFloatCast
import jllvm.LLVMExtendCast

class UninferredFunction(m: Module,n: String,args: List[Tuple2[String,UninferredArgument]],r: Option[TauType],b: (UninferredLexicalScope) => UninferredBlock) {
  val name: String = n
  val scope: Module = m
  val fScope = new UninferredLexicalScope(scope,args)
  val arguments = fScope.bindings
  val body = b(fScope)
  val functionType = new FunctionArrow(arguments.map(arg => arg.variableType),r match { case Some(tau) => tau case None => new TauVariable })
  
  def infer: ExpressionFunctionDefinition = {
    val rui = new RangeUnificationInstance(Some(scope))
    rui.constrain(new LesserEq(body.expressionType,functionType.range))
    body.constrain(rui)
    new ExpressionFunctionDefinition(this,rui.solve)
  }
}

trait FunctionDefinition extends Definition {
  val functionType: SigmaType
  
  def specializeScope(caller: Scope[_]): SigmaType
  def specialize(specialization: List[GammaType]): SpecializedFunction
  def specialized: Iterable[SpecializedFunction]
}

class ExpressionFunctionDefinition(original: UninferredFunction,substitution: TauSubstitution) extends FunctionDefinition {
  override val name: String = original.name
  override val scope: Module = {original.scope.define(this) ; original.scope}
  override val functionType: SigmaType = original.functionType.generalize(substitution)
  val fScope = original.fScope.infer(substitution)
  val body = original.body.substitute(substitution)
  
  protected val specializations: Map[List[GammaType],SpecializedExpressionFunction] = new HashMap[List[GammaType],SpecializedExpressionFunction]()
  
  functionType match {
    case arrow: FunctionArrow => specialize(Nil)
    case _ => {}
  }
  
  override def specialized = specializations.values.map(func => func.asInstanceOf[SpecializedFunction])
  
  override def specializeScope(caller: Scope[_]): SigmaType = functionType match {
    case beta: BetaType => beta.scopeMap(fScope => fScope match {
      case args: ArgumentScopeType => new ArgumentScopeType(args.function,Some(caller match {
        case mod: Module => new GlobalScopeType(Some(mod))
        case lexical: UninferredLexicalScope => new LexicalScopeType(lexical)
      }))
      case _ => fScope
    })
    case arrow: FunctionArrow => arrow.scopeMap(fScope => fScope match {
      case args: ArgumentScopeType => new ArgumentScopeType(args.function,Some(caller match {
        case mod: Module => new GlobalScopeType(Some(mod))
        case lexical: UninferredLexicalScope => new LexicalScopeType(lexical)
      }))
      case _ => fScope
    })
    case _ => throw new Exception("Why does a function have something other than an arrow type or generalized arrow type?")
  }
  
  override def specialize(specialization: List[GammaType]): SpecializedExpressionFunction = specializations.get(specialization) match {
    case Some(sf) => sf
    case None => {
      val specializer = functionType.specialize(specialization)
      val result = new SpecializedExpressionFunction(this,specializer)
      specializations.put(specialization,result)
      result
    }
  }
}

trait SpecializedFunction {
  val functionType: FunctionArrow
  
  def compile(builder: LLVMInstructionBuilder): LLVMFunction
}

class SpecializedExpressionFunction(org: ExpressionFunctionDefinition,specializer: BetaSpecialization) extends SpecializedFunction {
  val original = org
  val name: String = org.name
  val functionType: FunctionArrow = specializer.solve(original.functionType.body) match {
    case frho: FunctionArrow => frho
    case _ => throw new Exception("Specializing a function's type should never yield anything but an arrow type.")
  }
  val function = new LLVMFunction(original.scope.compiledModule,name + functionType.toString,functionType.compile)
  protected var compiled: Boolean = false
  val fScope = org.fScope.specialize(specializer,Some(function.getParameters.toList))
  val body = original.body.specialize(specializer)
  
  def compile(builder: LLVMInstructionBuilder): LLVMFunction = {
    if(compiled == false) {
      val entry = function.appendBasicBlock("entry")
      builder.positionBuilderAtEnd(entry)
      fScope.compile(builder)
      val result = body.compile(builder,fScope)
      //THIS KLUDGE IS BAD, AND I SHOULD FEEL BAD.
      //WHAT I REALLY NEED IS A "CAST" OPERATION IMPLEMENTED AS PART OF MY ORDERING ON TYPES.
      val castedResult = functionType.range match {
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
      compiled = true
    }
    function
  }
}
