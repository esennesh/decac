package decac

import scala.collection.mutable.HashMap
import jllvm.LLVMValue
import jllvm.LLVMCallInstruction
import jllvm.LLVMInstructionBuilder

abstract class UninferredCall(arrow: ArrowType,arguments: List[UninferredExpression]) extends UninferredExpression(arrow.range) {
  assert(arrow.domain.length == arguments.length)
  val signature = arrow
  override def children: List[UninferredExpression] = arguments
  override def substitute(substitution: TauSubstitution): CallExpression
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    for(pair <- signature.domain.zip(arguments))
      rui.constrain(new LesserEq(pair._2.expressionType,pair._1))
    rui
  }
}

class UninferredDefinitionCall(func: FunctionDefinition,arguments: List[UninferredExpression]) extends UninferredCall(func.functionType.freshlyInstantiate.asInstanceOf[FunctionArrow],arguments) {
  val definition = func
  
  override def substitute(substitution: TauSubstitution): DefinitionCall = {
    val substitutedSignature = substitution.solve(signature.asInstanceOf[FunctionArrow]).asInstanceOf[FunctionArrow]
    new DefinitionCall(definition,substitutedSignature,substituteTypes(substitution)._2)
  }
}

class UninferredExpressionCall(func: UninferredExpression,arguments: List[UninferredExpression]) extends UninferredCall(new ClosureArrow(arguments.map(tau => new TauVariable),new TauVariable,None),arguments) {
  val function = func
  override def children: List[UninferredExpression] = function :: arguments
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    super.constrain(rui)
    val sigType = signature match {
      case fpsig: FunctionArrow => fpsig
      case closure: ClosureArrow => closure
    }
    rui.constrain(new Equal(function.expressionType,sigType))
    rui
  }
  
  override def substitute(substitution: TauSubstitution): ExpressionCall = {
    val substitutedFunction = function.substitute(substitution)
    val substitutedArguments = substituteTypes(substitution)._2
    new ExpressionCall(substitutedFunction,substitutedArguments)
  }
}

abstract class CallExpression(arrow: FunctionArrow,args: List[Expression]) extends Expression(arrow.range) {
  assert(arrow.filter(tau => tau.isInstanceOf[BetaVariable]) == arrow.filter(tau => tau.isInstanceOf[TauVariable]))
  val signature = arrow
  val arguments = args
  val specializations: HashMap[BetaSpecialization,SpecializedCall]
  
  override def children: List[Expression] = arguments
  override def specialize(specialization: BetaSpecialization): SpecializedCall
}

class DefinitionCall(func: FunctionDefinition,sig: FunctionArrow,arguments: List[Expression]) extends CallExpression(sig,arguments) {
  val definition = func
  override val specializations = new HashMap[BetaSpecialization,SpecializedDefinitionCall]
  
  override def specialize(specialization: BetaSpecialization): SpecializedDefinitionCall = specializations.get(specialization) match {
    case Some(spec) => spec
    case None => {
      val typeParams = sig.filter(tau => tau.isInstanceOf[BetaVariable]).map(beta => specialization.solve(beta))
      val specFunc = definition.specialize(typeParams)
      val result = new SpecializedDefinitionCall(specFunc,arguments.map(expr => expr.specialize(specialization)))
      specializations.put(specialization,result)
      result
    }
  }
}

class ExpressionCall(func: Expression,args: List[Expression]) extends CallExpression(new FunctionArrow(func.expressionType.asInstanceOf[ArrowType].domain,func.expressionType.asInstanceOf[ArrowType].range),args) {
  val function = func
  override def children: List[Expression] = function :: arguments
  override val specializations = new HashMap[BetaSpecialization,SpecializedExpressionCall]
  
  override def specialize(specialization: BetaSpecialization): SpecializedExpressionCall = specializations.get(specialization) match {
    case Some(spec) => spec
    case None => {
      val args = arguments.map(arg => arg.specialize(specialization))
      val func = function.specialize(specialization)
      new SpecializedExpressionCall(func,args)
    }
  }
}

abstract class SpecializedCall(arrow: FunctionArrow,args: List[SpecializedExpression]) extends SpecializedExpression(arrow.range.asInstanceOf[GammaType]) {
  assert(arrow.filter(tau => tau.isInstanceOf[TauVariable]) == Nil)
  val signature = arrow
  val arguments = args
  override def children: List[SpecializedExpression] = args
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue
}

class SpecializedDefinitionCall(func: SpecializedFunction,args: List[SpecializedExpression]) extends SpecializedCall(func.functionType,args) {
  val function = func
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMCallInstruction = {
    val args = arguments.map(arg => arg.compile(builder,scope)).toArray
    val block = builder.getInsertBlock
    val func = function.compile(builder)
    builder.positionBuilderAtEnd(block)
    val result = new LLVMCallInstruction(builder,func,args,"call")
    //Use the LLVM compiling infrastructure to check for tail-calls.  Free tail-call optimization!
    result.setTailCall(block.getParent.getInstance == func.getInstance)
    result
  }
}
