package decac

import scala.collection.mutable.HashMap
import jllvm._
import jllvm.llvm.LLVMIntPredicate

abstract class UninferredCall(arrow: ArrowType,arguments: List[UninferredExpression]) extends UninferredExpression(arrow.range) {
  assert(arrow.domain.length == arguments.length)
  val signature = arrow
  override def children: List[UninferredExpression] = arguments
  override def substitute(substitution: TauSubstitution): CallExpression
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    children.map(child => child.constrain(rui))
    for(pair <- signature.domain.zip(arguments))
      rui.constrain(new LesserEq(pair._2.expressionType,pair._1))
    rui
  }
}

class UninferredDefinitionCall(func: FunctionDefinition,arguments: List[UninferredExpression]) extends UninferredCall(func.signature.freshlyInstantiate.asInstanceOf[FunctionArrow],arguments) {
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
  assert(arrow.filter(tau => if(tau.isInstanceOf[TauVariable]) !tau.isInstanceOf[BetaVariable] else false) == Nil)
  val signature = arrow
  val arguments = args
  
  override def children: List[Expression] = arguments
  override def specialize(specialization: BetaSpecialization): SpecializedCall
}

class DefinitionCall(func: FunctionDefinition,sig: FunctionArrow,arguments: List[Expression]) extends CallExpression(sig,arguments) {
  val definition = func
  val specializations = new HashMap[BetaSpecialization,SpecializedDefinitionCall]
  
  override def specialize(specialization: BetaSpecialization): SpecializedDefinitionCall = specializations.get(specialization) match {
    case Some(spec) => spec
    case None => {
      val args = arguments.map(expr => expr.specialize(specialization))
      val typeArgs = args.map(arg => arg.expressionType).zip(definition.signature.body.asInstanceOf[FunctionArrow].domain).filter(pair => pair._2.isInstanceOf[BetaVariable]).map(pair => pair._1)
      val specFunc = definition.specialize(typeArgs)
      val result = new SpecializedDefinitionCall(specFunc,args)
      specializations.put(specialization,result)
      result
    }
  }
}

class ExpressionCall(func: Expression,args: List[Expression]) extends CallExpression(func.expressionType.asInstanceOf[ClosureArrow].signature,args) {
  val function = func
  override def children: List[Expression] = function :: arguments
  val specializations = new HashMap[BetaSpecialization,SpecializedExpressionCall]
  
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

class SpecializedDefinitionCall(func: SpecializedFunction,args: List[SpecializedExpression]) extends SpecializedCall(func.signature,args) {
  val function = func
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMCallInstruction = {
    val args = arguments.map(arg => arg.compile(builder,scope)).toArray
    val block = builder.getInsertBlock
    val func = function.compile(builder)
    builder.positionBuilderAtEnd(block)
    val call = new LLVMCallInstruction(builder,func,args,"call")
    //Use the LLVM compiling infrastructure to check for tail-calls.  Free tail-call optimization!
    call.setTailCall(block.getParent.getInstance == func.getInstance)
    call
  }
}

class SpecializedExpressionCall(func: SpecializedExpression,args: List[SpecializedExpression]) extends SpecializedCall(func.expressionType.asInstanceOf[ClosureArrow].signature,args) {
  val function = func
  override def children: List[SpecializedExpression] = function :: arguments
  
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = {
    val func = function.compile(builder,scope)
    val args = arguments.map(arg => arg.compile(builder,scope))
    val closureType = function.expressionType.asInstanceOf[ClosureArrow].representation.get
    closureType.sumCases match {
      case plain :: environment :: Nil => {
        val mergeBlock = builder.getInsertBlock.getParent.appendBasicBlock("tag_merge")
        val mergeType = signature.range.asInstanceOf[GammaType].compile
        val plainBlock = mergeBlock.insertBasicBlockBefore("tag_plain")
        val varyingBlock = mergeBlock.insertBasicBlockBefore("tag_varying")
        //This needs to be rearranged into a switch instruction.
        val comparator = new LLVMExtractValueInstruction(builder,func,0,"variant_tag")
        val variant = new LLVMExtractValueInstruction(builder,func,1,"variant_body")
        val condition = new LLVMIntegerComparison(builder,LLVMIntPredicate.LLVMIntEQ,comparator,LLVMConstantInteger.constantInteger(closureType.tagRepresentation,plain.constructor,false),"tag_check")
        new LLVMBranchInstruction(builder,condition,plainBlock,varyingBlock)
        
        //Emit code for the plain branch, the one without a closure environment.
        builder.positionBuilderAtEnd(plainBlock)
        //Cast the variant to the type we now know it has, and take its first element to get the function pointer.
        val casted = new LLVMBitCast(builder,variant,closureType.caseRepresentation(plain).asInstanceOf[LLVMStructType],"plain_cast")
        val funcPointer = new LLVMExtractValueInstruction(builder,casted,0,"plain_function_pointer")
        val plainCall = new LLVMCallInstruction(builder,funcPointer,args.toArray,"plain_call")
        new LLVMBranchInstruction(builder,mergeBlock)
        
        //Emit code for the varying branch, the one with a closure environment.
        builder.positionBuilderAtEnd(varyingBlock)
        //Cast the variant to the type we now know it has, and take its first element to get the function pointer, then its second element's address for the environment.
        val varyingCasted = new LLVMBitCast(builder,variant,closureType.caseRepresentation(environment).asInstanceOf[LLVMStructType],"varying_cast")
        val closurePointer = new LLVMExtractValueInstruction(builder,varyingCasted,0,"varying_function_pointer")
        val closureEnv = new LLVMExtractValueInstruction(builder,varyingCasted,1,"varying_environment")
        val envPointer = new LLVMStackAllocation(builder,closureEnv.typeOf,LLVMConstantInteger.constantInteger(new LLVMIntegerType(1),1,false),"environment_pointer")
        new LLVMStoreInstruction(builder,closureEnv,envPointer)
        val varyingCall = new LLVMCallInstruction(builder,closurePointer,(envPointer :: args).toArray,"varying_call")
        new LLVMInsertValueInstruction(builder,varyingCasted,new LLVMLoadInstruction(builder,envPointer,"varying_load"),1,"varying_insert")
        new LLVMBranchInstruction(builder,mergeBlock)
        
        //Emit code to merge the two cases back together.
        builder.positionBuilderAtEnd(mergeBlock)
        val phi = new LLVMPhiNode(builder,mergeType,"closure_phi")
        phi.addIncoming((plainCall :: varyingCall :: Nil).toArray,(plainBlock :: varyingBlock :: Nil).toArray)
        phi
      }
      case _ => throw new Exception("Ill-formed closure type " + closureType)
    }
  }
}
