package decac

import jllvm._
import jllvm.llvm._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

class UninferredIf(possibilities: List[Tuple2[UninferredExpression,UninferredExpression]],scope: Scope) extends UninferredExpression(new TauVariable,scope) {
  val cases: List[Tuple2[UninferredExpression,UninferredExpression]] = possibilities
  override def children = cases.map(ifcase => ifcase._2)
  override def substitute(substitution: SigmaSubstitution,generalize: Boolean): IfExpression = {
    val conditions = cases.map(ifcase => ifcase._1.substitute(substitution,false))
    val results = cases.map(ifcase => ifcase._2.substitute(substitution,false))
    new IfExpression(conditions.zip(results),substituteTypes(substitution,generalize)._1,scope)
  }
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    cases.map(ifcase => { rui.constrain(new Equal(ifcase._1.expressionType,BooleanRho)); rui.constrain(new LesserEq(ifcase._2.expressionType,expressionType)) })
    return rui
  }
}

class IfExpression(possibilities: List[Tuple2[Expression,Expression]],overallType: SigmaType,scope: Scope) extends Expression(overallType,scope) {
  val cases: List[Tuple2[Expression,Expression]] = possibilities
  override def children = cases.map(ifcase => ifcase._2)
  val specializations: Map[List[RhoType],SpecializedIf] = new HashMap[List[RhoType],SpecializedIf]()
  override def specialize(specialization: List[RhoType]): SpecializedIf = {
    if(specialization.length == expressionType.countUniversals)
      specializations.get(specialization) match {
        case Some(sb) => sb
        case None => {
          val resultCases = cases.map(ifcase => (ifcase._1.specialize(specialization),ifcase._2.specialize(specialization)))
          val specializedType: RhoType = expressionType match {
            case forall: ForallSigma => forall.specialize(specialization)
            case rho: RhoType => rho
            case _ => throw new Exception("Cannot specialize expression whose principal type is neither a universally-quantified type nor a rho type.")
          }
          val result = new SpecializedIf(resultCases,specializedType,scope)
          specializations.put(specialization,result)
          return result
        }
      }
    else
      throw new Exception("Cannot specialize if expression with list of rho types whose length differs from the number of universally-quantified type variables.")
  }
}

class SpecializedIf(possibilities: List[Tuple2[SpecializedExpression,SpecializedExpression]],overallType: RhoType,scope: Scope) extends SpecializedExpression(overallType,scope) {
  val cases: List[Tuple2[SpecializedExpression,SpecializedExpression]] = possibilities
  override def children = cases.map(ifcase => ifcase._2)
  def compileCases(builder: LLVMInstructionBuilder,
                   ifcases: List[Tuple2[SpecializedExpression,SpecializedExpression]],
                   compiled: List[Tuple2[LLVMValue,LLVMBasicBlock]],
                   mergeBlock: LLVMBasicBlock,
                   mergeType: LLVMType): LLVMValue = ifcases match {
    case Nil => {
      builder.positionBuilderAtEnd(mergeBlock)
      val phi = new LLVMPhiNode(builder,mergeType,"ifphi")
      val caseValues: Array[LLVMValue] = compiled.map(compiledCase => compiledCase._1).toArray
      val caseBlocks: Array[LLVMBasicBlock] = compiled.map(compiledCase => compiledCase._2).toArray
      phi.addIncoming(caseValues,caseBlocks)
      phi
    }
    case ifcase :: rest => {
      //val decaTrue = GlobalScope.lookup("true")
      val condition = new LLVMIntegerComparison(builder,LLVMIntPredicate.LLVMIntEQ,ifcase._1.compile(builder),LLVMConstantInteger.constantInteger(new LLVMIntegerType(1),1,false),"condition")
      val thenBlock = mergeBlock.insertBasicBlockBefore("then")
      val elseBlock = mergeBlock.insertBasicBlockBefore("else")
      val ifBranch = new LLVMBranchInstruction(builder,condition,thenBlock,elseBlock)
      builder.positionBuilderAtEnd(thenBlock)
      val caseValue = ifcase._2.compile(builder)
      val mergeBranch = new LLVMBranchInstruction(builder,mergeBlock)
      val finalThenBlock = builder.getInsertBlock
      builder.positionBuilderAtEnd(elseBlock)
      compileCases(builder,rest,compiled ++ List((caseValue,finalThenBlock)),mergeBlock,mergeType)
    }
  }

  override def compile(builder: LLVMInstructionBuilder): LLVMValue = {
    val function = builder.getInsertBlock.getParent
    val mergeBlock = function.appendBasicBlock("merge")
    val mergeType = expressionType.compile
    compileCases(builder,cases,Nil,mergeBlock,mergeType)
  }
}
