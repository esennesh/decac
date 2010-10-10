package decac

import jllvm._
import jllvm.llvm._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

class UninferredIf(possibilities: List[Tuple2[UninferredExpression,UninferredExpression]]) extends UninferredExpression(new TauVariable) {
  val cases: List[Tuple2[UninferredExpression,UninferredExpression]] = possibilities
  override def children = cases.map(ifcase => ifcase._2)
  override def substitute(substitution: TauSubstitution): IfExpression = {
    val conditions = cases.map(ifcase => ifcase._1.substitute(substitution))
    val results = cases.map(ifcase => ifcase._2.substitute(substitution))
    new IfExpression(conditions.zip(results),substituteTypes(substitution)._1)
  }
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    cases.map(ifcase => { rui.constrain(new Equal(ifcase._1.expressionType,BooleanGamma)); rui.constrain(new LesserEq(ifcase._2.expressionType,expressionType)) })
    return rui
  }
}

class IfExpression(possibilities: List[Tuple2[Expression,Expression]],overallType: TauType) extends Expression(overallType) {
  val cases: List[Tuple2[Expression,Expression]] = possibilities
  override def children = cases.map(ifcase => ifcase._2)
  val specializations: Map[BetaSpecialization,SpecializedIf] = new HashMap[BetaSpecialization,SpecializedIf]()
  override def specialize(specialization: BetaSpecialization): SpecializedIf = specializations.get(specialization) match {
    case Some(si) => si
    case None => {
      val resultCases = cases.map(ifcase => (ifcase._1.specialize(specialization),ifcase._2.specialize(specialization)))
      val result = new SpecializedIf(resultCases,specialization.solve(expressionType))
      specializations.put(specialization,result)
      result
    }
  }
}

class SpecializedIf(possibilities: List[Tuple2[SpecializedExpression,SpecializedExpression]],overallType: GammaType) extends SpecializedExpression(overallType) {
  val cases: List[Tuple2[SpecializedExpression,SpecializedExpression]] = possibilities
  override def children = cases.map(ifcase => ifcase._2)
  def compileCases(builder: LLVMInstructionBuilder,
                   ifcases: List[Tuple2[SpecializedExpression,SpecializedExpression]],
                   compiled: List[Tuple2[LLVMValue,LLVMBasicBlock]],
                   mergeBlock: LLVMBasicBlock,
                   mergeType: LLVMType,
                   scope: Scope[_]): LLVMValue = ifcases match {
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
      val condition = new LLVMIntegerComparison(builder,LLVMIntPredicate.LLVMIntEQ,ifcase._1.compile(builder,scope),LLVMConstantInteger.constantInteger(new LLVMIntegerType(1),1,false),"condition")
      val thenBlock = mergeBlock.insertBasicBlockBefore("then")
      val elseBlock = mergeBlock.insertBasicBlockBefore("else")
      val ifBranch = new LLVMBranchInstruction(builder,condition,thenBlock,elseBlock)
      builder.positionBuilderAtEnd(thenBlock)
      val caseValue = ifcase._2.compile(builder,scope)
      val mergeBranch = new LLVMBranchInstruction(builder,mergeBlock)
      val finalThenBlock = builder.getInsertBlock
      builder.positionBuilderAtEnd(elseBlock)
      compileCases(builder,rest,compiled ++ List((caseValue,finalThenBlock)),mergeBlock,mergeType,scope)
    }
  }

  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = {
    val function = builder.getInsertBlock.getParent
    val mergeBlock = function.appendBasicBlock("merge")
    val mergeType = expressionType.compile
    compileCases(builder,cases,Nil,mergeBlock,mergeType,scope)
  }
}
