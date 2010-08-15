package decac

import jllvm.LLVMValue
import jllvm.LLVMBasicBlock
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

class UninferredBlock(exprs: List[UninferredExpression],scope: Scope) extends UninferredExpression(exprs.last.expressionType,scope) {
  val steps: List[UninferredExpression] = exprs
  override def children = steps
  override def substitute(substitution: SigmaSubstitution,generalize: Boolean): BlockExpression = {
    val pair = substituteTypes(substitution,generalize)
    new BlockExpression(pair._2,scope)
  }
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    children.map(child => child.constrain(rui))
    rui
  }
}

class BlockExpression(exprs: List[Expression],scope: Scope) extends Expression(exprs.last.expressionType,scope) {
  val steps: List[Expression] = exprs
  override def children = steps
  val specializations: Map[List[RhoType],SpecializedBlock] = new HashMap[List[RhoType],SpecializedBlock]()
  override def specialize(specialization: List[RhoType]): SpecializedBlock = {
    if(specialization.length == expressionType.countUniversals)
      specializations.get(specialization) match {
        case Some(sb) => sb
        case None => {
          val result = new SpecializedBlock(children.map(child => child.specialize(specialization)),scope)
          specializations.put(specialization,result)
          return result
        }
      }
    else
      throw new Exception("Cannot specialize block expression with list of rho types whose length differs from the number of universally-quantified type variables.")
  }
}

class SpecializedBlock(exprs: List[SpecializedExpression],scope: Scope) extends SpecializedExpression(exprs.last.expressionType,scope) {
  val steps: List[SpecializedExpression] = exprs
  override def children = steps
  override def compile(builder: LLVMInstructionBuilder): LLVMValue = {
    val builtSteps = children.map(child => compile(builder))
    builtSteps.last
  }
}
