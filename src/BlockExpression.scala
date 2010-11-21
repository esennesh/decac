package decac

import jllvm.LLVMValue
import jllvm.LLVMBasicBlock
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

class UninferredBlock(exprs: List[UninferredExpression]) extends UninferredExpression(exprs.last.expressionType) {
  val steps: List[UninferredExpression] = exprs
  override def children = steps
  override def substitute(substitution: TauSubstitution): BlockExpression = {
    new BlockExpression(substituteTypes(substitution)._2)
  }
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    children.map(child => child.constrain(rui))
    rui
  }
}

class BlockExpression(exprs: List[Expression]) extends Expression(exprs.last.expressionType) {
  val steps: List[Expression] = exprs
  override def children = steps
  val specializations: Map[BetaSpecialization,SpecializedBlock] = new HashMap[BetaSpecialization,SpecializedBlock]()
  override def specialize(specialization: BetaSpecialization): SpecializedBlock = specializations.get(specialization) match {
    case Some(sb) => sb
    case None => {
      val result = new SpecializedBlock(children.map(child => child.specialize(specialization)))
      specializations.put(specialization,result)
      return result
    }
  }
}

class SpecializedBlock(exprs: List[SpecializedExpression]) extends SpecializedExpression(exprs.last.expressionType) {
  val steps: List[SpecializedExpression] = exprs
  override def children = steps
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = {
    val builtSteps = children.map(child => child.compile(builder,scope))
    builtSteps.last
  }
}
