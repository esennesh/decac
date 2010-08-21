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
  override def substitute(substitution: TauSubstitution): BlockExpression = {
    new BlockExpression(substituteTypes(substitution)._2,scope)
  }
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    children.map(child => child.constrain(rui))
    rui
  }
}

class BlockExpression(exprs: List[Expression],scope: Scope) extends Expression(exprs.last.expressionType,scope) {
  val steps: List[Expression] = exprs
  override def children = steps
  val specializations: Map[SigmaSubstitution,SpecializedBlock] = new HashMap[SigmaSubstitution,SpecializedBlock]()
  override def specialize(specialization: SigmaSubstitution): SpecializedBlock = specializations.get(specialization) match {
    case Some(sb) => sb
    case None => {
      val result = new SpecializedBlock(children.map(child => child.specialize(specialization)),scope)
      specializations.put(specialization,result)
      return result
    }
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
