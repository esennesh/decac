package decac

import jllvm.LLVMBasicBlock
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

class BlockExpression(exprs: List[Expression]) extends Expression(new TauVariable()) {
  val steps: List[Expression] = exprs
  override def children = steps
  val specializations: Map[List[RhoType],LLVMBasicBlock] = new HashMap[List[RhoType],LLVMBasicBlock]()
  def specialize(types: List[RhoType],block: LLVMBasicBlock): Boolean
  def specialized(types: List[RhoType]): Option[LLVMBasicBlock] = {
    specializations.find(pair => pair._1.zip(types).forall(p => p._1.equals(p._2))) match {
      case Some((rhos: List[RhoType],block: LLVMBasicBlock)) => Some(block)
      case None => None
    }
  }
  override def compile(specialization: List[RhoType],block: Option[LLVMBasicBlock]): Option[LLVMBasicBlock] = {
    specialized(specialization) match {
      case Some(block) => {
        children.map(child => child.compile(specialization,Some(block)))
        Some(block)
      }
      case None => None
    }
  }
}
