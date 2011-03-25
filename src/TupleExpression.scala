package decac

import jllvm.LLVMValue
import jllvm.LLVMUndefinedValue
import jllvm.LLVMInsertValueInstruction
import jllvm.LLVMInstructionBuilder
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

class UninferredTuple(exprs: List[UninferredExpression]) extends UninferredExpression {
  val elements: List[UninferredExpression] = exprs
  override val expressionType: TauType = new RecordProduct(elements.map(e => RecordMember(None,e.expressionType)))
  override def children = elements
  override def substitute(substitution: TauSubstitution): TupleExpression = {
    new TupleExpression(substituteTypes(substitution)._2)
  }
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    children.map(child => child.constrain(rui))
    rui
  }
}

class TupleExpression(exprs: List[Expression]) extends Expression {
  val elements: List[Expression] = exprs
  override val expressionType: TauType = new RecordProduct(elements.map(e => RecordMember(None,e.expressionType)))
  override def children = elements
  val specializations: Map[BetaSpecialization,SpecializedTuple] = new HashMap[BetaSpecialization,SpecializedTuple]()
  override def specialize(specialization: BetaSpecialization): SpecializedTuple = specializations.get(specialization) match {
    case Some(sb) => sb
    case None => {
      val result = new SpecializedTuple(children.map(child => child.specialize(specialization)))
      specializations.put(specialization,result)
      return result
    }
  }
}

class SpecializedTuple(exprs: List[SpecializedExpression]) extends SpecializedExpression {
  val elements: List[SpecializedExpression] = exprs
  override val expressionType: GammaType = new RecordProduct(elements.map(e => RecordMember(None,e.expressionType)))
  override def children = elements
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = {
    val initial = new LLVMUndefinedValue(expressionType.compile)
    var result: LLVMValue = initial
    for((child,index) <- children.zipWithIndex)
      result = new LLVMInsertValueInstruction(builder,result,child.compile(builder,scope),index,"insert")
    result
  }
}
