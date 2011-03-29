package decac

import jllvm.LLVMValue
import jllvm.LLVMUndefinedValue
import jllvm.LLVMInsertValueInstruction
import jllvm.LLVMInstructionBuilder
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

class UninferredTuple(exprs: List[UninferredExpression]) extends UninferredExpression {
  val elements: List[UninferredExpression] = exprs
  override val expressionType: RecordProduct = new RecordProduct(elements.map(e => RecordMember(None,new TauVariable)))
  override def children = elements
  override def substitute(substitution: TauSubstitution): TupleExpression = {
    val (rec,exprs) = substituteTypes(substitution)
    new TupleExpression(exprs,rec.asInstanceOf[RecordProduct])
  }
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    children.map(child => child.constrain(rui))
    for((child,field) <- children.zip(expressionType.fields))
      rui.constrain(new LesserEq(child.expressionType,field.tau))
    rui
  }
}

class TupleExpression(exprs: List[Expression],record: RecordProduct) extends Expression {
  val elements: List[Expression] = exprs
  override val expressionType: RecordProduct = record
  override def children = elements
  val specializations: Map[BetaSpecialization,SpecializedTuple] = new HashMap[BetaSpecialization,SpecializedTuple]()
  override def specialize(specialization: BetaSpecialization): SpecializedTuple = specializations.get(specialization) match {
    case Some(sb) => sb
    case None => {
      val result = new SpecializedTuple(children.map(child => child.specialize(specialization)),specialization.solve(expressionType).asInstanceOf[RecordProduct])
      specializations.put(specialization,result)
      return result
    }
  }
}

class SpecializedTuple(exprs: List[SpecializedExpression],record: RecordProduct) extends SpecializedExpression {
  val elements: List[SpecializedExpression] = exprs
  override val expressionType: RecordProduct = record
  override def children = elements
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = {
    val initial = new LLVMUndefinedValue(expressionType.compile)
    var result: LLVMValue = initial
    for((child,index) <- children.zipWithIndex) {
      val subexpr = (new ImplicitUpcast(child,expressionType.fields.apply(index).tau.asInstanceOf[GammaType])).compile(builder,scope)
      result = new LLVMInsertValueInstruction(builder,result,subexpr,index,"insert")
    }
    result
  }
}
