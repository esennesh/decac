package decac

import jllvm.LLVMValue
import jllvm.LLVMBasicBlock
import jllvm.LLVMFunction
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

class UninferredIf(possibilities: List[Tuple2[UninferredExpression,UninferredExpression]],scope: Scope) extends UninferredExpression(new TauVariable,scope) {
  val cases: List[Tuple2[UninferredExpression,UninferredExpression]] = possibilities
  override def children = cases.map(ifcase => ifcase._2)
  override def substitute(substitution: SigmaSubstitution,generalize: Boolean): IfExpression = {
    val conditions = cases.map(ifcase => ifcase._1.substitute(substitution,false))
    val results = cases.map(ifcase => ifcase._2.substitute(substitution,false))
    new IfExpression(conditions.zip(results),scope)
  }
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    cases.map(ifcase => { rui.constrain(new Equal(ifcase._1.expressionType,BooleanRho)); rui.constrain(new LesserEq(ifcase._2.expressionType,expressionType)) })
    return rui
  }
}

class IfExpression(possibilities: List[Tuple2[Expression,Expression]],scope: Scope) extends Expression(possibilities.last._2.expressionType,scope) {
  val cases: List[Tuple2[Expression,Expression]] = possibilities
  override def children = cases.map(ifcase => ifcase._2)
  val specializations: Map[List[RhoType],SpecializedIf] = new HashMap[List[RhoType],SpecializedIf]()
  override def specialize(specialization: List[RhoType]): SpecializedIf = {
    if(specialization.length == expressionType.countUniversals)
      specializations.get(specialization) match {
        case Some(sb) => sb
        case None => {
          val resultCases = cases.map(ifcase => (ifcase._1.specialize(specialization),ifcase._2.specialize(specialization)))
          val result = new SpecializedIf(resultCases,scope)
          specializations.put(specialization,result)
          return result
        }
      }
    else
      throw new Exception("Cannot specialize if expression with list of rho types whose length differs from the number of universally-quantified type variables.")
  }
}
