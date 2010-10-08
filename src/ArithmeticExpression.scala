package decac

import jllvm.LLVMValue
import jllvm.LLVMBasicBlock
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder

abstract class UninferredArithmetic extends UninferredExpression(new GammaRange(None,Some(FP128Gamma))) {
  override def children: List[UninferredArithmetic]
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    children.map(child => child.expressionType).foreach(t => rui.constrain(new LesserEq(t,expressionType)))
    children.map(child => child.expressionType).foldLeft(expressionType)((x: TauType,y: TauType) => { rui.constrain(new Equal(x,y)) ; y })
    rui
  }
}

abstract class ArithmeticExpression(exprType: NumericalGamma) extends Expression(exprType) {
  override def children: List[ArithmeticExpression]
  override def specialize(specialization: BetaSpecialization): SpecializedArithmetic
}

abstract class SpecializedArithmetic(gamma: NumericalGamma) extends SpecializedExpression(gamma) {
  override def children: List[SpecializedArithmetic]
}
