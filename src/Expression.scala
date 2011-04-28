package decac

import scala.collection.mutable.HashMap
import jllvm.LLVMValue
import jllvm.LLVMConstant
import jllvm.LLVMBasicBlock
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder

trait UninferredExpression {
  val expressionType: TauType
  val children: List[UninferredExpression]
  val writable: Boolean
  protected def substituteTypes(substitution: TauSubstitution): Tuple2[TauType,List[Expression]] = {
    (substitution.solve(expressionType),children.map(child => child.substitute(substitution)))
  }
  def substitute(substitution: TauSubstitution): Expression
  def constrain(rui: RangeUnificationInstance): RangeUnificationInstance
  def check(rui: RangeUnificationInstance): RangeUnificationInstance = {
    children.foreach(child => child.check(rui))
    rui
  }
}

trait Expression {
  val expressionType: TauType
  val children: List[Expression]
  def specialize(specialization: BetaSpecialization): SpecializedExpression
}

trait SpecializedExpression {
  val expressionType: GammaType
  expressionType match {
    case rho: RhoType => assert(rho.filter(tau => tau.isInstanceOf[TauVariable]) == Nil)
    case _ => Unit
  }
  val children: List[SpecializedExpression]
  def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue
}

trait WritableExpression extends SpecializedExpression {
  def store(builder: LLVMInstructionBuilder,scope: Scope[_],value: LLVMValue): LLVMValue
}

trait ConstantExpression extends SpecializedExpression with UninferredExpression with Expression {
  override val expressionType: GammaType
  override val children: List[ConstantExpression]
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = rui
  override def substitute(substitution: TauSubstitution): ConstantExpression = this
  override def specialize(specialization: BetaSpecialization): ConstantExpression = this
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMConstant = build
  def build: LLVMConstant
}
