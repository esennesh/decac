package decac

import jllvm.LLVMValue
import jllvm.LLVMConstant
import jllvm.LLVMBasicBlock
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder

trait UninferredExpression {
  val expressionType: TauType
  def children: List[UninferredExpression]
  protected def substituteTypes(substitution: TauSubstitution): Tuple2[TauType,List[Expression]] = {
    (substitution.solve(expressionType),children.map(child => child.substitute(substitution)))
  }
  def substitute(substitution: TauSubstitution): Expression
  def constrain(rui: RangeUnificationInstance): RangeUnificationInstance
  
  def selectField(field: String,openPrivate: Boolean): Option[Tuple2[TauType,Int]] = expressionType match {
    case rec: RecordProduct => {
      val member = rec.fields.zipWithIndex.find(f => f._1.name == Some(field) && (f._1.isPublic || openPrivate)).get
      Some((member._1.tau,member._2))
    }
    case sum: SumType => {
      val member = sum.minimalRecord.fields.zipWithIndex.find(f => f._1.name == Some(field) && (f._1.isPublic || openPrivate)).get
      Some((member._1.tau,member._2))
    }
    case _ => None
  }
  
  def selectField(index: Int,openPrivate: Boolean): Option[Tuple2[TauType,Int]] = expressionType match {
    case rec: RecordProduct => {
      val member = rec.fields.filter(f => f.isPublic || openPrivate).apply(index)
      Some((member.tau,index))
    }
    case sum: SumType => {
      val member = sum.minimalRecord.fields.filter(f => f.isPublic || openPrivate).apply(index)
      Some((member.tau,index))
    }
    case _ => None
  }
}

trait Expression {
  val expressionType: TauType
  def children: List[Expression]
  def specialize(specialization: BetaSpecialization): SpecializedExpression
}

trait SpecializedExpression {
  val expressionType: GammaType
  expressionType match {
    case rho: RhoType => assert(rho.filter(tau => tau.isInstanceOf[TauVariable]) == Nil)
    case _ => Unit
  }
  def children: List[SpecializedExpression]
  def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue
}

trait ConstantExpression extends SpecializedExpression with UninferredExpression with Expression {
  override val expressionType: GammaType
  override def children: List[ConstantExpression]
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = rui
  override def substitute(substitution: TauSubstitution): ConstantExpression = this
  override def specialize(specialization: BetaSpecialization): ConstantExpression = this
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMConstant = build
  def build: LLVMConstant
}
