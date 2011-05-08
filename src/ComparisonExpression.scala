package decac

import jllvm._
import jllvm.llvm.LLVMIntPredicate
import jllvm.llvm.LLVMRealPredicate
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

abstract class ComparisonOperator
case class LesserComp(oreq: Boolean) extends ComparisonOperator
case class GreaterComp(oreq: Boolean) extends ComparisonOperator
case object EqualComp extends ComparisonOperator
case object DifferentComp extends ComparisonOperator

class UninferredComparison(op: ComparisonOperator,l: UninferredExpression,r: UninferredExpression) extends UninferredExpression {
  override val expressionType = BuiltInSums.BooleanGamma
  val operator = op
  val left = l
  val right = r
  val alpha = new TauVariable
  override val children: List[UninferredExpression] = List(l,r)
  override val writable = false
  
  def substitute(substitution: TauSubstitution): ComparisonExpression = {
    val genLeft = left.substitute(substitution)
    val genRight = right.substitute(substitution)
    new ComparisonExpression(op,genLeft,genRight,substitution.solve(alpha))
  }
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    rui.constrain(new LesserEq(left.expressionType,alpha))
    rui.constrain(new LesserEq(right.expressionType,alpha))
    if(operator == LesserComp || operator == GreaterComp)
      rui.constrain(new LesserEq(alpha,FP128Gamma))
    left.constrain(rui)
    right.constrain(rui)
    rui
  }
}

class ComparisonExpression(op: ComparisonOperator,l: Expression,r: Expression,tau: TauType) extends Expression {
  override val expressionType: TauType = BuiltInSums.BooleanGamma
  val operator = op
  val left = l
  val right = r
  val commonType = tau
  override val children: List[Expression] = List(l,r)
  val specializations: Map[BetaSpecialization,SpecializedComparison] = new HashMap[BetaSpecialization,SpecializedComparison]()
  def specialize(specialization: BetaSpecialization): SpecializedExpression = {
    val specLeft = left.specialize(specialization)
    val specRight = right.specialize(specialization)
    val gamma = specialization.solve(commonType)
    new SpecializedComparison(op,specLeft,specRight,gamma)
  }
}

class SpecializedComparison(op: ComparisonOperator,l: SpecializedExpression,r: SpecializedExpression,g: GammaType) extends SpecializedExpression {
  override val expressionType = BuiltInSums.BooleanGamma
  val left = l
  val right = l
  val operator = op
  val gamma = g
  override val children: List[SpecializedExpression] = List(left,right)
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = {
    val compLeft = (new ImplicitUpcast(left,gamma)).compile(builder,scope)
    val compRight = (new ImplicitUpcast(right,gamma)).compile(builder,scope)
    operator match {
      case LesserComp(oreq) => {
        assert(gamma.isInstanceOf[NumericalGamma])
        gamma.asInstanceOf[NumericalGamma] match {
          case real: RealGamma => {
            val op = if(oreq) LLVMRealPredicate.LLVMRealOLE else LLVMRealPredicate.LLVMRealOLT
            new LLVMFloatComparison(builder,op,compLeft,compRight,"comparison")
          }
          case unsigned: UnsignedIntegerGamma => {
            val op = if(oreq) LLVMIntPredicate.LLVMIntULE else LLVMIntPredicate.LLVMIntULT
            new LLVMIntegerComparison(builder,op,compLeft,compRight,"comparison")
          }
          case signed: IntegerGamma => {
            val op = if(oreq) LLVMIntPredicate.LLVMIntSLE else LLVMIntPredicate.LLVMIntSLT
            new LLVMIntegerComparison(builder,op,compLeft,compRight,"comparison")
          }
        }
      }
      case GreaterComp(oreq) => {
        assert(gamma.isInstanceOf[NumericalGamma])
        gamma.asInstanceOf[NumericalGamma] match {
          case real: RealGamma => {
            val op = if(oreq) LLVMRealPredicate.LLVMRealOGE else LLVMRealPredicate.LLVMRealOGT
            new LLVMFloatComparison(builder,op,compLeft,compRight,"comparison")
          }
          case unsigned: UnsignedIntegerGamma => {
            val op = if(oreq) LLVMIntPredicate.LLVMIntUGE else LLVMIntPredicate.LLVMIntUGT
            new LLVMIntegerComparison(builder,op,compLeft,compRight,"comparison")
          }
          case signed: IntegerGamma => {
            val op = if(oreq) LLVMIntPredicate.LLVMIntSGE else LLVMIntPredicate.LLVMIntSGT
            new LLVMIntegerComparison(builder,op,compLeft,compRight,"comparison")
          }
        }
      }
      case EqualComp => gamma match {
        case num: NumericalGamma => num match {
          case real: RealGamma => new LLVMFloatComparison(builder,LLVMRealPredicate.LLVMRealOEQ,compLeft,compRight,"comparison")
          case _ => {
            val op = num match {
              case unsigned: UnsignedIntegerGamma => LLVMIntPredicate.LLVMIntEQ
              case signed: IntegerGamma => LLVMIntPredicate.LLVMIntEQ
            }
            new LLVMIntegerComparison(builder,op,compLeft,compRight,"comparison")
          }
        }
        case sum: SumType => {
          assert(sum.enumeration)
          new LLVMIntegerComparison(builder,LLVMIntPredicate.LLVMIntEQ,compLeft,compRight,"comparison")
        }
        case pointer: PointerType => new LLVMIntegerComparison(builder,LLVMIntPredicate.LLVMIntEQ,compLeft,compRight,"comparison")
        case array: ArrayType => {
          assert(array.dynamic)
          new LLVMIntegerComparison(builder,LLVMIntPredicate.LLVMIntEQ,compLeft,compRight,"comparison")
        }
        case function: FunctionArrow => new LLVMIntegerComparison(builder,LLVMIntPredicate.LLVMIntEQ,compLeft,compRight,"comparison")
        //How the hell do I define equality for closures?
      }
      case DifferentComp => gamma match {
        case num: NumericalGamma => num match {
          case real: RealGamma => new LLVMFloatComparison(builder,LLVMRealPredicate.LLVMRealONE,compLeft,compRight,"comparison")
          case _ => {
            val op = num match {
              case unsigned: UnsignedIntegerGamma => LLVMIntPredicate.LLVMIntNE
              case signed: IntegerGamma => LLVMIntPredicate.LLVMIntNE
            }
            new LLVMIntegerComparison(builder,op,compLeft,compRight,"comparison")
          }
        }
        case sum: SumType => {
          assert(sum.enumeration)
          new LLVMIntegerComparison(builder,LLVMIntPredicate.LLVMIntNE,compLeft,compRight,"comparison")
        }
        case pointer: PointerType => new LLVMIntegerComparison(builder,LLVMIntPredicate.LLVMIntNE,compLeft,compRight,"comparison")
        case array: ArrayType => {
          assert(array.dynamic)
          new LLVMIntegerComparison(builder,LLVMIntPredicate.LLVMIntNE,compLeft,compRight,"comparison")
        }
      }
    }
  }
}
