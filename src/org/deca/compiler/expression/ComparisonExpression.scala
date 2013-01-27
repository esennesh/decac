package org.deca.compiler.expression

import org.jllvm._
import org.jllvm.bindings._
import org.deca.compiler.definition._
import org.deca.compiler.signature._

abstract class ComparisonOperator
case class OrdinalComparison(greater: Boolean,oreq: Boolean) extends ComparisonOperator
case class IdentityComparison(same: Boolean) extends ComparisonOperator

class ComparisonExpression(val operator: ComparisonOperator,
                           val left: Expression,
                           val right: Expression,
                           var join: MonoType = new TypeVariable(false,None)) extends Expression {
  expType = BuiltInSums.BooleanSum.represent(Nil)
  val children: List[Expression] = List(left,right)
  
  override def constrain(lui: LatticeUnificationInstance): Unit = {
    lui.constrain(new SubsumptionConstraint(left.expType,join))
    lui.constrain(new SubsumptionConstraint(right.expType,join))
    if(operator.isInstanceOf[OrdinalComparison])
      lui.constrain(new SubsumptionConstraint(join,FP128Type))
    left.constrain(lui)
    right.constrain(lui)
  }
  override def check(lui: LatticeUnificationInstance): Unit = Unit
  override def substitute(sub: SignatureSubstitution): Unit = {
    super.substitute(sub)
    join = sub.solve(join)
    left.substitute(sub)
    right.substitute(sub)
  }
  override def specialize(spec: SignatureSubstitution,specScope: Scope): ComparisonExpression =
    new ComparisonExpression(operator,left.specialize(spec,specScope),right.specialize(spec,specScope),spec.solve(join))
  override def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue = {
    val compLeft = (new ImplicitUpcast(left,join)).compile(builder,scope,instantiation)
    val compRight = (new ImplicitUpcast(right,join)).compile(builder,scope,instantiation)
    operator match {
      case OrdinalComparison(greater,oreq) => join.asInstanceOf[NumericalType] match {
        case real: RealType => {
          val op = (greater,oreq) match {
            case (true,true) => LLVMRealPredicate.LLVMRealOGE
            case (true,false) => LLVMRealPredicate.LLVMRealOGT
            case (false,true) => LLVMRealPredicate.LLVMRealOLE
            case (false,false) => LLVMRealPredicate.LLVMRealOLT
          }
          new LLVMFloatComparison(builder,op,compLeft,compRight,"comparison")
        }
        case unsigned: UnsignedIntegerType => {
          val op = (greater,oreq) match {
            case (true,true) => LLVMIntPredicate.LLVMIntUGE
            case (true,false) => LLVMIntPredicate.LLVMIntUGT
            case (false,true) => LLVMIntPredicate.LLVMIntULE
            case (false,false) => LLVMIntPredicate.LLVMIntULT
          }
          new LLVMIntegerComparison(builder,op,compLeft,compRight,"comparison")
        }
        case signed: IntegerType => {
          val op = (greater,oreq) match {
            case (true,true) => LLVMIntPredicate.LLVMIntSGE
            case (true,false) => LLVMIntPredicate.LLVMIntSGT
            case (false,true) => LLVMIntPredicate.LLVMIntSLE
            case (false,false) => LLVMIntPredicate.LLVMIntSLT
          }
          new LLVMIntegerComparison(builder,op,compLeft,compRight,"comparison")
        }
      }
      case IdentityComparison(same) => join match {
        case num: NumericalType => num match {
          case real: RealType => new LLVMFloatComparison(builder,if(same) LLVMRealPredicate.LLVMRealOEQ else LLVMRealPredicate.LLVMRealONE,compLeft,compRight,"comparison")
          case integer: IntegerType => new LLVMIntegerComparison(builder,if(same) LLVMIntPredicate.LLVMIntEQ else LLVMIntPredicate.LLVMIntNE,compLeft,compRight,"comparison")
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
        case function: FunctionPointer => new LLVMIntegerComparison(builder,LLVMIntPredicate.LLVMIntEQ,compLeft,compRight,"comparison")
        //How the hell do I define equality for closures?
      }
    }
  }
}
