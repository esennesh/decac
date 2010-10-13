package decac

import jllvm.LLVMValue
import jllvm.LLVMBasicBlock
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder
import jllvm.LLVMConstantInteger
import jllvm.LLVMAddInstruction
import jllvm.LLVMSubtractInstruction
import jllvm.LLVMMultiplyInstruction
import jllvm.LLVMFloatDivideInstruction
import jllvm.LLVMSignedDivideInstruction
import jllvm.LLVMIntegerToFloatCast
import scala.Math
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

abstract class UninferredArithmetic extends UninferredExpression(new GammaRange(None,Some(FP128Gamma))) {
  override def children: List[UninferredArithmetic]
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    rui.constrain(new LesserEq(expressionType,FP128Gamma))
    children.map(child => child.expressionType).foreach(t => rui.constrain(new LesserEq(t,expressionType)))
    rui
  }
  override def substitute(substitution: TauSubstitution): ArithmeticExpression
  protected override def substituteTypes(substitution: TauSubstitution): Tuple2[NumericalGamma,List[ArithmeticExpression]] = {
    val resultType = substitution.solve(expressionType) match {
      case ngam: NumericalGamma => ngam
      case _ => throw new Exception("ERROR: Type inference found non-numerical type for arithmetic operation.")
    }
    (resultType,children.map(child => child.substitute(substitution)))
  }
}

abstract class ArithmeticOperator

case object Add extends ArithmeticOperator
case object Subtract extends ArithmeticOperator
case object Multiply extends ArithmeticOperator
case object Divide extends ArithmeticOperator

class UninferredOperator(x: UninferredArithmetic,y: UninferredArithmetic,op: ArithmeticOperator) extends UninferredArithmetic {
  val operator = op
  override def children: List[UninferredArithmetic] = x :: y :: Nil
  override def substitute(substitution: TauSubstitution): OperatorExpression = {
    val substitutionResult = substituteTypes(substitution)
    val subexprs = substitutionResult._2
    new OperatorExpression(subexprs.apply(0),subexprs.apply(1),operator,substitutionResult._1)
  }
}

class UninferredInteger(i: Int) extends UninferredArithmetic {
  val value: Int = i
  override def children: List[UninferredArithmetic] = Nil
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    if(value >= IntegerConstants.min_unsigned) {
      if(value <= IntegerConstants.max_byte)
        rui.constrain(new LesserEq(expressionType,Byte))
      else if(value <= IntegerConstants.max_snat)
        rui.constrain(new LesserEq(expressionType,SNat))
      else if(value <= IntegerConstants.max_nat)
        rui.constrain(new LesserEq(expressionType,Nat))
      else if(value <= IntegerConstants.max_longnat)
        rui.constrain(new LesserEq(expressionType,LongNat))
    }
    else {
      if(value >= IntegerConstants.min_octet)
        rui.constrain(new LesserEq(expressionType,Octet))
      else if(value >= IntegerConstants.min_sInt)
        rui.constrain(new LesserEq(expressionType,SInt))
      else if(value >= IntegerConstants.min_Int)
        rui.constrain(new LesserEq(expressionType,Int))
      else if(value >= IntegerConstants.min_longInt)
        rui.constrain(new LesserEq(expressionType,LongInt))
    }
    rui
  }
  override def substitute(substitution: TauSubstitution): IntegerConstant = {
    val resultType = substituteTypes(substitution)._1 match {
      case igam: IntegerGamma => igam
      case _ => throw new Exception("Type inference should never infer a non-integer type for an integer constant.")
    }
    new IntegerConstant(value,resultType)
  }
}

abstract class ArithmeticExpression(exprType: NumericalGamma) extends Expression(exprType) {
  override val expressionType: NumericalGamma = exprType
  override def children: List[ArithmeticExpression]
  override def specialize(specialization: BetaSpecialization): SpecializedArithmetic
}

class OperatorExpression(x: ArithmeticExpression,y: ArithmeticExpression,op: ArithmeticOperator,exprType: NumericalGamma) extends ArithmeticExpression(exprType) {
  val specializations: Map[BetaSpecialization,SpecializedOperator] = new HashMap[BetaSpecialization,SpecializedOperator]()
  val operator = op
  override def children: List[ArithmeticExpression] = x :: y :: Nil
  override def specialize(specialization: BetaSpecialization): SpecializedOperator = specializations.get(specialization) match {
    case Some(so) => so
    case None => {
      val specializedChildren = children.map(child => child.specialize(specialization))
      val lhs = specializedChildren.apply(0)
      val rhs = specializedChildren.apply(1)
      val result = new SpecializedOperator(lhs,rhs,operator,expressionType)
      specializations.put(specialization,result)
      result
    }
  }
}

class IntegerConstant(i: Int,exprType: IntegerGamma) extends ArithmeticExpression(exprType) {
  val value: Int = i
  override def children: List[ArithmeticExpression] = Nil
  override def specialize(specialization: BetaSpecialization): SpecializedArithmetic = {
    new SpecializedIntConst(value,exprType)
  }
}

abstract class SpecializedArithmetic(gamma: NumericalGamma) extends SpecializedExpression(gamma) {
  override def children: List[SpecializedArithmetic]
}

class SpecializedOperator(x: SpecializedArithmetic,y: SpecializedArithmetic,op: ArithmeticOperator,exprType: NumericalGamma) extends SpecializedArithmetic(exprType) {
  val operator = op
  override def children: List[SpecializedArithmetic] = x :: y :: Nil
  def coerce(child: SpecializedArithmetic,builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = expressionType match {
    case real: RealGamma => children.apply(0).expressionType match {
      case unsigned: UnsignedIntegerGamma => new LLVMIntegerToFloatCast(builder,child.compile(builder,scope),real.compile,"cast",LLVMIntegerToFloatCast.IntCastType.UNSIGNED)
      case signed: IntegerGamma => new LLVMIntegerToFloatCast(builder,child.compile(builder,scope),real.compile,"cast",LLVMIntegerToFloatCast.IntCastType.SIGNED)
      case floating: RealGamma => child.compile(builder,scope)
    }
    case integer: IntegerGamma => child.compile(builder,scope)
  }
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = {
    val builtSteps = children.map(child => compile(builder,scope))
    //TODO: Add support for checking the types of the subexpressions and casting to floating-point where necessary.
    val lhs = coerce(children.apply(0),builder,scope)
    val rhs = coerce(children.apply(1),builder,scope)
    operator match {
      case Add => new LLVMAddInstruction(builder,lhs,rhs,"add")
      case Subtract => new LLVMSubtractInstruction(builder,lhs,rhs,"subtract")
      case Multiply => new LLVMMultiplyInstruction(builder,lhs,rhs,"multiply")
      case Divide => expressionType match {
        case real: RealGamma => new LLVMFloatDivideInstruction(builder,lhs,rhs,"fdivide")
        case integer: IntegerGamma => new LLVMSignedDivideInstruction(builder,lhs,rhs,"idivide")
      }
    }
  }
}

class SpecializedIntConst(i: Int,exprType: IntegerGamma) extends SpecializedArithmetic(exprType) {
  override val expressionType: IntegerGamma = exprType
  val value: Int = i
  override def children: List[SpecializedArithmetic] = Nil
  def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = {
    LLVMConstantInteger.constantInteger(expressionType.compile,value,true)
  }
}
