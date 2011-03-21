package decac

import jllvm._
import scala.Math
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

abstract class UninferredArithmetic extends UninferredExpression {
  override val expressionType: TauType = new TauVariable
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    rui.constrain(new LesserEq(expressionType,FP128Gamma))
    for(child <- children) {
      child.constrain(rui)
      rui.constrain(new LesserEq(child.expressionType,expressionType))
    }
    rui
  }
  override def substitute(substitution: TauSubstitution): ArithmeticExpression
  protected override def substituteTypes(substitution: TauSubstitution): Tuple2[NumericalGamma,List[Expression]] = {
    val resultType = substitution.solve(expressionType) match {
      case ngam: NumericalGamma => ngam
      case other => throw new Exception("ERROR: Type inference found non-numerical type for arithmetic expression, " + other.toString)
    }
    (resultType,children.map(child => child.substitute(substitution)))
  }
}

abstract class ArithmeticOperator

case object Add extends ArithmeticOperator {
  override def toString: String = "+"
}
case object Subtract extends ArithmeticOperator {
  override def toString = "-"
}
case object Multiply extends ArithmeticOperator {
  override def toString = "*"
}
case object Divide extends ArithmeticOperator {
  override def toString = "/"
}

class UninferredOperator(x: UninferredExpression,y: UninferredExpression,op: ArithmeticOperator) extends UninferredArithmetic {
  val operator = op
  override def children: List[UninferredExpression] = List(x,y)
  override def substitute(substitution: TauSubstitution): OperatorExpression = {
    val substitutionResult = substituteTypes(substitution)
    val subexprs = substitutionResult._2
    new OperatorExpression(subexprs.apply(0),subexprs.apply(1),operator,substitutionResult._1)
  }
}

class UninferredInteger(i: Int) extends UninferredArithmetic {
  val value: Int = i
  override def children: List[UninferredArithmetic] = Nil
  override val expressionType = if(value >= 0) {
      if(value <= IntegerConstants.max_byte)
        Byte
      else if(value <= IntegerConstants.max_snat)
        SNat
      else if(value <= IntegerConstants.max_nat)
        Nat
      else
        LongNat
    }
    else {
      if(value >= IntegerConstants.min_octet)
        Octet
      else if(value >= IntegerConstants.min_sInt)
        SInt
      else if(value >= IntegerConstants.min_Int)
        Int
      else
        LongInt
    }
  override def substitute(substitution: TauSubstitution): IntegerConstant = {
    val substitutedType = substituteTypes(substitution)._1
    val resultType = substitutedType match {
      case igam: IntegerGamma => igam
      case _ => throw new Exception("ERROR: Type inference inferred non-integer type for integer constant: " + substitutedType.toString)
    }
    new IntegerConstant(value,resultType)
  }
}

abstract class ArithmeticExpression(exprType: NumericalGamma) extends Expression {
  override val expressionType: NumericalGamma = exprType
  override def specialize(specialization: BetaSpecialization): SpecializedArithmetic
}

class OperatorExpression(x: Expression,y: Expression,op: ArithmeticOperator,exprType: NumericalGamma) extends ArithmeticExpression(exprType) {
  val specializations: Map[BetaSpecialization,SpecializedOperator] = new HashMap[BetaSpecialization,SpecializedOperator]()
  val operator = op
  override def children: List[Expression] = x :: y :: Nil
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

abstract class SpecializedArithmetic(gamma: NumericalGamma) extends SpecializedExpression {
  override val expressionType: NumericalGamma = gamma
}

class SpecializedOperator(x: SpecializedExpression,y: SpecializedExpression,op: ArithmeticOperator,exprType: NumericalGamma) extends SpecializedArithmetic(exprType) {
  val operator = op
  override def children: List[SpecializedExpression] = x :: y :: Nil
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = {
    val lhs = (new ImplicitUpcast(children.apply(0),expressionType)).compile(builder,scope)
    val rhs = (new ImplicitUpcast(children.apply(1),expressionType)).compile(builder,scope)
    operator match {
      case Add => new LLVMAddInstruction(builder,lhs,rhs,!expressionType.isInstanceOf[IntegerGamma],"add")
      case Subtract => new LLVMSubtractInstruction(builder,lhs,rhs,!expressionType.isInstanceOf[IntegerGamma],"subtract")
      case Multiply => new LLVMMultiplyInstruction(builder,lhs,rhs,!expressionType.isInstanceOf[IntegerGamma],"multiply")
      case Divide => {
        val DivisionType = expressionType match {
          case real: RealGamma => LLVMDivideInstruction.DivisionType.FLOAT
          case unsigned: UnsignedIntegerGamma => LLVMDivideInstruction.DivisionType.UNSIGNEDINT
          case integer: IntegerGamma => LLVMDivideInstruction.DivisionType.SIGNEDINT
        }
        new LLVMDivideInstruction(builder,lhs,rhs,DivisionType,"divide")
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
