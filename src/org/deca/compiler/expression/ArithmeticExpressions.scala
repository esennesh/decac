package org.deca.compiler.expression

import org.jllvm._
import org.deca.compiler.definition._
import org.deca.compiler.signature._

abstract class ArithmeticExpression extends Expression {
  expType = new TypeVariable(false,None)
  override def constrain(scs: SignatureConstraints): Unit = {
    scs.push(new SubsumptionConstraint(expType,FP128Type))
    for(child <- children) {
      child.constrain(scs)
      scs.push(new SubsumptionConstraint(child.expType,expType))
    }
  }
  override def check(scs: SignatureConstraints): Unit = for(child <- children) child.check(scs)
  def specialize(spec: SignatureSubstitution,specScope: Scope): ArithmeticExpression
}

class ArithmeticOperatorExpression(val operator: Char,val left: Expression,val right: Expression) extends ArithmeticExpression {
  override val children = List(left, right)
  override def specialize(spec: SignatureSubstitution,specScope: Scope): ArithmeticOperatorExpression =
    new ArithmeticOperatorExpression(operator,left.specialize(spec,specScope),right.specialize(spec,specScope))
  
  override def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue = {
    val lhs = (new ImplicitUpcast(left,expType)).compile(builder,scope,instantiation)
    val rhs = (new ImplicitUpcast(right,expType)).compile(builder,scope,instantiation)
    operator match {
      case '+' => new LLVMAddInstruction(builder,lhs,rhs,!expType.isInstanceOf[IntegerType],"add")
      case '-' => new LLVMSubtractInstruction(builder,lhs,rhs,!expType.isInstanceOf[IntegerType],"subtract")
      case '*' => new LLVMMultiplyInstruction(builder,lhs,rhs,!expType.isInstanceOf[IntegerType],"multiply")
      case '/' => {
        val divisionType = expType match {
          case real: RealType => LLVMDivideInstruction.DivisionType.FLOAT
          case unsigned: UnsignedIntegerType => LLVMDivideInstruction.DivisionType.UNSIGNEDINT
          case integer: IntegerType => LLVMDivideInstruction.DivisionType.SIGNEDINT
        }
        new LLVMDivideInstruction(builder,lhs,rhs,divisionType,"divide")
      }
    }
  }
}
