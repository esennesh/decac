package org.deca.compiler.expression

import org.deca.compiler.definition._
import org.deca.compiler.signature._
import org.jllvm._

class IfExpression(val condition: Expression,
                   val then: Expression,
                   val otherwise: Option[Expression]) extends Expression {
  expType = new TypeVariable(false,None)
  override val children: List[Expression] = condition :: then :: otherwise.toList
  override def constrain(lui: LatticeUnificationInstance): Unit = {
    condition.constrain(lui)
    lui.constrain(new SubsumptionConstraint(condition.expType,BuiltInSums.BooleanSum.represent(Nil)))
    then.constrain(lui)
    lui.constrain(new SubsumptionConstraint(then.expType,expType))
    otherwise.foreach(exp => {
      exp.constrain(lui)
      lui.constrain(new SubsumptionConstraint(exp.expType,expType))
    })
  }
  override def check(lui: LatticeUnificationInstance): Unit = {
    condition.check(lui)
    then.check(lui)
    otherwise.foreach(_.check(lui))
  }
  override def substitute(sub: SignatureSubstitution): Unit = {
    condition.substitute(sub)
    then.substitute(sub)
    otherwise.foreach(_.substitute(sub))
    expType = sub.solve(expType)
  }
  override def specialize(spec: SignatureSubstitution,specScope: Scope): IfExpression = {
    val result = new IfExpression(condition.specialize(spec,specScope),then.specialize(spec,specScope),otherwise.map(exp => exp.specialize(spec,specScope)))
    result.expType = spec.solve(expType)
    result
  }
  def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue = otherwise match {
    case Some(expr) => {
      val comparator = condition.compile(builder,scope,instantiation)
      val merge = builder.getInsertBlock.getParent.appendBasicBlock("merge")
      val thenBlock = merge.insertBasicBlockBefore("then")
      val elseBlock = merge.insertBasicBlockBefore("else")
      new LLVMBranchInstruction(builder,comparator,thenBlock,elseBlock)
      
      builder.positionBuilderAtEnd(thenBlock)
      val thenValue = (new ImplicitUpcast(then,expType)).compile(builder,scope,instantiation)
      new LLVMBranchInstruction(builder,merge)
      
      builder.positionBuilderAtEnd(elseBlock)
      val elseValue = (new ImplicitUpcast(expr,expType)).compile(builder,scope,instantiation)
      new LLVMBranchInstruction(builder,merge)
      
      builder.positionBuilderAtEnd(merge)
      val phi = new LLVMPhiNode(builder,expType.compile,"ifphi")
      phi.addIncoming((thenValue :: elseValue :: Nil).toArray,(thenBlock :: elseBlock :: Nil).toArray)
      phi
    }
    case None => {
      val comparator = condition.compile(builder,scope,instantiation)
      val merge = builder.getInsertBlock.getParent.appendBasicBlock("merge")
      val thenBlock = merge.insertBasicBlockBefore("then")
      new LLVMBranchInstruction(builder,comparator,thenBlock,merge)
      
      builder.positionBuilderAtEnd(thenBlock)
      val value = (new ImplicitUpcast(then,expType)).compile(builder,scope,instantiation)
      new LLVMBranchInstruction(builder,merge)
      
      builder.positionBuilderAtEnd(merge)
      val phi = new LLVMPhiNode(builder,expType.compile,"ifphi")
      phi.addIncoming((value :: Nil).toArray,(thenBlock :: Nil).toArray)
      phi
    }
  }
}
