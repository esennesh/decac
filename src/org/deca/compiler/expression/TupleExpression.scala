package org.deca.compiler.expression

import org.jllvm._
import org.deca.compiler.signature._
import org.deca.compiler.definition._

class TupleExpression(override val children: List[Expression]) extends Expression {
  expType = new RecordType(children.map(child => RecordMember(None,ImmutableMutability,child.expType)))
  
  override def constrain(lui: LatticeUnificationInstance): Unit =
    for(child <- children)
      child.constrain(lui)
  override def check(lui: LatticeUnificationInstance): Unit =
    for(child <- children)
      child.check(lui)
      
  override def substitute(sub: SignatureSubstitution): Unit =
    for(child <- children)
      child.substitute(sub)
  override def specialize(spec: SignatureSubstitution,specScope: Scope): TupleExpression =
    new TupleExpression(children.map(_.specialize(spec,specScope)))
  
  override def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue = {
    val base = new LLVMUndefinedValue(expType.compile)
    children.zipWithIndex.foldLeft[LLVMValue](base)((result: LLVMValue,child: (Expression,Int)) => {
      val compiledChild = child._1.compile(builder,scope,instantiation)
      new LLVMInsertValueInstruction(builder,result,compiledChild,child._2,"insert_value")
    })
  }
}
