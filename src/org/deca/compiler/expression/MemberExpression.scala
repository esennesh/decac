package org.deca.compiler.expression

import org.jllvm._
import org.deca.compiler.signature._
import org.deca.compiler.definition._

sealed abstract class MemberSelector
case class NameSelector(name: String) extends MemberSelector
case class IndexSelector(index: Int) extends MemberSelector

//TODO: Do I need structure to be a WritableExpression?
class MemberExpression(val structure: Expression,
                       val selector: MemberSelector) extends WritableExpression {
  expType = new TypeVariable(false,None)
  override val children = Nil
  protected var checkedSelector: Option[Int] = None
  
  def checkMember(sub: SignatureSubstitution): (MonoType,Int) = (sub.solve(structure.expType),selector) match {
    case (rec: RecordType,NameSelector(field)) => {
      val mem = rec.fields.zipWithIndex.find(f => f._1.name == Some(field)).get
      (mem._1.tau,mem._2)
    }
    case (rec: RecordType,IndexSelector(index)) => {
      val mem = rec.fields.apply(index)
      (mem.tau,index)
    }
    case (sum: SumType,NameSelector(field)) => {
      val mem = sum.minimalRecord.fields.zipWithIndex.find(f => f._1.name == Some(field)).get
      (mem._1.tau,mem._2)
    }
    case (sum: SumType,IndexSelector(index)) => {
      val mem = sum.minimalRecord.fields.apply(index)
      (mem.tau,index)
    }
    case _ => throw new TypeException("Cannot select fields of non-variant, non-record type.")
  }
  override def constrain(scs: SignatureConstraints): Unit = structure.constrain(scs)
  override def check(lui: LatticeUnificationInstance): Unit = {
    structure.check(lui)
    val substitution = lui.solve
    val member = checkMember(substitution)
    checkedSelector = Some(member._2)
    lui.constrain(EqualityConstraint(member._1,substitution.solve(expType)))
  }
  
  override def substitute(sub: SignatureSubstitution): Unit = structure.substitute(sub)
  override def specialize(spec: SignatureSubstitution,specScope: Scope): MemberExpression =
    new MemberExpression(structure.specialize(spec,specScope),selector)
  
  override def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue = {
    val struct = structure.compile(builder,scope,instantiation)
    structure.expType match {
      case record: RecordType => new LLVMExtractValueInstruction(builder,struct,checkedSelector.get,"extract")
      case sum: SumType => {
        val contents = new LLVMExtractValueInstruction(builder,struct,1,"extract")
        val p = new LLVMStackAllocation(builder,contents.typeOf,LLVMConstantInteger.constantInteger(Nat.compile,1,false),"cast_alloc")
        new LLVMStoreInstruction(builder,contents,p)
        val casted = new LLVMLoadInstruction(builder,new LLVMBitCast(builder,p,new LLVMPointerType(sum.minimalRecord.compile,0),"pointer_cast"),"bit_casted")
        new LLVMExtractValueInstruction(builder,casted,checkedSelector.get,"extract")
      }
      case _ => throw new Exception("Why does a member expression have a base other than a variant or a record?")
    }
  }
  override def store(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module,value: LLVMValue): LLVMValue
}
