package org.deca.compiler.definition

import scala.collection.immutable.{Set,HashMap,Map}
import org.jllvm._
import org.deca.compiler.signature._
import org.deca.compiler.definition._
import org.deca.compiler.expression._

sealed trait VariantCase {
  val name: String
  protected def record: RecordType
  val tag: Any = this
  def tagCode: Int = tag match {
    case i: Int => i
    case _ => tag.hashCode
  }
  def taggedRecord: TaggedRecord = TaggedRecord(name,tag,record)
  def defineSelf(scope: Module): Unit
}

case class EnumVariant(override val name: String) extends VariantCase {
  override protected def record: RecordType = EmptyRecord
  override def defineSelf(scope: Module): Unit = new EnumValueDefinition(scope,name,taggedRecord.tagCode)
}

class EnumValueDefinition(scope: Module,name: String,val tag: Int) extends VariableDefinition(scope,name,new EnumerationValue(name,tag),new SumType(List(TaggedRecord(name,tag,EmptyRecord))),ImmutableMutability)

class EnumerationValue(val enum: String,val tag: Int) extends ConstantExpression {
  expType = new SumType(List(TaggedRecord(enum,tag,EmptyRecord)))
  override val children = Nil
  override def build(scope: Scope,instantiation: Module): LLVMConstantInteger =
    LLVMConstantInteger.constantInteger(expType.compile.asInstanceOf[LLVMIntegerType],tag,false)
}

case class DataConstructor(override val name: String,arguments: List[RecordMember]) extends VariantCase {
  override protected def record: RecordType = new RecordType(arguments)
  override def defineSelf(scope: Module): Unit = {
    val members = arguments.zipWithIndex.map(arg => {
      val argName = arg._1.name getOrElse ("#" + arg._2.toString)
      val initializer = (lexical: LexicalScope) => new VariableExpression(List(argName),lexical)
      MemberConstructor(argName,arg._1.mutable,arg._1.tau,initializer)
    })
    val args = arguments.zipWithIndex.map(arg => (arg._1.name getOrElse ("#" + arg._2.toString),arg._1.tau))
    val resultType = new SumType(List(TaggedRecord(name,tag,record)))
    new FunctionDefinition(name,scope,new FunctionSignature(args,Nil,resultType),Some(sig => new RecordConstructorBody(name,sig,scope,tagCode,members)))
  }
}

class RecordConstructor(override val name: String,
                        val arguments: List[(String,MonoType)],
                        val scope: TypeDefinitionScope,
                        mems: List[MemberConstructor]) extends VariantCase {
  val body = new RecordConstructorBody(name,FunctionSignature(arguments,Nil),scope.owner,tagCode,mems)
  val members = body.members
  override def defineSelf(scope: Module): Unit = {
    new FunctionDefinition(name,scope,body.signature,Some(sig => body))
  }
  override protected def record: RecordType = 
    new RecordType(members.map(member => RecordMember(Some(member.name),member.mu,member.tau)))
}

case class MemberConstructor(name: String,mu: MonoMutability,tau: MonoType,initializer: LexicalScope => Expression)
case class MemberInitializer(name: String,var mu: MonoMutability,var tau: MonoType,val initializer: Expression)

class RecordConstructorBody(val name: String,
                            override val signature: FunctionSignature,
                            val parent: Module,
                            val tag: Int,
                            mems: List[MemberConstructor]) extends FunctionBody {
  //TODO: Enable implicit parameters for data constructors
  override val scope: LexicalScope = new LexicalScope(parent,signature.arguments)
  val members: List[MemberInitializer] = mems.map(mem => MemberInitializer(mem.name,mem.mu,mem.tau,mem.initializer(scope))) 
  override def infer: SignatureSubstitution = {
    val inference = new LatticeUnificationInstance
    for(member <- members) {
      member.initializer.constrain(inference)
      inference.constrain(new SubsumptionConstraint(member.initializer.expType,member.tau))
      inference.constrain(new SubsumptionConstraint(member.initializer.expEffect.positive,signature.effect.positive))
      inference.constrain(new SubsumptionConstraint(member.initializer.expEffect.negative,signature.effect.negative))
    }
    inference.constrain(new SubsumptionConstraint(bodyType,signature.result))
    inference.solve
    for(member <- members)
      member.initializer.check(inference)
    inference.solve
  }
  val bodyType: SumType = {
    val recordMembers = members.map(member => {
      val name: Option[String] = if(member.name(0) == '#') None else Some(member.name)
      RecordMember(name,member.mu,member.tau)
    })
    new SumType(List(new TaggedRecord(name,tag,new RecordType(recordMembers))))
  }
  override def substitute(substitution: SignatureSubstitution): Unit = {
    scope.substitute(substitution)
    for(member <- members) {
      member.mu = substitution.solve(member.mu)
      member.tau = substitution.solve(member.tau)
      member.initializer.substitute(substitution)
    }
  }
  def specialize(spec: SignatureSubstitution): RecordConstructorBody = {
    val args = signature.arguments.map(arg => (arg._1,spec.solve(arg._2)))
    val mems = members.map(member => MemberConstructor(member.name,spec.solve(member.mu),spec.solve(member.tau),specScope => member.initializer.specialize(spec,specScope)))
    new RecordConstructorBody(name,FunctionSignature(args,Nil),parent,tag,mems)
  }
  def compile(instantiation: Module,builder: LLVMInstructionBuilder): LLVMValue = {
    val llvmArguments = new HashMap ++ builder.getInsertBlock.getParent.getParameters.toList.map(arg => (arg.getValueName,arg))
    scope.setArguments(llvmArguments)
    val tagType = new LLVMIntegerType(math.floor(math.log(hashCode - 1) / math.log(2)).toInt + 1)
    val compiledTag = LLVMConstantInteger.constantInteger(tagType,hashCode,false)
    val mems = members.map(_.initializer.compile(builder,scope,instantiation))
    val elements = compiledTag :: mems
    val variant = new LLVMUndefinedValue(new LLVMStructType(elements.toArray.map(_.typeOf),true))
    elements.zipWithIndex.foldLeft[LLVMValue](variant)((res: LLVMValue,element: (LLVMValue,Int)) =>
      new LLVMInsertValueInstruction(builder,res,element._1,element._2,"insert"))
  }
}
