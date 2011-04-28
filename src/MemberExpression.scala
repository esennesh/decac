package decac

import jllvm._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

abstract class MemberSelector
case class NameSelector(name: String) extends MemberSelector
case class IndexSelector(index: Int) extends MemberSelector

class UninferredMember(struct: UninferredExpression,selector: MemberSelector,openPrivate: Boolean = false) extends UninferredExpression {
  val selection = selector
  override val expressionType = new TauVariable
  val structure = struct
  override val children = (structure :: Nil)
  override val writable = struct.writable
  def checkMember(substitution: TauSubstitution): Tuple2[TauType,Int] = (substitution.solve(structure.expressionType),selection) match {
    case (rec: RecordProduct,NameSelector(field)) => {
      val mem = rec.fields.zipWithIndex.find(f => f._1.name == Some(field) && (f._1.isPublic || openPrivate)).get
      (mem._1.tau,mem._2)
    }
    case (rec: RecordProduct,IndexSelector(index)) => {
      val mem = rec.fields.filter(f => f.isPublic || openPrivate).apply(index)
      (mem.tau,index)
    }
    case (sum: SumType,NameSelector(field)) => {
      val mem = sum.minimalRecord.fields.zipWithIndex.find(f => f._1.name == Some(field) && (f._1.isPublic || openPrivate)).get
      (mem._1.tau,mem._2)
    }
    case (sum: SumType,IndexSelector(index)) => {
      val mem = sum.minimalRecord.fields.filter(f => f.isPublic || openPrivate).apply(index)
      (mem.tau,index)
    }
    case _ => throw new TypeException("Cannot select fields of non-variant, non-record type.")
  }
  override def check(rui: RangeUnificationInstance): RangeUnificationInstance = {
    structure.check(rui)
    val substitution = rui.solve
    val member = checkMember(substitution)
    rui.constrain(new Equal(member._1,substitution.solve(expressionType)))
    rui
  }
  override def substitute(substitution: TauSubstitution): MemberExpression = {
    val member = checkMember(substitution)
    val struct = structure.substitute(substitution)
    new MemberExpression(struct,(substitution.solve(member._1),member._2))
  }
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    children.map(child => child.constrain(rui))
    rui
  }
}

class MemberExpression(struct: Expression,m: Tuple2[TauType,Int]) extends Expression {
  val member = m
  override val expressionType: TauType = member._1
  val structure = struct
  override val children = (structure :: Nil)
  val specializations: Map[BetaSpecialization,SpecializedMember] = new HashMap[BetaSpecialization,SpecializedMember]()
  override def specialize(specialization: BetaSpecialization): SpecializedMember = specializations.get(specialization) match {
    case Some(sm) => sm
    case None => {
      val struct = structure.specialize(specialization)
      val mem = (specialization.solve(member._1),member._2)
      val result = new SpecializedMember(struct,mem)
      specializations.put(specialization,result)
      result
    }
  }
}

class SpecializedMember(struct: SpecializedExpression,m: Tuple2[GammaType,Int]) extends SpecializedExpression {
  val member = m
  override val expressionType: GammaType = member._1
  val structure = struct
  override val children: List[SpecializedExpression] = (structure :: Nil)
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = {
    val struct = structure.compile(builder,scope)
    structure.expressionType match {
      case rec: RecordProduct => new LLVMExtractValueInstruction(builder,struct,member._2,"extract")
      case sum: SumType => {
        val contents = new LLVMExtractValueInstruction(builder,struct,1,"extract")
        val p = new LLVMStackAllocation(builder,contents.typeOf,LLVMConstantInteger.constantInteger(Nat.compile,1,false),"cast_alloc")
        new LLVMStoreInstruction(builder,contents,p)
        val casted = new LLVMLoadInstruction(builder,new LLVMBitCast(builder,p,new LLVMPointerType(sum.minimalRecord.compile,0),"pointer_cast"),"bit_casted")
        new LLVMExtractValueInstruction(builder,casted,member._2,"extract")
      }
      case _ => throw new Exception("Why does a member expression have a base other than a variant or a record?")
    }
  }
}
