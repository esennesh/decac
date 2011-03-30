package decac

import jllvm._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

abstract class MemberSelector
case class NameSelector(name: String) extends MemberSelector
case class IndexSelector(index: Int) extends MemberSelector

class UninferredMember(struct: UninferredExpression,selector: MemberSelector,openPrivate: Boolean = false) extends UninferredExpression {
  val member: Tuple2[TauType,Int] = selector match {
    case NameSelector(name) => struct.selectField(name,openPrivate).get
    case IndexSelector(index) => struct.selectField(index,openPrivate).get
  }
  override val expressionType: TauType = member._1
  val structure = struct
  override def children = (structure :: Nil)
  override def substitute(substitution: TauSubstitution): MemberExpression = {
    val struct = structure.substitute(substitution)
    val mem = (substitution.solve(member._1),member._2)
    new MemberExpression(struct,mem)
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
  override def children = (structure :: Nil)
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
  override def children: List[SpecializedExpression] = (structure :: Nil)
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
