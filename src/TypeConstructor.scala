package decac

import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import jllvm._

class TypeDefinition(cons: TypeConstructor,n: String,context: Module) extends Definition {
  override val name = n
  override val scope = {context.define(this) ; context }
  val constructor = { cons.declare(name) ; cons }
}

abstract class TypeConstructor(alphas: List[TypeVariable]) {
  protected var strName: Option[String] = None
  def declare(str: String): String = strName match {
    case Some(n) => n
    case None => {
      strName = Some(str)
      str
    }
  }
  def name: String = strName match {
    case Some(n) => n
    case None => getClass().getName() + '@' + Integer.toHexString(hashCode())
  }
  val parameters: List[TypeVariable] = alphas
  assert(parameters.forall(param => param.universal))
  protected val specializations = new HashMap[List[MonoType],LLVMType]()
  
  def compile(params: List[MonoType]): LLVMType
  def resolve(params: List[MonoType]): LLVMType
  def represent(params: List[MonoType]): MonoType
  protected def getSpecialization(params: List[MonoType]): Option[LLVMType] = {
    for((specialization,llvmType) <- specializations.contents)
      if(specialization.zip(params).forall(p => p._1 == p._2))
        return Some(llvmType)
    None
  }
  def allSpecializations: Iterable[LLVMType] = specializations.values
}

class TypeExpressionConstructor(alphas: List[TypeVariable],t: MonoType extends TypeConstructor(alphas) {
  assert(t.filter(t => t.isInstanceOf[TypeVariable]).forall(tvar => parameters.contains(tvar)))
  protected val tau: MonoType = t
  
  override def compile(params: List[MonoType]): LLVMType = specializations.get(params) match {
    case Some(t) => t
    case None => {
      val result = represent(params).compile
      specializations.put(params,result)
      result
    }
  }
  override def resolve(params: List[MonoType]): LLVMType = compile(params)
  override def represent(params: List[MonoType]): MonoType = {
    parameters.zip(params).foldLeft(tau)((result: MonoType,spec: Tuple2[TypeVariable,MonoType]) => result.replace(spec._1,spec._2))
  }
}

class OpenSumConstructor(alphas: List[TypeVariable],addends: List[Tuple2[String,RecordType]],loopNode: Option[MonoType]) extends TypeConstructor(alphas) {
  protected val recurser = new OpaqueType
  protected var cases: List[Tuple2[String,RecordType]] = loopNode match {
    case Some(loop) => addends.map(addend => (addend._1,addend._2.replace(loop,recurser)))
    case None => addends
  }
  
  def extend(addend: Tuple2[String,RecordType],loopNode: Option[MonoType]): Unit = {
    assert(addend._2.filter(tau => tau.isInstanceOf[TypeVariable]).forall(tvar => parameters.contains(tvar)))
    if(!cases.contains(c => c._1 == addend._1))
      cases = loopNode match {
        case Some(loop) => (addend._1,addend._2.replace(loop,recurser)) :: cases
        case None => addend :: cases
      }
  }
  
  protected val tagRepresentation = new LLVMPointerType(LLVMIntegerType.i8,0)
  
  override def compile(params: List[MonoType]): LLVMType = getSpecialization(params) match {
    case Some(t) => t
    case None => {
      val temporary = new LLVMStructType(List(tagRepresentation,new LLVMOpaqueType).toArray,true)
      specializations.put(params,temporary)
      temporary
    }
  }
  override def represent(params: List[MonoType]): MonoType = {
    val sum = {
      val sum = new SumType(cases)
      if(sum.filter(tau => tau == recurser))
        new RecursiveType(sum,Some(recurser))
      else
        sum
    }
    parameters.zip(params).foldLeft(sum)((result: MonoType,spec: Tuple2[TypeVariable,MonoType]) => result.replace(spec._1,spec._2))
  }
  
  protected def caseRepresentation(which: Int): LLVMType = {
    val sum = 
    assert(which < cases.length)
    if(enumeration)
      tagRepresentation
    else
      new LLVMStructType(List(tagRepresentation,cases.apply(which).record.compile).toArray,true)
  }
  
  override def resolve(params: List[MonoType]): LLVMType = {
    val sum = represent(params)
    if(sum.enumeration)
      tagRepresentation
    else {
      val maxRecord = sum.cases.map(c => c.record).sortWith((x,y) => x.sizeOf >= y.sizeOf).head
      new LLVMStructType(List(tagRepresentation,maxRecord.compile).toArray,true)
    }
  }
}

object ExceptionConstructor extends OpenSumConstructor(Nil,List(("AnyException",EmptyRecord)),None) {
  new TypeDefinition(this,"Exception",StandardLibrary)
}

case class SkolemConstructor(shape: RecordType) extends TypeConstructor(shape.variables) {
  var witnesses = new HashSet[MonoType]
  
  override def compile(params: List[MonoType]): LLVMType = getSpecialization(params) match {
    case Some(op) => op
    case None => {
      val result = (new OpaqueType).compile
      specializations.put(params,result)
      result
    }
  }
  override def resolve(params: List[MonoType]): LLVMType = represent(params).compile
  override def represent(params: List[MonoType]): MonoType = {
    val specialize = (spec: MonoType) => parameters.zip(params).foldLeft(spec)((result: MonoType,specs: Tuple2[TypeVariable,MonoType]) => result.replace(specs._1,specs._2))
    witnesses.toList.sortWith((x,y) => specialize(x).sizeOf >= specialize(y).sizeOf).head
  }
  def witness(w: MonoType): OpaqueType = {
    assert(w.variables.forall(svar => parameters.contains(svar))
    witnesses.put(w)
  }
}

object SkolemOrdering extends PartialOrdering[Tuple2[RecordType,SkolemConstructor]] {
  override def lt(x: Tuple2[RecordType,SkolemConstructor],y: Tuple2[RecordType,SkolemConstructor]): Boolean = x._1 < y._1
  override def equiv(x: Tuple2[RecordType,SkolemConstructor],y: Tuple2[RecordType,SkolemConstructor]): Boolean = x._1 == y._1
  override def gt(x: Tuple2[RecordType,SkolemConstructor],y: Tuple2[RecordType,SkolemConstructor]): Boolean = x._1 < y._1
  override def lteq(x: Tuple2[RecordType,SkolemConstructor],y: Tuple2[RecordType,SkolemConstructor]): Boolean = x._1 <= y._1
  override def gteq(x: Tuple2[RecordType,SkolemConstructor],y: Tuple2[RecordType,SkolemConstructor]): Boolean = x._1 >= y._1
  override def tryCompare(x: Tuple2[RecordType,SkolemConstructor],y: Tuple2[RecordType,SkolemConstructor]): Option[Int] = {
    if(gt(x,y))
      Some(1)
    else if(lt(x,y))
      Some(-1)
    else if(equiv(x,y))
      Some(0)
    else
      None
  }
}

object TopSkolem extends SkolemConstructor(EmptyRecord)
object BottomSkolem extends SkolemConstructor(EmptyRecord)
  
object SkolemConstructors {
  protected val skolems = new GraphLattice[Tuple2[RecordType,SkolemConstructor]](TopSkolem,BottomSkolem,SkolemOrdering)
  
  def get(shape: RecordType): SkolemConstructor = skolems.get(shape) match {
    case Some(skolem) => skolem
    case None => {
      val result = new SkolemConstructor(shape)
      skolems.put(shape,result)
      result
    }
  }
}
