package org.deca.compiler.signature

import scala.collection.mutable.Set
import scala.collection.immutable
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.GraphLattice
import scala.util.Memoize1
import org.jllvm._
import org.deca.compiler.definition._

class TypeDefinition(val constructor: TypeConstructor,val name: String,override val scope: Module) extends Definition {
  scope.define(this)
  constructor.declare(name)
  override val build: Memoize1[Module,immutable.Set[LLVMValue]] = Memoize1((instantiation: Module) => {
    val result = constructor.allSpecializations.toSet
    //for(spec <- result)
    //  instantiation.compiledModule.addTypeName(constructor.name,spec)
    immutable.Set.empty[LLVMValue]
  })
}

object BuiltInSums {
  val BooleanSum = {
    val result = new TypeExpressionConstructor(Nil,new SumType(List(TaggedRecord("false",0,EmptyRecord),TaggedRecord("true",1,EmptyRecord))))
    new TypeDefinition(result,"boolean",GlobalScope)
    result
  }
}

abstract class TypeConstructor(val parameters: List[SignatureVariable]) {
  assert(parameters.forall(_.universal))
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
  protected val specializations = new HashMap[List[MonoSignature],LLVMType]()
  
  def compile(params: List[MonoSignature]): LLVMType
  def resolve(params: List[MonoSignature]): LLVMType
  def represent(params: List[MonoSignature]): MonoType
  def substitution(params: List[MonoSignature]): SignatureSubstitution = {
    val result = new SignatureSubstitution
    for(param <- parameters zip params)
      result.substitute(param._1,param._2,true)
    result
  }
  def freshlySpecialize: List[SignatureVariable] = parameters.map(_ match {
    case tau: TypeVariable => new TypeVariable(false,None)
    case rho: RegionVariable => new RegionVariable(false)
    case epsilon: EffectVariable => new EffectVariable(false)
  })
  def freshlyRepresent: MonoType = represent(freshlySpecialize)
  protected def getSpecialization(params: List[MonoSignature]): Option[LLVMType] = {
    for((specialization,llvmType) <- specializations)
      if(specialization.zip(params).forall(p => p._1 == p._2))
        return Some(llvmType)
    None
  }
  def allSpecializations: Iterable[LLVMType] = specializations.values
}

class TypeExpressionConstructor(alphas: List[SignatureVariable],protected val tau: MonoType) extends TypeConstructor(alphas) {
  if(!tau.variables.forall(parameters.contains(_)))
    throw new Exception("Not all variables in type " + tau.toString + " passed as type-constructor parameters: " + parameters.toString)
  assert(parameters.forall(tvar => !tau.filterT(_ == tvar).isEmpty))
  
  override def compile(params: List[MonoSignature]): LLVMType = specializations.get(params) match {
    case Some(t) => t
    case None => {
      val result = represent(params).compile
      specializations.put(params,result)
      result
    }
  }
  override def resolve(params: List[MonoSignature]): LLVMType = compile(params)
  override def represent(params: List[MonoSignature]): MonoType = {
    parameters.zip(params).foldLeft(tau)((result: MonoType,spec: (SignatureVariable,MonoSignature)) => result.replace(spec._1,spec._2).asInstanceOf[MonoType])
  }
  
  override def toString: String = "forall " + alphas.foldRight(".")((svar,res) => svar.toString + " " + res) + tau.toString
}

class OpenSumConstructor(alphas: List[TypeVariable],addends: List[TaggedRecord],loopNode: Option[MonoType]) extends TypeConstructor(alphas) {
  protected val recurser = new OpaqueType
  protected var cases: List[TaggedRecord] = loopNode match {
    case Some(loop) =>
      for(addend <- addends)
        yield TaggedRecord(addend.name,addend.tag,addend.record.mapT(sig => if(sig == loop) recurser else sig).asInstanceOf[RecordType])
    case None => addends
  }
  
  def extend(addend: TaggedRecord,loopNode: Option[MonoType]): Unit = {
    assert(addend.record.variables.forall(tvar => alphas.contains(tvar)))
    if(!cases.contains((c: TaggedRecord) => c.name == addend.name))
      cases = loopNode match {
        case Some(loop) => TaggedRecord(addend.name,addend.tag,addend.record.mapT(sig => if(sig == loop) recurser else sig).asInstanceOf[RecordType]) :: cases
        case None => addend :: cases
      }
  }
  
  protected val tagRepresentation = new LLVMPointerType(LLVMIntegerType.i8,0)
  
  override def compile(params: List[MonoSignature]): LLVMType = getSpecialization(params) match {
    case Some(t) => t
    case None => {
      //TODO: The identified struct here is probably not actually appropriate.  What I want is a type the linker will resolve later.
      val temporary = new LLVMStructType(List(tagRepresentation,new LLVMIdentifiedStructType(name + "contents")).toArray,true)
      specializations.put(params,temporary)
      temporary
    }
  }
  override def represent(params: List[MonoSignature]): MonoType = {
    val sum = {
      val sum = new SumType(cases)
      val recursive = new RecursiveType(sum,Some(recurser))
      if(TypeOrdering.equiv(recursive,sum))
        sum
      else
        recursive
    }
    parameters.zip(params).foldLeft(sum)((result: MonoType,spec: Tuple2[SignatureVariable,MonoSignature]) => result.mapT((sig: MonoType) => if(sig == spec._1) spec._2.asInstanceOf[MonoType] else sig))
  }
  
  protected def caseRepresentation(which: Int): LLVMType = {
    assert(which < cases.length)
    if(cases.forall(c => TypeOrdering.equiv(c.record,EmptyRecord)))
      tagRepresentation
    else
      new LLVMStructType(List(tagRepresentation,cases.apply(which).record.compile).toArray,true)
  }
  
  override def resolve(params: List[MonoSignature]): LLVMType = represent(params).compile
}

object ExceptionConstructor extends OpenSumConstructor(Nil,List(TaggedRecord("AnyException","AnyException",EmptyRecord)),None) {
  new TypeDefinition(this,"Exception",StandardLibrary)
}

case class SkolemConstructor(shape: RecordType) extends TypeConstructor(shape.variables.toList) {
  val witnesses = new HashSet[MonoType]
  override def compile(params: List[MonoSignature]): LLVMType = getSpecialization(params) match {
    case Some(op) => op
    case None => {
      val result = (new OpaqueType).compile
      specializations.put(params,result)
      result
    }
  }
  override def resolve(params: List[MonoSignature]): LLVMType = represent(params).compile
  override def represent(params: List[MonoSignature]): MonoType = {
    val specialize = (spec: MonoType) => parameters.zip(params).foldLeft(spec)((result: MonoType,specs: Tuple2[SignatureVariable,MonoSignature]) => result.mapT((sig: MonoType) => if(sig == specs._1) specs._2.asInstanceOf[MonoType] else sig))
    witnesses.toList.sortWith((x,y) => specialize(x).sizeOf >= specialize(y).sizeOf).head
  }
  def witness(w: MonoType): Unit = {
    assert(w.variables.forall(svar => parameters.contains(svar)))
    witnesses.add(w)
  }
}

object TopSkolem extends SkolemConstructor(EmptyRecord)
object BottomSkolem extends SkolemConstructor(EmptyRecord)

object SkolemOrdering extends PartialOrdering[SkolemConstructor] {
  implicit val typeOrdering = TypeOrdering
  override def lt(x: SkolemConstructor,y: SkolemConstructor): Boolean =  x == BottomSkolem || x.shape < y.shape
  override def equiv(x: SkolemConstructor,y: SkolemConstructor): Boolean = x.shape == y.shape
  override def gt(x: SkolemConstructor,y: SkolemConstructor): Boolean = y == BottomSkolem || x.shape > y.shape
  override def lteq(x: SkolemConstructor,y: SkolemConstructor): Boolean = x == BottomSkolem || x.shape <= y.shape
  override def gteq(x: SkolemConstructor,y: SkolemConstructor): Boolean = y == BottomSkolem || x.shape >= y.shape
  override def tryCompare(x: SkolemConstructor,y: SkolemConstructor): Option[Int] = {
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
  
object SkolemConstructors {
  implicit val skolemOrdering = SkolemOrdering
  protected val skolems = new GraphLattice[SkolemConstructor](TopSkolem,BottomSkolem)
  protected val shapes = new HashMap[RecordType,SkolemConstructor]()
  
  def get(shape: RecordType): SkolemConstructor = shapes.get(shape) match {
    case Some(skolem) => skolem
    case None => {
      val result = new SkolemConstructor(shape)
      skolems.add(result)
      shapes.put(shape,result)
      result
    }
  }
}
