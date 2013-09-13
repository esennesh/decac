package org.deca.compiler.signature

import scala.collection.{mutable, immutable}
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
    case None => getClass.getName + '@' + Integer.toHexString(hashCode())
  }
  protected val specializations = new mutable.HashMap[List[MonoSignature],LLVMType]()
  
  def compile(params: List[MonoSignature]): LLVMType
  def resolve(params: List[MonoSignature]): LLVMType
  def represent(params: List[MonoSignature]): MonoType
  def substitution(params: List[MonoSignature]): SignatureSubstitution = {
    val result = new SignatureSubstitution
    for(param <- parameters zip params)
      result.substitute(param._1, param._2, specialize = true)
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

object VariantTypes {
  def variant(parent: String, alternatives: List[(String,RecordType)], loopNode: Option[MonoType] = None): ClassBrand = {
    val result = new ClassBrand(parent,EmptyRecord,Map.empty,None)
    val loop = new BrandType(result,EmptyRecord)
    for(alternative <- alternatives) {
      val record: RecordType = loopNode match {
        case Some(hook) => alternative._2.mapT(tau => if(tau == hook) loop else tau).asInstanceOf[RecordType]
        case None => alternative._2
      }
      new ClassBrand(alternative._1,record,Map.empty,Some(result))
    }
    result
  }
}

object BuiltInSums {
  val BooleanBrand: ClassBrand = {
    val result = VariantTypes.variant("boolean", List(("false",EmptyRecord),("true",EmptyRecord)))
    result.seal(Map("false" -> 0,"true" -> 1))
    result
  }
  val BooleanSum: TypeExpressionConstructor = {
    val result = new TypeExpressionConstructor(Nil,new BrandType(BooleanBrand,EmptyRecord))
    new TypeDefinition(result,"boolean",GlobalScope)
    result
  }
}

object ClosureTypes {
  val apply = Memoize1((fp: FunctionPointer) => {
    val brand = new ClassBrand("Closure" + fp.toString, EmptyRecord, Map("apply" -> fp), None)
    new BrandType(brand,EmptyRecord)
  })
}
