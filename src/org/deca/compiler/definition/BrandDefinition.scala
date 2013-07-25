package org.deca.compiler.definition

import scala.collection.immutable.{Set,HashMap,Map}
import scala.util._
import org.jllvm._
import org.deca.compiler.signature._
import org.deca.compiler.definition._
import org.deca.compiler.expression._

object EnumerationClasses {
  val apply: Memoize4[Module, String, Int, Option[ClassBrand], ClassBrand] = Memoize4((scope: Module, name: String, tag: Int, parent: Option[ClassBrand]) => {
    val res = ClassBrands.apply(scope, name, parent, Nil, Nil)
    res.seal(Map(name -> tag))
    res
  })
}

object ClassBrands {
  val apply = Memoize5((scope: Scope, name: String, parent: Option[ClassBrand], members: List[MemberDeclaration], methods: List[MethodDeclaration]) => {
    val recordMembers: List[RecordMember] = members.map(member => RecordMember(Some(member.name), member.mutability, member.tau))
    new ClassBrand(name, new RecordType(recordMembers), methods.map(method => (method.name, method.signature.arrow.asInstanceOf[FunctionPointer])).toMap, parent)
  })
}

class EnumValueDefinition(scope: Module, name: String, val tag: Int, val parent: ClassBrand) extends VariableDefinition(scope, name, new EnumerationValue(scope, name, tag, parent), new BrandType(EnumerationClasses.apply(scope, name, tag, Some(parent)), EmptyRecord), ImmutableMutability)

class EnumerationValue(scope: Module, val enum: String, val tag: Int, val parent: ClassBrand) extends ConstantExpression {
  val brand: ClassBrand = EnumerationClasses.apply(scope, enum, tag, Some(parent))
  expType = new BrandType(brand, EmptyRecord)
  override val children = Nil
  override def build(scope: Scope,instantiation: Module): LLVMConstantInteger =
    LLVMConstantInteger.constantInteger(expType.compile.asInstanceOf[LLVMIntegerType],tag,false)
}

class MemberDeclaration(val name: String, val mutability: MonoMutability, val tau: MonoType, init: LexicalScope => Expression) {
  protected var initializerOpt: Option[Expression] = None
  
  def initialize(scope: LexicalScope): Unit =
    initializerOpt = Some(init(scope))
  
  def initializer: Expression = initializerOpt.get
}

object MemberDeclaration {
  def dataConstructors(args: List[(String, MonoMutability, MonoType)]): List[MemberDeclaration] =
    args.map(arg => new MemberDeclaration(arg._1, arg._2, arg._3, (scope: LexicalScope) => new VariableExpression(List(arg._1), scope)))
}

class ClassDefinition(override val scope: Module,
                      override val name: String,
                      arguments: List[(String, MonoType)],
                      implicits: List[(String, MonoType)],
                      val parent: Option[ClassDefinition],
                      val fields: List[MemberDeclaration],
                      methodDeclarations: List[MethodDeclaration]) extends Definition {
  val brand: ClassBrand = ClassBrands.apply(scope, name, parent.map(_.brand), fields, methodDeclarations)
  for(method <- methodDeclarations)
    method.addThis(brand)
  def thisPointer: PointerType = new PointerType(new BrandType(brand, EmptyRecord), new RegionVariable(false), ReadOnlyMutability)
  
  val constructor: FunctionDefinition =
    new FunctionDefinition(name + "_constructor", scope, new FunctionSignature(arguments, implicits, new BrandType(brand, EmptyRecord), EffectPair(PureEffect, PureEffect)), Some(signature => new ClassConstructorBody(signature, fields, scope, brand)))
    
  val methods: List[FunctionDefinition] =
    for(method <- methodDeclarations) yield {
      val methodSig = new FunctionSignature(("this", thisPointer) :: method.signature.arguments, method.signature.implicits, method.signature.result, method.signature.effect)
      new FunctionDefinition(name + method.name, scope, methodSig, Some(signature => new MethodBody(methodSig, brand, scope, method.body)))
    }
  
  override val build: Memoize1[Module, Set[LLVMValue]] = constructor.build
}

class DataConstructorDefinition(scope: Module, name: String, arguments: List[(String, MonoMutability, MonoType)], parent: ClassDefinition) extends ClassDefinition(scope, name, arguments.map(arg => (arg._1, arg._3)), Nil, Some(parent), MemberDeclaration.dataConstructors(arguments), Nil)
