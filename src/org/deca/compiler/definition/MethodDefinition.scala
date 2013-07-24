package org.deca.compiler.definition

import scala.collection.immutable.{Set,HashMap,Map}
import scala.util._
import org.jllvm._
import org.deca.compiler.signature._
import org.deca.compiler.definition._
import org.deca.compiler.expression._

class MethodDeclaration(val name: String, sig: FunctionSignature, val body: MethodScope => Expression) {
  private var signatureWithThis: Option[FunctionSignature] = None
  
  def addThis(brand: ClassBrand): Unit = {
    val pointer = new PointerType(new BrandType(brand, EmptyRecord), new RegionVariable(false), ReadOnlyMutability)
    signatureWithThis = Some(new FunctionSignature(("this", pointer) :: sig.arguments, sig.implicits, sig.result, sig.effect))
  }
  def signature: FunctionSignature = signatureWithThis.get
}

class MethodBody(sig: FunctionSignature, owner: ClassBrand, parent: Module, mkBody: MethodScope => Expression) extends ExpressionBody(sig, parent, (s: LexicalScope) => null) {
  override val scope: MethodScope = {
    val functionScope = new LexicalScope(parent,signature.arguments ++ signature.implicits)
    new MethodScope(functionScope, new BrandType(owner, EmptyRecord))
  }
  override val body = mkBody(scope)
}

class InMethodFieldBinding(name: String,
                           scope: MethodScope,
                           tau: MonoType,
                           mut: MonoMutability) extends LexicalBinding(name,scope,tau,mut) {
  protected var member: MemberExpression = new MemberExpression(new VariableExpression(List("this"), scope), NameSelector(name))
  override protected val build = Memoize2((builder: LLVMInstructionBuilder, instantiation: Module) => member.pointer(builder, scope, instantiation))
  override def initialize(builder: LLVMInstructionBuilder,instantiation: Module): LLVMValue = member.compile(builder, scope, instantiation)
  override def substitute(sub: SignatureSubstitution): Unit = {
    variableType = sub.solve(variableType)
    mutability = sub.solve(mutability)
    member.substitute(sub)
  }
  override def specialize(spec: SignatureSubstitution): InMethodFieldBinding = {
    val result = new InMethodFieldBinding(name, scope, spec.solve(tau), spec.solve(mut))
    result.member = result.member.specialize(spec, scope)
    result
  }
}

class MethodScope(parent: LexicalScope, val brand: BrandType) extends LexicalScope(parent, Nil) {
  for(field <- brand.fields)
    field.name.map((name: String) => new InMethodFieldBinding(name, this, field.tau, field.mutable))
  for(method <- brand.brand.methods)
    new InMethodFieldBinding(method._1, this, method._2, ImmutableMutability)
}

class ClassConstructorBody(override val signature: FunctionSignature,
                           val members: List[MemberDeclaration],
                           parent: Module,
                           brand: ClassBrand) extends FunctionBody {
  override val scope: LexicalScope = new LexicalScope(parent,signature.arguments ++ signature.implicits)
  for(member <- members)
    member.initialize(scope)
  
  override def infer: SignatureSubstitution = {
    val inference = new LatticeUnificationInstance
    for((decl, field) <- members.zip(brand.record.fields)) {
      decl.initializer.constrain(inference)
      inference.constrain(new SubsumptionConstraint(decl.initializer.expType, field.tau))
      inference.constrain(new SubsumptionConstraint(decl.initializer.expEffect.positive, signature.effect.positive))
      inference.constrain(new SubsumptionConstraint(decl.initializer.expEffect.negative, signature.effect.negative))
    }
    inference.solve
    for(member <- members.zip(brand.record.fields))
      member._1.initializer.check(inference)
    inference.solve
  }
  override def substitute(substitution: SignatureSubstitution): Unit = {
    signature.substitute(substitution)
    for(member <- members)
      member.initializer.substitute(substitution)
  }
  override def specialize(spec: SignatureSubstitution) =
    new ClassConstructorBody(signature.specialize(spec), members.map(member => new MemberDeclaration(member.name, member.mutability, spec.solve(member.tau), specScope => member.initializer.specialize(spec, specScope))), parent, brand)
  
  override def compile(instantiation: Module,builder: LLVMInstructionBuilder): LLVMValue = {
    val llvmArguments = new HashMap ++ builder.getInsertBlock.getParent.getParameters.toList.map(arg => (arg.getValueName,arg))
    scope.setArguments(llvmArguments)
    val struct = new LLVMUndefinedValue(signature.result.compile)
    members.zipWithIndex.foldLeft[LLVMValue](struct)((res: LLVMValue, member: (MemberDeclaration, Int)) => 
      new LLVMInsertValueInstruction(builder,res,member._1.initializer.compile(builder, scope, instantiation),member._2,"insert"))
  }
}
