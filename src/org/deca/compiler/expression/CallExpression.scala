package org.deca.compiler.expression

import org.jllvm.LLVMValue
import org.jllvm.LLVMFunction
import org.jllvm.LLVMInstructionBuilder
import org.jllvm.{LLVMCallInstruction,LLVMExtractValueInstruction}
import org.deca.compiler.definition._
import org.deca.compiler.signature._

abstract class CallExpression extends Expression {
  val arguments: List[Expression]
  var arrow: FunctionPointer
  override val children: List[Expression] = arguments
  
  override def constrain(lui: LatticeUnificationInstance): Unit = {
    for(argument <- arguments)
      argument.constrain(lui)
    for(actual <- (arguments.map(_.expType) zip arrow.domain))
      lui.constrain(new SubsumptionConstraint(actual._1,actual._2))
  }
  override def check(lui: LatticeUnificationInstance): Unit = {
    for(argument <- arguments)
      argument.check(lui)
    for(actual <- (arguments.map(_.expType) zip arrow.domain))
      lui.constrain(new SubsumptionConstraint(actual._1,actual._2))
  }
  override def substitute(sub: SignatureSubstitution): Unit = {
    for(argument <- arguments)
      argument.substitute(sub)
    arrow = sub.solve(arrow)
  }
  
  def specialize(spec: SignatureSubstitution,specScope: Scope): CallExpression
  
  def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue
}


class DefinitionCall(val definition: FunctionDefinition,
                     args: List[Expression],
                     val implicits: (List[Option[Expression]],Scope),
                     spec: Option[List[MonoSignature]] = None) extends CallExpression {
  val specialization: List[MonoSignature] = spec getOrElse (definition.signature.arrow match {
    case Left(funcType) => funcType.freshlySpecialize
    case Right(_) => Nil
  })
  override val arguments: List[Expression] = {
    val specializeSubstitution = definition.signature.arrow match {
      case Left(funcType) => funcType.substitution(specialization)
      case Right(arrow) => new SignatureSubstitution
    }
    val specializedImplicits = definition.signature.implicits.map(impl => (impl._1,specializeSubstitution.solve(impl._2)))
    val actualImplicits = implicits._1.zip(specializedImplicits).map(impl =>
      impl._1 getOrElse new ImplicitResolutionExpression(impl._2._2,implicits._2))
    args ++ actualImplicits
  }
  assert(definition.signature.arguments.length == args.length)
  assert(definition.signature.implicits.length == implicits._1.length)
  var arrow: FunctionPointer = definition.signature.arrow match {
    case Left(funcType) => funcType.represent(specialization).asInstanceOf[FunctionPointer]
    case Right(arrow) => arrow
  }
  expType = arrow.range
  expEffect = EffectPair(arrow.positive,arrow.negative)
  assert(arguments.length == arrow.domain.length)
  
  override def specialize(spec: SignatureSubstitution,specScope: Scope): DefinitionCall = {
    val newArguments = args.map(_.specialize(spec,specScope))
    val newImplicits = implicits._1.map(impl => impl.map(_.specialize(spec,specScope)))
    val newSpecialization = specialization.map(sigma => spec.solve(sigma))
    new DefinitionCall(definition,newArguments,(newImplicits,specScope),Some(newSpecialization))
  }
    
  override def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue = {
    val function: LLVMFunction = definition.specialize(specialization)(instantiation)
    new LLVMCallInstruction(builder,function,arguments.map(_.compile(builder,scope,instantiation)).toArray,"definition_call")
  }
}

class ExpressionCall(val expression: Expression,override val arguments: List[Expression]) extends CallExpression {
  override val children: List[Expression] = expression :: arguments
  var arrow: FunctionPointer = new FunctionPointer(arguments.map(arg => new TypeVariable(false,None)),new TypeVariable(false,None),new EffectVariable(false),new EffectVariable(false))
  expType = arrow.range
  expEffect = EffectPair(arrow.positive,arrow.negative)
  assert(arguments.length == arrow.domain.length)
  val closureType = {
    val tau = new OpaqueType
    //It might be completely wrong to create a new region variable here.  The region of the environment has to be the region in which the closure itself is stored.
    val funcPointer = new FunctionPointer(new PointerType(tau,new RegionVariable(false),ImmutableMutability) :: arrow.domain,arrow.range,arrow.positive,arrow.negative)
    val shape = new RecordType(List(RecordMember(None,ImmutableMutability,tau),RecordMember(None,ImmutableMutability,funcPointer)))
    new ExistentialInterface(shape,tau)
  }
  
  override def constrain(lui: LatticeUnificationInstance): Unit = {
    super.constrain(lui)
    lui.constrain(new SubsumptionConstraint(expression.expType,closureType))
  }
  
  override def specialize(spec: SignatureSubstitution,specScope: Scope): ExpressionCall = 
    new ExpressionCall(expression.specialize(spec,specScope),arguments.map(_.specialize(spec,specScope)))
  
  override def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue = {
    val closure = expression.compile(builder,scope,instantiation)
    val environment = new LLVMExtractValueInstruction(builder,closure,0,"extract_closure_environment")
    val function = new LLVMExtractValueInstruction(builder,closure,1,"extract_closure_function")
    new LLVMCallInstruction(builder,function,(environment :: arguments.map(_.compile(builder,scope,instantiation))).toArray,"expression_call")
  }
}
