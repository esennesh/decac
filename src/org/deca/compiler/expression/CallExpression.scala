package org.deca.compiler.expression

import org.jllvm.LLVMValue
import org.jllvm.LLVMFunction
import org.jllvm.LLVMInstructionBuilder
import org.jllvm.{LLVMCallInstruction,LLVMExtractValueInstruction}
import org.deca.compiler.definition._
import org.deca.compiler.signature._

abstract class CallExpression(val arguments: List[Expression]) extends Expression {
  var arrow: FunctionPointer
  expType = arrow.range
  expEffect = EffectPair(arrow.positive,arrow.negative)
  assert(arguments.length == arrow.domain.length)
  override val children: List[Expression] = arguments
  
  override def constrain(scs: SignatureConstraints): Unit = {
    for(argument <- arguments)
      argument.constrain(scs)
    for(actual <- (arguments.map(_.expType) zip arrow.domain))
      scs.push(new SubsumptionConstraint(actual._1,actual._2))
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
                     spec: Option[List[MonoSignature]] = None) extends CallExpression(args) {
  val specialization: List[MonoSignature] = spec getOrElse definition.funcType.freshlySpecialize
  var arrow: FunctionPointer = definition.funcType.represent(specialization).asInstanceOf[FunctionPointer]
  
  override def specialize(spec: SignatureSubstitution,specScope: Scope): DefinitionCall = {
    val newArguments = arguments.map(_.specialize(spec,specScope))
    val newSpecialization = specialization.map(sigma => spec.solve(sigma))
    new DefinitionCall(definition,newArguments,Some(newSpecialization))
  }
    
  override def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue = {
    val function: LLVMFunction = definition.specialize(specialization)(instantiation)
    new LLVMCallInstruction(builder,function,arguments.map(_.compile(builder,scope,instantiation)).toArray,"definition_call")
  }
}

class ExpressionCall(val expression: Expression,args: List[Expression]) extends CallExpression(args) {
  override val children: List[Expression] = expression :: arguments
  var arrow: FunctionPointer = new FunctionPointer(args.map(arg => new TypeVariable(false,None)),new TypeVariable(false,None),new EffectVariable(false),new EffectVariable(false))
  val closureType = {
    val tau = new OpaqueType
    //It might be completely wrong to create a new region variable here.  The region of the environment has to be the region in which the closure itself is stored.
    val funcPointer = new FunctionPointer(new PointerType(tau,new RegionVariable(false),ImmutableMutability) :: arrow.domain,arrow.range,arrow.positive,arrow.negative)
    val shape = new RecordType(List(RecordMember(None,ImmutableMutability,tau),RecordMember(None,ImmutableMutability,funcPointer)))
    new ExistentialInterface(shape,tau)
  }
  
  override def constrain(scs: SignatureConstraints): Unit = {
    super.constrain(scs)
    scs.push(new SubsumptionConstraint(expression.expType,closureType))
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
