package org.deca.compiler.definition

import scala.collection.immutable.{Set,HashMap,Map}
import scala.util.Memoize1
import org.jllvm._
import org.jllvm.bindings._
import org.deca.compiler.signature._
import org.deca.compiler.expression.{EffectPair,Expression}

trait FunctionBody {
  val scope: LexicalScope
  val signature: FunctionSignature
  
  def infer: SignatureSubstitution
  def substitute(substitution: SignatureSubstitution): Unit
  def specialize(spec: SignatureSubstitution): FunctionBody
  def compile(instantiation: Module,builder: LLVMInstructionBuilder): LLVMValue
}

case class FunctionSignature(var arguments: List[(String,MonoType)],
                             var implicits: List[(String,MonoType)],
                             var result: MonoType = new TypeVariable(false,None),
                             var effect: EffectPair = EffectPair(new EffectVariable(false),new EffectVariable(false))) {
  def substitute(substitution: SignatureSubstitution): Unit = {
    arguments = arguments.map(arg => (arg._1,substitution.solve(arg._2)))
    implicits = implicits.map(impl => (impl._1,substitution.solve(impl._2)))
    result = substitution.solve(result)
    effect = EffectPair(substitution.solve(effect.positive),
                        substitution.solve(effect.negative))
  }
  def specialize(spec: SignatureSubstitution): FunctionSignature =
    FunctionSignature(arguments.map(arg => (arg._1,spec.solve(arg._2))),
                      implicits.map(impl => (impl._1,spec.solve(impl._2))),
                      spec.solve(result),
                      EffectPair(spec.solve(effect.positive),spec.solve(effect.negative)))
  def arrow: Either[TypeExpressionConstructor,FunctionPointer] = {
    val func = new FunctionPointer(arguments.map(_._2) ++ implicits.map(_._2),result,effect.positive,effect.negative)
    val argVariables: List[SignatureVariable] = arguments.map(_._2.variables.toList).reduceLeft(_ ++ _)
    if(argVariables.forall(svar => svar.universal))
      Left(new TypeExpressionConstructor(argVariables,func))
    else
      Right(func)
  }
  override def toString: String = arrow match {
    case Left(constructor) => constructor.toString
    case Right(func) => func.toString
  }
}

class FunctionDefinition(val name: String,
                         override val scope: Module,
                         val signature: FunctionSignature,
                         mkBody: Option[FunctionSignature => FunctionBody]) extends Definition {
  scope.define(this)
  val body: Option[FunctionBody] = mkBody.map(f => f(signature))
  for(b <- body) {
    System.err.println("Inferring types for " + name + "()")
    val substitution = b.infer
    signature.substitute(substitution)
    b.substitute(substitution)
    assert(signature.effect.safe(PureEffect))
  }
  val specialize: Memoize1[List[MonoSignature],Memoize1[Module,LLVMFunction]] = Memoize1(sigvars => {
    val funcType: TypeConstructor = this.signature.arrow match {
      case Left(funcType) => funcType
      case Right(_) => throw new Exception("Attempting to specialize a function before its principal type has been inferred.")
    }
    val signature: FunctionPointer = funcType.represent(sigvars).asInstanceOf[FunctionPointer]
    Memoize1(instantiation => {
      val func = new LLVMFunction(instantiation.compiledModule,name + signature.toString,signature.compile)
      System.err.println("Defining " + func.getValueName + ": " + signature.toString + " (" + func.typeOf.toString + ")")
      body match {
        case Some(b) if funcType.parameters != Nil || instantiation == scope => {
          if(funcType.parameters != Nil)
            func.setLinkage(LLVMLinkage.LLVMWeakODRLinkage)
          val specialization = new SignatureSubstitution
          for(spec <- (funcType.parameters zip sigvars))
            specialization.substitute(spec._1,spec._2)
          val entry = func.appendBasicBlock("entry")
          val builder = new LLVMInstructionBuilder
          builder.positionBuilderAtEnd(entry)
          new LLVMReturnInstruction(builder,b.specialize(specialization).compile(instantiation,builder))
        }
        case _ => func.setLinkage(LLVMLinkage.LLVMExternalLinkage)
      }
      func
    })
  })
  override val build: Memoize1[Module,Set[LLVMValue]] = Memoize1(instantiation => signature.arrow match {
    case Left(funcType) => {
      if(funcType.parameters == Nil)
        Set.empty[LLVMValue] + specialize(Nil)(instantiation)
      else
        specialize.values.foldLeft(Set.empty[LLVMValue])((set,specialization) => set + specialization(instantiation))
    }
    case Right(_) => throw new Exception("Attempting to build LLVM code for a function before its principal type has been inferred.")
  })
}

class ExpressionBody(override val signature: FunctionSignature,
                     parent: Module,
                     mkBody: LexicalScope => Expression) extends FunctionBody {
  override val scope = new LexicalScope(parent,signature.arguments ++ signature.implicits)
  val body: Expression = mkBody(scope)
  override def infer: SignatureSubstitution = {
    val inference = new LatticeUnificationInstance
    body.constrain(inference.constraints)
    inference.constrain(new SubsumptionConstraint(body.expType,signature.result))
    inference.constrain(new SubsumptionConstraint(body.expEffect.positive,signature.effect.positive))
    inference.constrain(new SubsumptionConstraint(body.expEffect.negative,signature.effect.negative))
    inference.solve
    body.check(inference)
    val substitution = inference.solve
    substitution
  }
  override def substitute(substitution: SignatureSubstitution): Unit = {
    scope.substitute(substitution)
    body.substitute(substitution)
  }
  override def specialize(spec: SignatureSubstitution): ExpressionBody =
    new ExpressionBody(signature.specialize(spec),parent,(lexical: LexicalScope) => body.specialize(spec,lexical))
  override def compile(instantiation: Module,builder: LLVMInstructionBuilder): LLVMValue = {
    val llvmArguments = new HashMap ++ builder.getInsertBlock.getParent.getParameters.toList.map(arg => (arg.getValueName,arg))
    scope.setArguments(llvmArguments)
    body.compile(builder,scope,instantiation)
  }
}
