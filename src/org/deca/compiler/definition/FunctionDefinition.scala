package org.deca.compiler.definition

import scala.collection.immutable.{Set,HashMap,Map}
import scala.util.Memoize1
import org.jllvm._
import org.jllvm.bindings._
import org.deca.compiler.signature._
import org.deca.compiler.expression.{EffectPair,Expression}

trait FunctionBody {
  val scope: LexicalScope
  val arguments: List[(String,MonoType)]
  val implicits: List[(String,MonoType)]
  def bodyType: MonoType
  def bodyEffect: EffectPair
  
  def infer: SignatureSubstitution
  def substitute(substitution: SignatureSubstitution): Unit
  def specialize(spec: SignatureSubstitution): FunctionBody
  def compile(instantiation: Module,builder: LLVMInstructionBuilder): LLVMValue
}

case class ExternalFunctionSignature(arguments: List[(String,MonoType)],
                                     implicits: List[(String,MonoType)],
                                     result: MonoType,
                                     effect: EffectPair)

class FunctionDefinition(val name: String,
                         val scope: Module,
                         val body: Either[FunctionBody,ExternalFunctionSignature]) extends Definition {
  val arguments = body match {
    case Left(fBody) => fBody.arguments
    case Right(sig) => sig.arguments
  }
  val implicits = body match {
    case Left(fBody) => fBody.implicits
    case Right(sig) => sig.implicits
  }
  val funcType: TypeConstructor = {
    val substitution = body match {
      case Left(fBody) => fBody.infer
      case Right(_) => new SignatureSubstitution
    }
    val args = arguments.map(_._2) ++ implicits.map(_._2)
    val arrow = substitution.solve[MonoType](body match {
      case Left(fBody) => new FunctionPointer(args,fBody.bodyType,fBody.bodyEffect.positive,fBody.bodyEffect.negative)
      case Right(ExternalFunctionSignature(_,_,tau,epsilons)) =>
        new FunctionPointer(args,tau,epsilons.positive,epsilons.negative)
    })
    val solvedArrow = substitution.solve(arrow)
    val result = new TypeExpressionConstructor(solvedArrow.variables.toList,solvedArrow)
    body match {
      case Left(fBody) => {
        fBody.substitute(substitution)
        assert(fBody.bodyEffect.safe(PureEffect))
      }
      case Right(ExternalFunctionSignature(_,_,_,epsilons)) => assert(epsilons.safe(PureEffect))
    }
    result
  }
  val specialize: Memoize1[List[MonoSignature],Memoize1[Module,LLVMFunction]] = Memoize1(sigvars => {
    val signature = funcType.represent(sigvars).asInstanceOf[FunctionPointer]
    Memoize1(instantiation => {
      val func = new LLVMFunction(instantiation.compiledModule,name + signature.toString,signature.compile)
      body match {
        case Left(fBody) if funcType.parameters != Nil || instantiation == scope => {
          if(funcType.parameters != Nil)
            func.setLinkage(LLVMLinkage.LLVMWeakODRLinkage)
          val specialization = new SignatureSubstitution
          for(spec <- (funcType.parameters zip sigvars))
            specialization.substitute(spec._1,spec._2)
          val entry = func.appendBasicBlock("entry")
          val builder = new LLVMInstructionBuilder
          builder.positionBuilderAtEnd(entry)
          new LLVMReturnInstruction(builder,fBody.specialize(specialization).compile(instantiation,builder))
          func
        }
        case _ => func.setLinkage(LLVMLinkage.LLVMExternalLinkage)
      }
      func
    })
  })
  override val build: Memoize1[Module,Set[LLVMValue]] = Memoize1(instantiation => {
    var result = Set.empty[LLVMValue]
    for(specialization <- specialize.values)
      result += specialization(instantiation)
    result
  })
}

class ExpressionBody(override val arguments: List[(String,MonoType)],
                     override val implicits: List[(String,MonoType)],
                     parent: Module,
                     mkBody: LexicalScope => Expression) extends FunctionBody {
  override val scope = new LexicalScope(parent,arguments)
  val body: Expression = mkBody(scope)
  override def bodyType: MonoType = body.expType
  override def bodyEffect: EffectPair = body.expEffect
  override def infer: SignatureSubstitution = {
    val inference = new LatticeUnificationInstance
    body.constrain(inference.constraints)
    inference.solve
    body.check(inference)
    val substitution = inference.solve
    substitution
  }
  override def substitute(substitution: SignatureSubstitution): Unit = {
    scope.substitute(substitution)
    body.substitute(substitution)
  }
  override def specialize(spec: SignatureSubstitution): ExpressionBody = {
    val args = arguments.map(arg => (arg._1,spec.solve(arg._2)))
    val impls = implicits.map(arg => (arg._1,spec.solve(arg._2)))
    val bod = (lexi: LexicalScope) => body.specialize(spec,lexi)
    new ExpressionBody(args,impls,parent,bod)
  }
  override def compile(instantiation: Module,builder: LLVMInstructionBuilder): LLVMValue = {
    val llvmArguments = new HashMap ++ builder.getInsertBlock.getParent.getParameters.toList.map(arg => (arg.getValueName,arg))
    scope.setArguments(llvmArguments)
    body.compile(builder,scope,instantiation)
  }
}
