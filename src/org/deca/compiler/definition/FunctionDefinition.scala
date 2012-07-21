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

case class ExternalFunctionBody(override val arguments: List[(String,MonoType)],
                                override val implicits: List[(String,MonoType)],
                                result: MonoType,
                                effect: EffectPair,
                                parent: Module) extends FunctionBody {
  override val scope = new LexicalScope(parent,arguments)
  override val bodyType: MonoType = result
  override val bodyEffect: EffectPair = effect
  
  override def infer = new SignatureSubstitution
  override def substitute(substitution: SignatureSubstitution): Unit = Unit
  override def specialize(spec: SignatureSubstitution): ExternalFunctionBody = 
    ExternalFunctionBody(arguments.map(arg => (arg._1,spec.solve(arg._2))),
                         implicits.map(impl => (impl._1,spec.solve(impl._2))),
                         spec.solve(result),
                         effect.map(spec.solve(_)),
                         parent)
  override def compile(instantiation: Module,builder: LLVMInstructionBuilder): LLVMValue = new LLVMUnreachableInstruction(builder)
}

class FunctionDefinition(val name: String,
                         override val scope: Module,
                         bod: Unit => FunctionBody) extends Definition {
  scope.define(this)
  val body = bod(Unit)
  val arguments = body.arguments
  val implicits = body.implicits
  val funcType: TypeConstructor = {
    val substitution = body.infer
    val args = arguments.map(_._2) ++ implicits.map(_._2)
    val arrow = substitution.solve[MonoType](new FunctionPointer(args,body.bodyType,body.bodyEffect.positive,body.bodyEffect.negative))
    //Substitute away solved type variables, and then replace what remains with universal variables.
    val solvedArrow = MonoSignature.universalize[MonoType](substitution.solve(arrow))
    //Replace the remaining variables in solvedArrow with universal variables
    solvedArrow
    val result = new TypeExpressionConstructor(solvedArrow.variables.toList,solvedArrow)
    body.substitute(substitution)
    assert(body.bodyEffect.safe(PureEffect))
    result
  }
  val specialize: Memoize1[List[MonoSignature],Memoize1[Module,LLVMFunction]] = Memoize1(sigvars => {
    val signature = funcType.represent(sigvars).asInstanceOf[FunctionPointer]
    Memoize1(instantiation => {
      val func = new LLVMFunction(instantiation.compiledModule,name + signature.toString,signature.compile)
      body match {
        case external: ExternalFunctionBody => func.setLinkage(LLVMLinkage.LLVMExternalLinkage)
        case _ if funcType.parameters != Nil || instantiation == scope => {
          if(funcType.parameters != Nil)
            func.setLinkage(LLVMLinkage.LLVMWeakODRLinkage)
          val specialization = new SignatureSubstitution
          for(spec <- (funcType.parameters zip sigvars))
            specialization.substitute(spec._1,spec._2)
          val entry = func.appendBasicBlock("entry")
          val builder = new LLVMInstructionBuilder
          builder.positionBuilderAtEnd(entry)
          new LLVMReturnInstruction(builder,body.specialize(specialization).compile(instantiation,builder))
        }
        case _ => func.setLinkage(LLVMLinkage.LLVMExternalLinkage)
      }
      func
    })
  })
  override val build: Memoize1[Module,Set[LLVMValue]] = Memoize1(instantiation =>
    specialize.values.foldLeft(Set.empty[LLVMValue])((set,specialization) => set + specialization(instantiation))
  )
}

class ExpressionBody(override val arguments: List[(String,MonoType)],
                     override val implicits: List[(String,MonoType)],
                     val result: Option[MonoType],
                     parent: Module,
                     mkBody: LexicalScope => Expression) extends FunctionBody {
  override val scope = new LexicalScope(parent,arguments)
  val body: Expression = mkBody(scope)
  override def bodyType: MonoType = result getOrElse body.expType
  override def bodyEffect: EffectPair = body.expEffect
  override def infer: SignatureSubstitution = {
    val inference = new LatticeUnificationInstance
    body.constrain(inference.constraints)
    result match {
      case Some(tau) => inference.constrain(new SubsumptionConstraint(body.expType,tau))
      case None => Unit
    }
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
    new ExpressionBody(args,impls,result.map(spec.solve(_)),parent,bod)
  }
  override def compile(instantiation: Module,builder: LLVMInstructionBuilder): LLVMValue = {
    val llvmArguments = new HashMap ++ builder.getInsertBlock.getParent.getParameters.toList.map(arg => (arg.getValueName,arg))
    scope.setArguments(llvmArguments)
    body.compile(builder,scope,instantiation)
  }
}
