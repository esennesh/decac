package org.deca.compiler.definition

import scala.collection.immutable.{Set,HashMap,Map}
import scala.util.Memoize1
import org.jllvm._
import org.jllvm.bindings._
import org.deca.compiler.signature._
import org.deca.compiler.expression.{EffectPair,Expression}

trait FunctionBody {
  val scope: LexicalScope
  def bodyType: MonoType
  def bodyEffect: EffectPair
  
  def infer: SignatureSubstitution
  def substitute(substitution: SignatureSubstitution): Unit
  def specialize(spec: SignatureSubstitution): FunctionBody
  def compile(instantiation: Module,builder: LLVMInstructionBuilder): LLVMValue
}

class FunctionDefinition(val name: String,
                         val scope: Module,
                         val arguments: List[(String,MonoType)],
                         val body: FunctionBody) extends Definition {
  val funcType: TypeConstructor = {
    val substitution = body.infer
    val arrow = substitution.solve[MonoType](new FunctionPointer(arguments.map(_._2),body.bodyType,body.bodyEffect.positive,body.bodyEffect.negative))
    val solvedArrow = substitution.solve(arrow)
    val result = new TypeExpressionConstructor(solvedArrow.variables.toList,solvedArrow)
    body.substitute(substitution)
    result
  }
  val specialize: Memoize1[List[MonoSignature],Memoize1[Module,LLVMFunction]] = Memoize1(sigvars => {
    val signature = funcType.represent(sigvars).asInstanceOf[FunctionPointer]
    Memoize1(instantiation => {
      val func = new LLVMFunction(instantiation.compiledModule,name + signature.toString,signature.compile)
      if(funcType.parameters != Nil || instantiation == scope) {
        if(funcType.parameters != Nil)
          func.setLinkage(LLVMLinkage.LLVMWeakODRLinkage)
        val specialization = new SignatureSubstitution
        for(spec <- (funcType.parameters zip sigvars))
          specialization.substitute(spec._1,spec._2)
        val entry = func.appendBasicBlock("entry")
        val builder = new LLVMInstructionBuilder
        builder.positionBuilderAtEnd(entry)
        new LLVMReturnInstruction(builder,body.specialize(specialization).compile(instantiation,builder))
        func
      }
      else
        func.setLinkage(LLVMLinkage.LLVMExternalLinkage)
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

class ExpressionBody(arguments: List[(String,MonoType)],
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
    body.check(inference.constraints)
    val substitution = inference.solve
    substitution
  }
  override def substitute(substitution: SignatureSubstitution): Unit = {
    scope.substitute(substitution)
    body.substitute(substitution)
  }
  override def specialize(spec: SignatureSubstitution): ExpressionBody = {
    val args = arguments.map(arg => (arg._1,spec.solve(arg._2)))
    val bod = (lexi: LexicalScope) => body.specialize(spec,lexi)
    new ExpressionBody(args,parent,bod)
  }
  override def compile(instantiation: Module,builder: LLVMInstructionBuilder): LLVMValue = {
    val llvmArguments = new HashMap ++ builder.getInsertBlock.getParent.getParameters.toList.map(arg => (arg.getValueName,arg))
    scope.setArguments(llvmArguments)
    body.compile(builder,scope,instantiation)
  }
}
