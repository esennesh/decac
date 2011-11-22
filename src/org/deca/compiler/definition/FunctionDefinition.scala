package org.deca.compiler.definition

import scala.collection.immutable.Set
import scala.util.Memoize1
import org.jllvm._
import org.jllvm.bindings._
import org.deca.compiler.signature._
import org.deca.compiler.expression.Expression

trait FunctionDefinition extends Definition {
  val funcType: TypeConstructor
  val specialize: Memoize1[List[MonoSignature],Memoize1[Module,LLVMValue]] = Memoize1(sigvars => {
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
        new LLVMReturnInstruction(builder,compile(specialization,instantiation,builder))
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
  def compile(spec: SignatureSubstitution,instantiation: Module,builder: LLVMInstructionBuilder): LLVMValue
}

class ExpressionFunction(override val name: String,
                         override val scope: Module,
                         val arguments: List[(String,MonoMutability,MonoType)],
                         mkBody: LexicalScope => Expression) extends FunctionDefinition {
  val bodyScope: LexicalScope = new LexicalScope(scope,arguments.map(arg => (arg._1,arg._2,FreshArgument(arg._3))))
  val body: Expression = {
    val expression = mkBody(bodyScope)
    val inference = new LatticeUnificationInstance
    expression.constrain(inference.constraints)
    inference.solve
    expression.check(inference.constraints)
    val substitution = inference.solve
    expression.substitute(substitution)
    bodyScope.bindings.map(_.substitute(substitution))
    expression
  }
  override val funcType = {
    val arrow = new FunctionPointer(arguments.map(_._3),body.expType,body.expEffect.positive,body.expEffect.negative)
    new TypeExpressionConstructor(arrow.variables.toList,arrow)
  }
  override def compile(spec: SignatureSubstitution,instantiation: Module,builder: LLVMInstructionBuilder): LLVMValue =
    body.specialize(spec).compile(builder,bodyScope,instantiation)
}
