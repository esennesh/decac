package org.deca.compiler.definition

import scala.collection.immutable.Set
import scala.util.Memoize1
import org.jllvm._
import org.jllvm.bindings._
import org.deca.compiler.signature._

trait FunctionDefinition extends Definition {
  val funcType: TypeConstructor
  val specialize: Memoize1[List[SignatureVariable],Memoize1[Module,LLVMValue]] = Memoize1(sigvars => {
    val signature = funcType.represent(sigvars).asInstanceOf[FunctionPointer]
    Memoize1(instantiation => {
      val func = new LLVMFunction(instantiation.compiledModule,name + signature.toString,signature.compile)
      if(funcType.parameters != Nil || instantiation == scope) {
        if(funcType.parameters != Nil)
          func.setLinkage(LLVMLinkage.LLVMWeakODRLinkage)
        val entry = func.appendBasicBlock("entry")
        val builder = new LLVMInstructionBuilder
        builder.positionBuilderAtEnd(entry)
        new LLVMReturnInstruction(builder,compile(builder))
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
  def compile(builder: LLVMInstructionBuilder): LLVMValue
}
