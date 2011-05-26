package decac

import jllvm._

trait Binding extends Scopeable {
  val symbolType: SigmaType
  def substitute(substitution: TauSubstitution): Unit
}

class ModuleVariableDefinition(m: Module,n: String,value: ConstantExpression,mut: Boolean) extends Definition with SpecializedVariableBinding {
  override val name: String = n
  override val scope: Module = { m.define(this) ; m}
  val initialValue: ConstantExpression = value
  protected var compiledGlobal: Option[LLVMGlobalVariable] = None
  override val variableType: GammaType = value.expressionType
  val mutable = mut
  def build: LLVMValue = compiledGlobal match{
    case Some(global) => global
    case None => {
      val global = scope.compiledModule.addGlobal(variableType.compile,name)
      global.setInitializer(initialValue.build)
      global.setThreadLocal(mutable)
      global.setConstant(!mutable)
      compiledGlobal = Some(global)
      global
    }
  }
  override def compile(builder: LLVMInstructionBuilder): LLVMValue = build
  override def load(builder: LLVMInstructionBuilder): LLVMValue = {
    build
    new LLVMLoadInstruction(builder,compiledGlobal.get,name)
  }
  override def store(builder: LLVMInstructionBuilder,value: LLVMValue): LLVMValue = {
    build
    new LLVMStoreInstruction(builder,value,compiledGlobal.get)
  }
}
