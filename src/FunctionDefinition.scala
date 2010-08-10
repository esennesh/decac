package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder
import jllvm.LLVMReturnInstruction

class UninferredFunction(m: Module,n: String,ft: FunctionRho,b: UninferredBlock) {
  val name: String = n
  val scope: Module = {assert(b.scope == m) ; m}
  val functionType: FunctionRho = ft
  val functionBody: UninferredBlock = b
  
  def infer: FunctionDefinition = {
    val rui = new RangeUnificationInstance
    functionBody.constrain(rui)
    val substitution = new SigmaSubstitution(rui.solve)
    val inferredType = substitution.generalize(functionType)
    val body = functionBody.substitute(substitution,true)
    new FunctionDefinition(m,n,inferredType,body)
  }
}
  
class FunctionDefinition(m: Module,n: String,ft: SigmaType,b: BlockExpression) extends Definition {
  override val name: String = n
  override val scope: Module = { m.declare(this) ; m}
  val functionType: SigmaType = ft
  val functionBody: BlockExpression = b
  protected val specializations: Map[List[RhoType],SpecializedFunction] = new HashMap[List[RhoType],SpecializedFunction]()
  def specialize(specialization: List[RhoType]): SpecializedFunction = specializations.get(specialization) match {
    case Some(sf) => sf
    case None => functionType match {
      case universal: ForallSigma => {
        val specialized: FunctionRho = universal.specialize(specialization) match {
          case f: FunctionRho => f
          case _ => throw new Exception("Specializing a function's type should never yield anything but a function-rho type.")
        }
        val result = new SpecializedFunction(this,specialized,functionBody.specialize(specialization))
        specializations.put(specialization,result)
        return result
      }
      case frho: FunctionRho => {
        assert(specialization.length == 0)
        val result = new SpecializedFunction(this,frho,functionBody.specialize(Nil))
        specializations.put(Nil,result)
        return result
      }
    }
  }
}

class SpecializedFunction(f: FunctionDefinition,frho: FunctionRho,b: SpecializedBlock) {
  val originalFunction = f
  val functionType: FunctionRho = frho
  val functionBody: SpecializedBlock = b
  def compile(builder: LLVMInstructionBuilder): LLVMFunction = {
    val result = originalFunction.scope.compiledModule.getNamedFunction(originalFunction.name + functionType.mangle)
    val entry = result.getEntryBasicBlock
    builder.positionBuilderAtEnd(entry)
    val funcResult = functionBody.compile(builder)
    new LLVMReturnInstruction(builder,funcResult)
    return result
  }
}
