package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder
import jllvm.LLVMReturnInstruction

class UninferredFunction(m: Module,n: String,ft: FunctionArrow,b: UninferredBlock) {
  val name: String = n
  val scope: Module = {assert(b.scope == m) ; m}
  val functionType: FunctionArrow = ft
  val functionBody: UninferredBlock = b
  
  def infer: FunctionDefinition = {
    val rui = new RangeUnificationInstance
    functionBody.constrain(rui)
    val substitution = rui.solve
    val inferredType = functionType.generalize(substitution)
    val body = functionBody.substitute(substitution)
    new FunctionDefinition(m,n,inferredType,body)
  }
}
  
class FunctionDefinition(m: Module,n: String,ft: SigmaType,b: BlockExpression) extends Definition {
  override val name: String = n
  override val scope: Module = { m.declare(this) ; m}
  val functionType: SigmaType = ft
  val functionBody: BlockExpression = b
  protected val specializations: Map[List[GammaType],SpecializedFunction] = new HashMap[List[GammaType],SpecializedFunction]()
  def specialize(specialization: List[GammaType]): SpecializedFunction = specializations.get(specialization) match {
    case Some(sf) => sf
    case None => functionType match {
      case universal: BetaType => {
        val specializer = universal.specialize(specialization)
        val specialized: FunctionArrow = specializer.solve(functionType.body) match {
          case f: FunctionArrow => f
          case _ => throw new Exception("Specializing a function's type should never yield anything but an arrow type.")
        }
        val result = new SpecializedFunction(this,specialized,functionBody.specialize(specializer))
        specializations.put(specialization,result)
        return result
      }
      case frho: FunctionArrow => {
        assert(specialization.length == 0)
        val result = new SpecializedFunction(this,frho,functionBody.specialize(new SigmaSubstitution))
        specializations.put(Nil,result)
        return result
      }
    }
  }
}

class SpecializedFunction(f: FunctionDefinition,frho: FunctionArrow,b: SpecializedBlock) {
  val originalFunction = f
  val functionType: FunctionArrow = frho
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
