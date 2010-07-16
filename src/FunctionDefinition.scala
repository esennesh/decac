package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import jllvm.LLVMFunction

class FunctionDefinition(m: Module,n: String,ft: FunctionRho,b: BlockExpression) extends Definition {
  override val name: String = n
  override val scope: Module = { m.declare(this) ; m}
  val functionType: FunctionRho = ft
  val body: BlockExpression = b
  val specializations: Map[List[RhoType],BlockExpression] = new HashMap[List[RhoType],BlockExpression]()
  def specialize(types: List[RhoType]): Boolean
  def compile(specialization: List[RhoType]): Option[LLVMFunction] = {
    None
  }
}
