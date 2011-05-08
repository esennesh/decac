package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import jllvm.llvm.LLVMLinkage
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder

class ExternalFunction(s: TypeBindingScope,n: String,args: List[Tuple2[String,GammaType]],result: GammaType) extends FunctionDefinition with SpecializedFunction {
  override val name: String = n
  override val scope: Module = { s.owner.define(this) ; s.owner }
  override val generic = this
  override val signature: FunctionArrow = new FunctionArrow(args.map(arg => arg._2),result)
  protected val specs: Map[List[GammaType],ExternalFunction] = new HashMap[List[GammaType],ExternalFunction]()
  specs.put(Nil,this)
  val function = {
    val f = new LLVMFunction(scope.compiledModule,name + signature.toString,signature.compile)
    f.setLinkage(LLVMLinkage.LLVMExternalLinkage)
    f
  }
  
  def specializeScope(caller: Scope[_]): FunctionArrow = {
    signature.scopeMap(fScope => fScope match {
      case args: ArgumentScopeType => new ArgumentScopeType(args.function,Some(caller.scopeType))
      case _ => fScope
    }).asInstanceOf[FunctionArrow]
  }
  def specialize(specialization: List[GammaType]): ExternalFunction = specs.get(specialization).get
  def specialized: Iterable[SpecializedFunction] = specs.values
  
  def compile: LLVMFunction = function
}
