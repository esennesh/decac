package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder
import jllvm.LLVMReturnInstruction

class UninferredFunction(m: Module,n: String,args: List[Tuple2[String,TauType]],b: (LexicalScope) => UninferredBlock) {
  val name: String = n
  val scope: Module = m
  val functionScope = new LexicalScope(scope)
  val arguments = args.map(arg => new LexicalBinding(arg._1,functionScope,arg._2))
  val body = b(functionScope)
  val functionType = new FunctionArrow(arguments.map(arg => arg.variableType),body.expressionType)
  
  def infer: FunctionDefinition = {
    val rui = new RangeUnificationInstance(Some(scope))
    body.constrain(rui)
    new FunctionDefinition(this,rui.solve)
  }
}

class FunctionDefinition(original: UninferredFunction,substitution: TauSubstitution) extends Definition {
  override val name: String = original.name
  override val scope: Module = {original.scope.define(this) ; original.scope}
  val functionScope = original.functionScope
  val functionType: SigmaType = original.functionType.generalize(substitution)
  val arguments = original.arguments
  val body = original.body.substitute(substitution)
  
  protected val specializations: Map[List[GammaType],SpecializedFunction] = new HashMap[List[GammaType],SpecializedFunction]()
  
  def specializeScope(caller: Scope[_]): SigmaType = functionType match {
    case beta: BetaType => beta.scopeMap(scope => scope match {
      case args: ArgumentScopeType => new ArgumentScopeType(args.function,Some(caller match {
        case mod: Module => new GlobalScopeType(Some(mod))
        case lexical: LexicalScope => new LexicalScopeType(lexical)
      }))
      case _ => scope
    })
    case arrow: FunctionArrow => arrow.scopeMap(scope => scope match {
      case args: ArgumentScopeType => new ArgumentScopeType(args.function,Some(caller match {
        case mod: Module => new GlobalScopeType(Some(mod))
        case lexical: LexicalScope => new LexicalScopeType(lexical)
      }))
      case _ => scope
    })
    case _ => throw new Exception("Why does a function have something other than an arrow type or generalized arrow type?")
  }
  
  def specialize(specialization: List[GammaType]): SpecializedFunction = specializations.get(specialization) match {
    case Some(sf) => sf
    case None => {
      val specializer = functionType.specialize(specialization)
      val result = new SpecializedFunction(this,specializer)
      specializations.put(specialization,result)
      result
    }
  }
}

class SpecializedFunction(original: FunctionDefinition,specializer: BetaSpecialization) {
  val module: Module = original.scope
  val name: String = original.name
  val functionType: FunctionArrow = specializer.solve(original.functionType.body) match {
    case frho: FunctionArrow => frho
    case _ => throw new Exception("Specializing a function's type should never yield anything but an arrow type.")
  }
  val specialization = specializer
  val arguments = original.arguments.map(argument => (argument.name,specialization.solve(argument.variableType)))
  val body = original.body.specialize(specialization)
  
  def compile(builder: LLVMInstructionBuilder): LLVMFunction = {
    val result = module.compiledModule.getNamedFunction(name + functionType.mangle)
    val entry = result.getEntryBasicBlock
    builder.positionBuilderAtEnd(entry)
    val bodyScope = new LexicalScope(module)
    arguments.foreach(arg => new LexicalBinding(arg._1,bodyScope,arg._2))
    val funcResult = body.compile(builder,bodyScope)
    new LLVMReturnInstruction(builder,funcResult)
    return result
  }
}
