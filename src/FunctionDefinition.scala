package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder
import jllvm.LLVMReturnInstruction

class UninferredFunction(m: Module,n: String,args: List[Tuple2[String,TauType]],b: (UninferredLexicalScope) => UninferredBlock) {
  val name: String = n
  val scope: Module = m
  val fScope = new UninferredLexicalScope(scope,args)
  val arguments = args.map(arg => new UninferredLexicalBinding(arg._1,fScope,arg._2))
  val body = b(fScope)
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
  val functionType: SigmaType = original.functionType.generalize(substitution)
  val fScope = original.fScope.infer(substitution)
  val body = original.body.substitute(substitution)
  
  protected val specializations: Map[List[GammaType],SpecializedFunction] = new HashMap[List[GammaType],SpecializedFunction]()
  
  functionType match {
    case arrow: FunctionArrow => specialize(Nil)
  }
  
  def specialized = specializations.values
  
  def specializeScope(caller: Scope[_]): SigmaType = functionType match {
    case beta: BetaType => beta.scopeMap(fScope => fScope match {
      case args: ArgumentScopeType => new ArgumentScopeType(args.function,Some(caller match {
        case mod: Module => new GlobalScopeType(Some(mod))
        case lexical: UninferredLexicalScope => new LexicalScopeType(lexical)
      }))
      case _ => fScope
    })
    case arrow: FunctionArrow => arrow.scopeMap(fScope => fScope match {
      case args: ArgumentScopeType => new ArgumentScopeType(args.function,Some(caller match {
        case mod: Module => new GlobalScopeType(Some(mod))
        case lexical: UninferredLexicalScope => new LexicalScopeType(lexical)
      }))
      case _ => fScope
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

class SpecializedFunction(org: FunctionDefinition,specializer: BetaSpecialization) {
  val original = org
  val name: String = org.name
  val functionType: FunctionArrow = specializer.solve(original.functionType.body) match {
    case frho: FunctionArrow => frho
    case _ => throw new Exception("Specializing a function's type should never yield anything but an arrow type.")
  }
  val function = new LLVMFunction(original.scope.compiledModule,name + functionType.mangle,functionType.compile)
  var compiled: Boolean = false
  val fScope = org.fScope.specialize(specializer,Some(function.getParameters.toList))
  val body = original.body.specialize(specializer)
  
  def compile(builder: LLVMInstructionBuilder): LLVMFunction = {
    if(compiled == false) {
      fScope.compile(builder)
      val entry = function.appendBasicBlock("entry")
      builder.positionBuilderAtEnd(entry)
      val result = body.compile(builder,fScope)
      new LLVMReturnInstruction(builder,result)
    }
    function
  }
}
