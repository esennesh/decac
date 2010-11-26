package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import jllvm.LLVMInstructionBuilder
import jllvm.LLVMStackAllocation
import jllvm.LLVMLoadInstruction
import jllvm.LLVMStoreInstruction
import jllvm.LLVMValue
import jllvm.LLVMArgument

abstract class UninferredLexicalKind(t: TauType) {
  val variableType: TauType = t
}
case class UninferredLet(initializer: UninferredExpression) extends UninferredLexicalKind(initializer.expressionType)
case class UninferredArgument(argType: TauType) extends UninferredLexicalKind(argType)

class UninferredLexicalBinding(n: String,s: UninferredLexicalScope,k: UninferredLexicalKind) extends VariableBinding {
  override val name = n
  override val scope = s
  val kind = k
  override val variableType: TauType = kind.variableType
  
  def infer(substitution: TauSubstitution) = kind match {
    case UninferredLet(initializer) => (name,LexicalLet(initializer.substitute(substitution)))
    case UninferredArgument(_) => (name,LexicalArgument(substitution.solve(variableType)))
  }
}

class UninferredLexicalScope(p: Scope[_],binds: List[Tuple2[String,UninferredLexicalKind]]) extends Scope[VariableBinding](p) {
  val bindings = binds.map(pair => new UninferredLexicalBinding(pair._1,this,pair._2))
  bindings.foreach(binding => declare(binding))
  
  
  def infer(substitution: TauSubstitution): LexicalScope = {
    val bound = bindings.map(binding => binding.infer(substitution))
    new LexicalScope(parent,bound)
  }
}

abstract class LexicalKind(t: TauType) {
  val variableType: TauType = t
}
case class LexicalLet(initializer: Expression) extends LexicalKind(initializer.expressionType)
case class LexicalArgument(argType: TauType) extends LexicalKind(argType)

class LexicalBinding(n: String,s: LexicalScope,k: LexicalKind) extends VariableBinding {
  override val name = n
  override val scope = s
  val kind = k
  override val variableType = kind.variableType match {
    case bvar: BetaVariable => bvar
    case gamma: GammaType => gamma
    case _ => throw new Exception("Generalized lexical binding has neither gamma type nor beta-variable type.")
  }
  def specialize(substitution: BetaSpecialization,arg: Option[LLVMArgument]) = kind match {
    case LexicalLet(initializer) => {
      assert(arg == None)
      (name,new SpecializedLet(initializer.specialize(substitution)))
    }
    case LexicalArgument(_) => (name,new SpecializedArgument(substitution.solve(variableType),arg.get))
  }
}

class LexicalScope(p: Scope[_],bound: List[Tuple2[String,LexicalKind]]) extends Scope[LexicalBinding](p) {
  protected val bindings: List[LexicalBinding] = bound.map(pair => new LexicalBinding(pair._1,this,pair._2))
  bindings.foreach(binding => declare(binding))
  protected val specializations = new HashMap[BetaSpecialization,SpecializedLexicalScope]()
  def specialize(substitution: BetaSpecialization,args: Option[List[LLVMArgument]]): SpecializedLexicalScope = specializations.get(substitution) match {
    case None => {
      val bound = args match {
        case None => bindings.map(binding => binding.specialize(substitution,None))
        case Some(arguments) => bindings.zip(arguments).map(pair => pair._1.specialize(substitution,Some(pair._2)))
      }
      val result = new SpecializedLexicalScope(parent,bound)
      specializations.put(substitution,result)
      result
    }
    case Some(result) => result
  }
}

abstract class SpecializedKind(g: GammaType) {
  val variableType: GammaType = g
}
case class SpecializedLet(initializer: SpecializedExpression) extends SpecializedKind(initializer.expressionType)
case class SpecializedArgument(argType: GammaType,arg: LLVMArgument) extends SpecializedKind(argType)

class SpecializedLexicalBinding(n: String,s: SpecializedLexicalScope,k: SpecializedKind) extends SpecializedVariableBinding {
  override val name = n
  override val scope = s
  val kind = k
  override val variableType: GammaType = k.variableType
  protected var compiled: Option[LLVMValue] = None
  
  def compile(builder: LLVMInstructionBuilder): LLVMValue = compiled match {
    case Some(allocation) => allocation
    case None => {
      val result = new LLVMStackAllocation(builder,variableType.compile,null,name)
      val init = kind match {
        case SpecializedLet(initializer) => initializer.compile(builder,scope.parent)
        case SpecializedArgument(_,arg) => arg
      }
      new LLVMStoreInstruction(builder,init,result)
      compiled = Some(result)
      result
    }
  }
  
  override def load(builder: LLVMInstructionBuilder): LLVMValue = compile(builder) match {
    case stack: LLVMStackAllocation => new LLVMLoadInstruction(builder,stack,name)
    case argument: LLVMArgument => argument
  }
  
  override def store(builder: LLVMInstructionBuilder,value: LLVMValue): LLVMValue = compile(builder) match{
    case stack: LLVMStackAllocation => new LLVMStoreInstruction(builder,value,stack)
    case argument: LLVMArgument => throw new Exception("Cannot store into an immutable function argument!")
  }
}

class SpecializedLexicalScope(p: Scope[_],bound: List[Tuple2[String,SpecializedKind]]) extends Scope[SpecializedLexicalBinding](p) {
  val bindings: List[SpecializedLexicalBinding] = bound.map(pair => new SpecializedLexicalBinding(pair._1,this,pair._2))
  bindings.foreach(binding => declare(binding))
  def compile(builder: LLVMInstructionBuilder): List[LLVMValue] = bindings.map(binding => binding.compile(builder))
}
