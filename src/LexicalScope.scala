package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import jllvm.LLVMInstructionBuilder
import jllvm.LLVMStackAllocation
import jllvm.LLVMLoadInstruction
import jllvm.LLVMStoreInstruction
import jllvm.LLVMValue
import jllvm.LLVMArgument

class UninferredLexicalBinding(n: String,s: UninferredLexicalScope,t: TauType) extends VariableBinding {
  override val name = n
  override val scope = s
  override val variableType: TauType = t
  
  def infer(substitution: TauSubstitution) = (name,substitution.solve(variableType))
}

class UninferredLexicalScope(p: Scope[_],binds: List[Tuple2[String,TauType]]) extends Scope[VariableBinding](p) {
  protected val bindings = binds.map(pair => new UninferredLexicalBinding(pair._1,this,pair._2))
  bindings.foreach(binding => declare(binding))
  def infer(substitution: TauSubstitution): LexicalScope = {
    val bound = bindings.map(binding => binding.infer(substitution))
    new LexicalScope(parent,bound)
  }
}

class LexicalBinding(n: String,s: LexicalScope,t: TauType) extends VariableBinding {
  override val name = n
  override val scope = s
  override val variableType = t match {
    case bvar: BetaVariable => bvar
    case gamma: GammaType => gamma
    case _ => throw new Exception("Generalized lexical binding has neither gamma type nor beta-variable type.")
  }
  def specialize(substitution: BetaSpecialization,arg: Option[LLVMArgument]) = (name,substitution.solve(variableType),arg)
}

class LexicalScope(p: Scope[_],bound: List[Tuple2[String,TauType]]) extends Scope[LexicalBinding](p) {
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

class SpecializedLexicalBinding(n: String,s: SpecializedLexicalScope,g: GammaType,arg: Option[LLVMArgument]) extends SpecializedVariableBinding {
  override val name = n
  override val scope = s
  override val variableType: GammaType = g
  protected var compiled: Option[LLVMValue] = arg
  
  def compile(builder: LLVMInstructionBuilder): LLVMValue = compiled match {
    case Some(allocation) => allocation
    case None => {
      val result = new LLVMStackAllocation(builder,variableType.compile,null,name)
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
    case argument: LLVMArgument => throw new Exception("Cannot store into a function argument!")
  }
}

class SpecializedLexicalScope(p: Scope[_],bound: List[Tuple3[String,GammaType,Option[LLVMArgument]]]) extends Scope[SpecializedLexicalBinding](p) {
  val bindings: List[SpecializedLexicalBinding] = bound.map(triple => new SpecializedLexicalBinding(triple._1,this,triple._2,triple._3))
  bindings.foreach(binding => declare(binding))
  def compile(builder: LLVMInstructionBuilder): List[LLVMValue] = bindings.map(binding => binding.compile(builder))
}
