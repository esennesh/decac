package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import jllvm.LLVMFunction
import jllvm.LLVMInstructionBuilder
import jllvm.LLVMReturnInstruction
import jllvm.LLVMGetElementPointerInstruction
import jllvm.LLVMStoreInstruction
import jllvm.LLVMStackAllocation
import jllvm.LLVMConstantInteger
import jllvm.LLVMValue

abstract class ConstructorDefinition(m: Module,tp: TaggedProduct) extends Definition {
  override val name = tp.name.name match {
    case None => throw new Exception("Cannot define constructor function for unnamed tagged product.")
    case Some(str) => str
  }
  override val scope: Module = {m.define(this); m}
  val arguments: List[Tuple2[String,TauType]]
  val functionType: SigmaType = {
    val argTypes = arguments.map(arg => arg._2)
    val resultType = new SumType(tp :: Nil)
    (new FunctionArrow(argTypes,resultType)).generalize(new TauSubstitution)
  }
  val bodyScope: LexicalScope = {
    val binds = arguments.map(arg => (arg._1,LexicalArgument(arg._2)))
    new LexicalScope(scope,binds)
  }
  val bodies: HashMap[RecordMember,Expression]
  
  protected val specializations: Map[List[GammaType],SpecializedConstructor] = new HashMap[List[GammaType],SpecializedConstructor]()
  
  functionType match {
    case arrow: FunctionArrow => specialize(Nil)
    case _ => {}
  }
  assert(bodies.size == tp.record.length)
  
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
  
  def specialize(specialization: List[GammaType]): SpecializedConstructor = specializations.get(specialization) match {
    case Some(sf) => sf
    case None => {
      val specializer = functionType.specialize(specialization)
      val result = new SpecializedConstructor(this,specializer)
      specializations.put(specialization,result)
      result
    }
  }
}

class DefaultConstructor(m: Module,tp: TaggedProduct) extends ConstructorDefinition(m,tp) {
  override val arguments: List[Tuple2[String,TauType]] = {
    tp.record.fields.zipWithIndex.map(field => (field._1.name match { case Some(str) => str case None => "_" + field._2.toString },field._1.tau))
  }
  override val bodies: HashMap[RecordMember,Expression] = {
    val result = new HashMap[RecordMember,Expression]()
    for(arg <- arguments) {
      val declaration = bodyScope.typedLookup(arg._1)
      assert(TauOrdering.equiv(arg._2,declaration.variableType))
      val member = tp.record.fields.find(mem => mem.name == Some(arg._1) || mem.name == None && mem.tau == arg._2).get
      result.put(member,new VariableExpression(arg._1 :: Nil,declaration.variableType))
    }
    result
  }
}

class ExplicitConstructor(m: Module,tp: TaggedProduct,args: List[Tuple2[String,TauType]],body: (LexicalScope,RecordMember) => Expression) extends ConstructorDefinition(m,tp) {
  override val arguments: List[Tuple2[String,TauType]] = args
  override val bodies: HashMap[RecordMember,Expression] = {
    val result = new HashMap[RecordMember,Expression]()
    for(field <- tp.record.fields)
      result.put(field,body(bodyScope,field))
    result
  }
}

class SpecializedConstructor(org: ConstructorDefinition,specializer: BetaSpecialization) {
  val original = org
  val name: String = org.name
  
  val functionType: FunctionArrow = specializer.solve(original.functionType.body) match {
    case frho: FunctionArrow => frho
    case _ => throw new Exception("Specializing a function's type should never yield anything but an arrow type.")
  }
  protected val range = functionType.range.asInstanceOf[SumType].sumCases.head
  
  val function = new LLVMFunction(original.scope.compiledModule,name + "constructor" + functionType.toString,functionType.compile)
  protected var compiled: Boolean = false
  val bodyScope = org.bodyScope.specialize(specializer,Some(function.getParameters.toList))
  val bodies = org.bodies.mapValues(expr => expr.specialize(specializer))
  
  def compile(builder: LLVMInstructionBuilder): LLVMFunction = {
    if(compiled == false) {
      val entry = function.appendBasicBlock("entry")
      builder.positionBuilderAtEnd(entry)
      bodyScope.compile(builder)
      val result = new LLVMStackAllocation(builder,functionType.range.asInstanceOf[GammaType].compile,null,"result")
      val tagIndices = (LLVMConstantInteger.constantInteger(Nat.compile,0,false).asInstanceOf[LLVMValue] :: LLVMConstantInteger.constantInteger(Nat.compile,0,false).asInstanceOf[LLVMValue] :: Nil).toArray
      val tagPointer = new LLVMGetElementPointerInstruction(builder,result,tagIndices,"gep")
      new LLVMStoreInstruction(builder,LLVMConstantInteger.constantInteger(functionType.range.asInstanceOf[SumType].tagRepresentation,range.constructor,false),tagPointer)
      for(body <- bodies) {
        val index = range.record.fields.zipWithIndex.find(pair => pair._1 == body._1).get._2 + 1
        val indices = (LLVMConstantInteger.constantInteger(Nat.compile,0,false).asInstanceOf[LLVMValue] :: LLVMConstantInteger.constantInteger(Nat.compile,index,false).asInstanceOf[LLVMValue] :: Nil).toArray
        val memberPointer = new LLVMGetElementPointerInstruction(builder,result,indices,"gep")
        new LLVMStoreInstruction(builder,body._2.compile(builder,bodyScope),memberPointer)
      }
      new LLVMReturnInstruction(builder,result)
      compiled = true
    }
    function
  }
}
