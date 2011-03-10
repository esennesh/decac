package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import jllvm._

abstract class ConstructorDefinition(m: Module,tp: TaggedProduct,args: List[Tuple2[String,TauType]]) extends FunctionDefinition {
  override val name = tp.name.name match {
    case None => throw new Exception("Cannot define constructor function for unnamed tagged product.")
    case Some(str) => str
  }
  override val scope: Module = {m.define(this); m}
  val arguments: List[Tuple2[String,TauType]] = args
  protected val generalization = new TauSubstitution
  override val signature: SigmaType = {
    val argTypes = arguments.map(arg => arg._2)
    val resultType = new SumType(tp :: Nil)
    (new FunctionArrow(argTypes,resultType)).generalize(generalization)
  }
  val bodyScope: LexicalScope = {
    val binds = arguments.map(arg => (arg._1,LexicalArgument(generalization.solve(arg._2))))
    new LexicalScope(scope,binds)
  }
  val bodies: HashMap[RecordMember,Expression] = new HashMap[RecordMember,Expression]()
  
  protected val specializations: Map[List[GammaType],SpecializedConstructor] = new HashMap[List[GammaType],SpecializedConstructor]()
  
  signature match {
    case arrow: FunctionArrow => specialize(Nil)
    case _ => {}
  }
  
  override def specialized = specializations.values.map(func => func.asInstanceOf[SpecializedFunction])
  
  override def specializeScope(caller: Scope[_]): FunctionArrow = {
    assert(signature.body.isInstanceOf[FunctionArrow])
    signature.body.asInstanceOf[FunctionArrow].scopeMap(fScope => fScope match {
      case args: ArgumentScopeType => new ArgumentScopeType(args.function,Some(caller.scopeType))
      case _ => fScope
    }).asInstanceOf[FunctionArrow]
  }
  
  override def specialize(specialization: List[GammaType]): SpecializedConstructor = {
    for((key,value) <- specializations)
      if(specialization.zip(key).forall(pair => TauOrdering.equiv(pair._1,pair._2)))
        return value
    //So we didn't find a specialization already in the map.
    val specializer = signature.specialize(specialization)
    val result = new SpecializedConstructor(this,specializer)
    specializations.put(specialization,result)
    result
  }
}

class DefaultConstructor(m: Module,tp: TaggedProduct) extends ConstructorDefinition(m,tp,tp.record.fields.zipWithIndex.map(field => (field._1.name match { case Some(str) => str case None => "_" + field._2.toString },field._1.tau))) {
  for(arg <- arguments) {
    val declaration = bodyScope.typedLookup(arg._1)
    assert(TauOrdering.equiv(arg._2,declaration.variableType))
    val member = tp.record.fields.find(mem => mem.name == Some(arg._1) || mem.name == None && mem.tau == arg._2).get
    bodies.put(member,new VariableExpression(arg._1 :: Nil,declaration.variableType))
  }
  assert(bodies.size == tp.record.length)
}

class ExplicitConstructor(m: Module,tp: TaggedProduct,args: List[Tuple2[String,TauType]],body: (LexicalScope,RecordMember) => Expression) extends ConstructorDefinition(m,tp,args) {
  for(field <- tp.record.fields)
    bodies.put(field,body(bodyScope,field))
  assert(bodies.size == tp.record.length)
}

class SpecializedConstructor(org: ConstructorDefinition,specializer: BetaSpecialization) extends SpecializedFunction {
  val original = org
  val name: String = org.name
  
  override val signature: FunctionArrow = specializer.solve(original.signature.body).asInstanceOf[FunctionArrow]
  protected val range = signature.range.asInstanceOf[SumType].sumCases.head
  
  val function = new LLVMFunction(original.scope.compiledModule,name + "constructor" + signature.toString,signature.compile)
  protected var compiled: Boolean = false
  val bodyScope = org.bodyScope.specialize(specializer,Some(function.getParameters.toList))
  val bodies = org.bodies.mapValues(expr => expr.specialize(specializer))
  
  def compile(builder: LLVMInstructionBuilder): LLVMFunction = {
    if(compiled == false) {
      compiled = true
      val entry = function.appendBasicBlock("entry")
      builder.positionBuilderAtEnd(entry)
      bodyScope.compile(builder)
      
      val tag = LLVMConstantInteger.constantInteger(signature.range.asInstanceOf[SumType].tagRepresentation,range.constructor,false)
      val rangeType = signature.range.asInstanceOf[SumType]
      val result = if(rangeType.enumeration)
        tag
      else {
        val variant = new LLVMInsertValueInstruction(builder,new LLVMUndefinedValue(rangeType.compile),tag,0,"variant")
        var contents: LLVMValue = new LLVMUndefinedValue(rangeType.sumCases.apply(0).record.compile)
        for(body <- bodies) {
          val index = range.record.fields.zipWithIndex.find(pair => pair._1.name == body._1.name).get._2
          contents = new LLVMInsertValueInstruction(builder,contents,body._2.compile(builder,bodyScope),index,"variant_field")
        }
        new LLVMInsertValueInstruction(builder,variant,contents,1,"filled_variant")
      }
      new LLVMReturnInstruction(builder,result)
    }
    function
  }
}
