package decac

abstract class UninferredCall(arrow: ArrowType,arguments: List[UninferredExpression]) extends UninferredExpression(arrow.range) {
  assert(arrow.domain.length == arguments.length)
  val signature = arrow
  override def children: List[UninferredExpression] = arguments
  override def substitute(substitution: TauSubstitution): CallExpression
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    for(pair <- signature.domain.zip(arguments))
      rui.constrain(new LesserEq(pair._2.expressionType,pair._1))
    rui
  }
}

class UninferredDefinitionCall(func: FunctionDefinition,arguments: List[UninferredExpression]) extends UninferredCall(func.functionType.freshlyInstantiate.asInstanceOf[FunctionArrow],arguments) {
  val definition = func
  
  override def substitute(substitution: TauSubstitution): DefinitionCallExpression = {
    val substitutedSignature = substitution.solve(signature.asInstanceOf[FunctionArrow])
    new DefinitionCallExpression(definition,substitutedSignature,substituteTypes(substitution)._2)
  }
}

class UninferredExpressionCall(func: UninferredExpression,arguments: List[UninferredExpression]) extends UninferredCall(new ClosureArrow(arguments.map(tau => new TauVariable),new TauVariable,None),arguments) {
  val function = func
  override def children: List[UninferredExpression] = function :: arguments
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    super.constrain(rui)
    val sigType = signature match {
      case fpsig: FunctionArrow => fpsig
      case closure: ClosureArrow => closure
    }
    rui.constrain(new Equal(function.expressionType,sigType))
    rui
  }
  
  override def substitute(substitution: TauSubstitution): ExpressionCallExpression = {
    val substitutedFunction = function.substitute(substitution)
    val substitutedArguments = substituteTypes(substitution)._2
    new ExpressionCallExpression(substitutedFunction,substitutedArguments)
  }
}
