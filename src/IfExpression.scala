package decac

import jllvm._
import jllvm.llvm._
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

class UninferredIf(c: UninferredExpression,t: UninferredExpression,e: Option[UninferredExpression]) extends UninferredExpression {
  val condition = c
  val then = t
  val otherwise = e
  override val writable = false
  override val expressionType: TauType = new TauVariable
  override val children = List(condition,then) ++ (otherwise match {
    case Some(exp) => (exp :: Nil)
    case None => Nil
  })
  
  override def substitute(substitution: TauSubstitution): IfExpression = {
    val c = condition.substitute(substitution)
    val t = then.substitute(substitution)
    val e = otherwise match {
      case Some(exp) => Some(exp.substitute(substitution))
      case None => None
    }
    new IfExpression(c,t,e,substituteTypes(substitution)._1)
  }
  override def constrain(rui: RangeUnificationInstance): RangeUnificationInstance = {
    rui.constrain(new LesserEq(condition.expressionType,BuiltInSums.BooleanGamma))
    condition.constrain(rui)
    then.constrain(rui)
    otherwise match {
      case Some(exp) => {
        rui.constrain(new LesserEq(then.expressionType,expressionType))
        rui.constrain(new LesserEq(exp.expressionType,expressionType))
        exp.constrain(rui)
      }
      case None => rui.constrain(new Equal(expressionType,UnitGamma))
    }
    rui
  }
}

class IfExpression(c: Expression,t: Expression,e: Option[Expression],ty: TauType) extends Expression {
  val condition = c
  val then = t
  val otherwise = e
  override val expressionType: TauType = ty
  override val children = List(condition,then) ++ (otherwise match {
    case Some(exp) => (exp :: Nil)
    case None => Nil
  })
  val specializations: Map[BetaSpecialization,SpecializedIf] = new HashMap[BetaSpecialization,SpecializedIf]()
  override def specialize(specialization: BetaSpecialization): SpecializedIf = specializations.get(specialization) match {
    case Some(si) => si
    case None => {
      val rc = condition.specialize(specialization)
      val rt = then.specialize(specialization)
      val re = otherwise match {
        case Some(exp) => Some(exp.specialize(specialization))
        case None => None
      }
      val result = new SpecializedIf(rc,rt,re,specialization.solve(expressionType))
      specializations.put(specialization,result)
      result
    }
  }
}

class SpecializedIf(c: SpecializedExpression,t: SpecializedExpression,e: Option[SpecializedExpression],typ: GammaType) extends SpecializedExpression {
  val condition = c
  val then = t
  val otherwise = e
  override val expressionType: GammaType = typ
  override val children = List(condition,then) ++ (otherwise match {
    case Some(exp) => (exp :: Nil)
    case None => Nil
  })
  
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = otherwise match {
    case Some(expr) => {
      val comparator = condition.compile(builder,scope)
      val merge = builder.getInsertBlock.getParent.appendBasicBlock("merge")
      val thenBlock = merge.insertBasicBlockBefore("then")
      val elseBlock = merge.insertBasicBlockBefore("else")
      new LLVMBranchInstruction(builder,comparator,thenBlock,elseBlock)
      
      builder.positionBuilderAtEnd(thenBlock)
      val thenValue = (new ImplicitUpcast(then,expressionType)).compile(builder,scope)
      new LLVMBranchInstruction(builder,merge)
      
      builder.positionBuilderAtEnd(elseBlock)
      val elseValue = (new ImplicitUpcast(expr,expressionType)).compile(builder,scope)
      new LLVMBranchInstruction(builder,merge)
      
      builder.positionBuilderAtEnd(merge)
      val phi = new LLVMPhiNode(builder,expressionType.compile,"ifphi")
      phi.addIncoming((thenValue :: elseValue :: Nil).toArray,(thenBlock :: elseBlock :: Nil).toArray)
      phi
    }
    case None => {
      val comparator = condition.compile(builder,scope)
      val merge = builder.getInsertBlock.getParent.appendBasicBlock("merge")
      val thenBlock = merge.insertBasicBlockBefore("then")
      new LLVMBranchInstruction(builder,comparator,thenBlock,merge)
      
      builder.positionBuilderAtEnd(thenBlock)
      val value = (new ImplicitUpcast(then,expressionType)).compile(builder,scope)
      new LLVMBranchInstruction(builder,merge)
      
      builder.positionBuilderAtEnd(merge)
      val phi = new LLVMPhiNode(builder,expressionType.compile,"ifphi")
      phi.addIncoming((value :: Nil).toArray,(thenBlock :: Nil).toArray)
      phi
    }
  }
}
