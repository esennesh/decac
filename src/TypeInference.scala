package decac;

import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

class Generalization(x: RhoType,y: Option[BetaType]) {
  var _1: RhoType = x
  var _2: Option[BetaType] = y
}

abstract class Substitution[A <: TauVariable,B <: TauType] {
  val queue: Queue[Tuple2[A,B]] = new Queue[Tuple2[A,B]]()

  def solve(tau: TauType): TauType = tau match {
    case alpha: TauVariable => queue.foldLeft[TauType](alpha)((result: TauType,sub: Tuple2[TauVariable,TauType]) => if(sub._1.equals(result)) sub._2 else result) match {
      case range: GammaRange => solve(if(range.lowerBound == BottomGamma) range.upperBound else range.lowerBound)
      case tau: TauType => tau
    }
    case rho: RhoType => rho.map(tau => solve(tau))
    case _ => tau
  }
}

class TauSubstitution extends Substitution[TauVariable,TauType] {
  def substitute(x: TauVariable,y: TauType): Unit = {
    queue.enqueue((x,y))
    y match {
      case y: GammaRange => if(y.lowerBound.equals(y.upperBound)) substitute(y,y.lowerBound)
      case _ => {}
    }
  }
}

class BetaInstantiation extends Substitution[BetaVariable,TauType] {
  def substitute(x: BetaVariable,y: TauType): Unit = {
    queue.enqueue((x,y match { case y: GammaRange => y.lowerBound case _ => y }))
  }
}

class BetaSpecialization extends Substitution[BetaVariable,GammaType] {
  def substitute(x: BetaVariable,y: GammaType): Unit = queue.enqueue((x,y))
  
  override def solve(tau: TauType): GammaType = tau match {
    case range: GammaRange => solve(range.lowerBound)
    case alpha: BetaVariable => {
      val result = queue.foldLeft[Option[GammaType]](None)((x: Option[GammaType],sub: Tuple2[BetaVariable,GammaType]) => x match { case Some(gamma) => Some(gamma) case None => if(sub._1 == alpha) Some(sub._2) else None })
      result match {
        case Some(gamma) => gamma
        case None => throw new TypeException("Had no specialization for beta variable " + alpha.toString + ".")
      }
    }
    case rho: RhoType => rho.map(tau => solve(tau))
    case gamma: GammaType => gamma
  }
}

class ConstraintSet {
  val stack: Stack[Constraint] = new Stack[Constraint]()
  
  def substitute(x: TauVariable,y: TauType): Unit = {
    for(constraint <- stack)
      constraint.substitute(x,y)
  }
  
  def push(c: Constraint): Unit = stack.push(c)
  
  def pop: Constraint = stack.pop
  
  def isEmpty = stack.isEmpty
}

abstract class Assumption(x: TauVariable,y: TauVariable)
case class Subtype(x: TauVariable,y: TauVariable) extends Assumption(x,y)
case class Equality(x: TauVariable,y: TauVariable) extends Assumption(x,y)

class RangeUnificationInstance(scope: Option[Module]) {
  protected val constraints = new ConstraintSet()
  val assumptions = new Stack[Assumption]()
  protected val result = new TauSubstitution
  scope match {
    case Some(module) => SigmaLattice.addModule(module)
    case None => None
  }
  
  def constrain(c: Constraint) = {
    constraints.push(c)
    c.alpha match {
      case gamma: GammaType => SigmaLattice.add(gamma)
      case _ => {}
    }
    c.beta match {
      case gamma: GammaType => SigmaLattice.add(gamma)
      case _ => {}
    }
  }
  
  def substitute(x: TauVariable,y: TauType): Unit = {
    result.substitute(x,y)
    constraints.substitute(x,y)
  }
  
  def solve: TauSubstitution = {
    while(constraints.isEmpty != true) {
      val constraint = constraints.pop
      try
        constraint.infer(this)
      catch {
        case te: TypeException => throw new TypeException(constraint.toString + "; " + te.error)
        case e: Exception => throw e
      }
    }
    return result
  }
}

abstract class Constraint(x: TauType,y: TauType) {
  var alpha = x
  var beta = y
  
  def substitute(from: TauVariable,to: TauType): Unit = {
    alpha match {
      case a: TauVariable => if(a.equals(from)) alpha = to
      case a: RhoType => a.replace(from,to)
      case _ => alpha
    }
    beta match {
      case b: TauVariable => if(b.equals(from)) beta = to
      case b: RhoType => b.replace(from,to)
      case _ => beta
    }
  }
  
  def infer(rui: RangeUnificationInstance): Unit
  
  override def toString: String
}

class LesserEq(x: TauType,y: TauType) extends Constraint(x,y) {
  override def infer(rui: RangeUnificationInstance): Unit = (alpha,beta) match {
    //Cases for handling ranges and variables.
    case (alpha: GammaRange,beta: GammaRange) => if(alpha != beta) {
      rui.substitute(alpha,alpha.refine(None,Some(SigmaLattice.meet(alpha.upperBound,beta.lowerBound,rui))))
      rui.substitute(beta,beta.refine(Some(SigmaLattice.join(alpha.upperBound,beta.lowerBound,rui)),None))
    }
    case (alpha: GammaRange,beta: TauVariable) => rui.substitute(beta,alpha)
    case (alpha: TauVariable,beta: GammaRange) => rui.substitute(alpha,beta)
    case (alpha: TauVariable,beta: TauVariable) => if(alpha != beta) {
      if(rui.assumptions.contains(Subtype(beta,alpha)))
        throw new TypeException("Assumption " + beta.toString + " <: " + alpha.toString + " contradicts constraint.")
      else if(!rui.assumptions.contains(Subtype(alpha,beta)))
        rui.substitute(alpha,beta)
    }
    case (alpha: GammaRange,beta: GammaType) => rui.substitute(alpha,alpha.refine(None,Some(SigmaLattice.meet(alpha.upperBound,beta,rui))))
    case (alpha: GammaType,beta: GammaRange) => rui.substitute(beta,beta.refine(Some(SigmaLattice.join(beta.lowerBound,alpha,rui)),None))
    case (alpha: GammaType,beta: TauVariable) => rui.substitute(beta,beta.refine(Some(alpha),None))
    case (alpha: TauVariable,beta: GammaType) => rui.substitute(alpha,alpha.refine(None,Some(beta)))
    //Cases for type constructors.
    case (alpha: ScopedPointer,beta: ScopedPointer) => {
      rui.constrain(new Equal(alpha.target,beta.target))
      if(!ScopeTypeOrdering.lt(alpha.scope,beta.scope))
        throw new TypeException("Reference type " + alpha.toString + " has smaller scope than " + beta.toString)
    }
    case (alpha: PointerType,beta: PointerType) => {
      if(alpha.target.tagged || beta.target.tagged)
        rui.constrain(new LesserEq(alpha.target,beta.target))
      else
        rui.constrain(new Equal(alpha.target,beta.target))
    }
    case (alpha: RecursiveMu,beta: RecursiveMu) => {
      val unfoldx = alpha.derecurse
      val unfoldy = beta.derecurse
      rui.assumptions.push(Subtype(unfoldx._1,unfoldy._1))
      (new LesserEq(unfoldx._2,unfoldy._2)).infer(rui)
      rui.assumptions.pop
    }
    case (rx: RhoType,ry: RecursiveMu) => rui.constrain(new LesserEq(rx,ry.unfold))
    case (alpha: SumType,beta: SumType) => {
      val shared = alpha.sumCases.zip(beta.sumCases)
      shared.foreach(pair => rui.constrain(new Equal(pair._1.record,pair._2.record)))
    }
    case (alpha: FunctionArrow,beta: FunctionArrow) => {
      beta.domain.zip(alpha.domain).map(pair => rui.constrain(new LesserEq(pair._1,pair._2)))
      rui.constrain(new LesserEq(alpha.range,beta.range))
    }
    case (alpha: PrimitiveGamma,beta: PrimitiveGamma) => if(!TauOrdering.lt(alpha,beta)) throw new TypeException("Type inference error: Incorrect <: constraint on base types.")
    case _ => throw new TypeException("Type inference error: " + alpha.toString + " </: " + beta.toString)
  }
  
  override def toString: String = alpha.toString + " <: " + beta.toString
}

class Equal(x: TauType,y: TauType) extends Constraint(x,y) {
  override def infer(rui: RangeUnificationInstance): Unit = (alpha,beta) match {
    case (alpha: GammaRange,beta: GammaRange) => {
      rui.constrain(new Equal(alpha.lowerBound,beta.lowerBound))
      rui.constrain(new Equal(alpha.upperBound,beta.upperBound))
    }
    
    case (alpha,beta: GammaRange) => throw new TypeException("Type inference error: Rho ranges cannot equal any other type.")
    case (alpha: GammaRange,beta) => throw new TypeException("Type inference error: Rho ranges cannot equal any other type.")
    
    case (alpha: TauVariable,beta: TauVariable) => rui.substitute(alpha,beta)
    case (alpha: GammaType,beta: TauVariable) => alpha match {
      case rho: RhoType => {
        if(rho.filter(tau => tau == beta) != Nil)
          rui.substitute(beta,new RecursiveMu(rho,UnrecursiveAlpha(beta)))
        else
          rui.substitute(beta,alpha)
      }
      case _ => rui.substitute(beta,alpha)
    }
    case (alpha: TauVariable,beta: GammaType) => beta match {
      case rho: RhoType => {
        if(rho.filter(tau => tau == alpha) != Nil)
          rui.substitute(alpha,new RecursiveMu(rho,UnrecursiveAlpha(alpha)))
        else
          rui.substitute(alpha,beta)
      }
      case _ => rui.substitute(alpha,beta)
    }
    case (alpha: PrimitiveGamma,beta: PrimitiveGamma) => if(alpha != beta) throw new TypeException("Type inference error: Two primitive types set equal to each other that are not equal.")
    case (alpha: RecordProduct,beta: RecordProduct) => {
      if(alpha.length == beta.length)
        alpha.fields.zip(beta.fields).map(pair => rui.constrain(new Equal(pair._1.tau,pair._2.tau)))
      else
        throw new TypeException("Type inference error: Pi types set as equal have different numbers of fields.")
    }
    case (alpha: FunctionArrow,beta: FunctionArrow) => {
      alpha.domain.zip(beta.domain).map(pair => rui.constrain(new Equal(pair._1,pair._2)))
      rui.constrain(new Equal(alpha.range,beta.range))
    }
    
    case (_,_) => throw new TypeException("Type inference error: incompatible types equated to each other.")
  }
  
  override def toString: String = alpha.toString + " = " + beta.toString
}
