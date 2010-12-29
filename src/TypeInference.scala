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
        case None => throw new Exception("Had no specialization for beta variable " + alpha.mangle + ".")
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

class RangeUnificationInstance(scope: Option[Module]) {
  protected val constraints = new ConstraintSet()
  protected val result = new TauSubstitution
  scope match {
    case Some(module) => SigmaLattice.addModule(module)
    case None => None
  }
  
  def constrain(c: Constraint) = {
    constraints.push(c)
    c.alpha match {
      case gamma: GammaType => SigmaLattice.find(gamma)
      case _ => {}
    }
    c.beta match {
      case gamma: GammaType => SigmaLattice.find(gamma)
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
      System.err.println(constraint.toString)
      constraint.infer(this)
    }
    return result
  }
}

abstract class Constraint(x: TauType,y: TauType) {
  var alpha = x
  var beta = y
  
  def substitute(from: TauVariable,to: TauType): Unit = {
    alpha match {
      case a: TauVariable => if(a.equals(from)) {
        alpha = to
        System.err.println(a.mangle + " |-> " + to.mangle)
      }
      case a: RhoType => a.replace(from,to)
      case _ => alpha
    }
    beta match {
      case b: TauVariable => if(b.equals(from)) {
        beta = to
        System.err.println(b.mangle + " |-> " + to.mangle)
      }
      case b: RhoType => b.replace(from,to)
      case _ => beta
    }
  }
  
  def infer(rui: RangeUnificationInstance): Unit
  
  override def toString: String
}

class LesserEq(x: TauType,y: TauType) extends Constraint(x,y) {
  override def infer(rui: RangeUnificationInstance): Unit = (alpha,beta) match {
    case (alpha: GammaRange,beta: GammaRange) => {
      rui.substitute(alpha,alpha.refine(None,Some(SigmaLattice.meet(alpha.upperBound,beta.lowerBound,rui))))
      rui.substitute(beta,beta.refine(Some(SigmaLattice.join(alpha.upperBound,beta.lowerBound,rui)),None))
    }
    case (alpha: GammaRange,beta: TauVariable) => rui.substitute(beta,alpha)
    case (alpha: TauVariable,beta: GammaRange) => rui.substitute(alpha,beta)
    case (alpha: TauVariable,beta: TauVariable) => rui.substitute(alpha,beta)
    
    case (alpha: GammaRange,beta: GammaType) => rui.substitute(alpha,alpha.refine(None,Some(SigmaLattice.meet(alpha.upperBound,beta,rui))))
    case (alpha: GammaType,beta: GammaRange) => rui.substitute(beta,beta.refine(Some(SigmaLattice.join(beta.lowerBound,alpha,rui)),None))
    case (alpha: GammaType,beta: TauVariable) => rui.substitute(beta,beta.refine(Some(alpha),None))
    case (alpha: TauVariable,beta: GammaType) => rui.substitute(alpha,alpha.refine(None,Some(beta)))
    case (alpha: FunctionArrow,beta: FunctionArrow) => {
      beta.domain.zip(alpha.domain).map(pair => rui.constrain(new LesserEq(pair._1,pair._2)))
      rui.constrain(new LesserEq(alpha.range,beta.range))
    }
    case (alpha: RecordPi,beta: RecordPi) => {
      if(alpha.length >= beta.length)
        alpha.fields.zip(beta.fields).map(pair => rui.constrain(new Equal(pair._1.tau,pair._2.tau)))
      else
        throw new Exception("Type inference error: Pi types set as subtype does not have a greater number of fields than its purported supertype.")
    }
    case (alpha: PrimitiveGamma,beta: PrimitiveGamma) => if(!alpha.subtypes(beta)) throw new Exception("Type inference error: Incorrect <: constraint on base types.")
    case _ => throw new Exception("Type inference error: Invalid type inequality.")
  }
  
  override def toString: String = alpha.mangle + " <: " + beta.mangle
}

class Equal(x: TauType,y: TauType) extends Constraint(x,y) {
  override def infer(rui: RangeUnificationInstance): Unit = (alpha,beta) match {
    case (alpha: GammaRange,beta: GammaRange) => {
      rui.constrain(new Equal(alpha.lowerBound,beta.lowerBound))
      rui.constrain(new Equal(alpha.upperBound,beta.upperBound))
    }
    
    case (alpha,beta: GammaRange) => throw new Exception("Type inference error: Rho ranges cannot equal any other type.")
    case (alpha: GammaRange,beta) => throw new Exception("Type inference error: Rho ranges cannot equal any other type.")
    
    case (alpha: TauVariable,beta: TauVariable) => rui.substitute(alpha,beta)
    case (alpha: RhoType,beta: TauVariable) => rui.substitute(beta,alpha)
    case (alpha: TauVariable,beta: RhoType) => rui.substitute(alpha,beta)
    
    case (alpha: PrimitiveGamma,beta: PrimitiveGamma) => if(alpha != beta) throw new Exception("Type inference error: Two primitive types set equal to each other that are not equal.")
    case (alpha: RecordPi,beta: RecordPi) => {
      if(alpha.length == beta.length)
        alpha.fields.zip(beta.fields).map(pair => rui.constrain(new Equal(pair._1.tau,pair._2.tau)))
      else
        throw new Exception("Type inference error: Pi types set as equal have different numbers of fields.")
    }
    case (alpha: FunctionArrow,beta: FunctionArrow) => {
      alpha.domain.zip(beta.domain).map(pair => rui.constrain(new Equal(pair._1,pair._2)))
      rui.constrain(new Equal(alpha.range,beta.range))
    }
    
    case (_,_) => throw new Exception("Type inference error: incompatible types equated to each other.")
  }
  
  override def toString: String = alpha.mangle + " = " + beta.mangle
}
