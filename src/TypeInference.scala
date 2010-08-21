package decac;

import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

class Generalization(x: RhoType,y: Option[BetaType]) {
  var _1: RhoType = x
  var _2: Option[BetaType] = y
}

class TauSubstitution {
  val queue: Queue[Tuple2[TauVariable,TauType]] = new Queue[Tuple2[TauVariable,TauType]]()
  
  def substitute(x: TauVariable,y: TauType): Unit = y match {
    case y: GammaRange => if(y.lowerBound.equals(y.upperBound)) substitute(y,y.lowerBound) else queue.enqueue((x,y))
    case _ => queue.enqueue((x,y))
  }
  
 def solve(tau: TauType): TauType = tau match {
    case range: GammaRange => solve(range.lowerBound)
    case alpha: TauVariable => queue.foldLeft[TauType](alpha)((result: TauType,sub: Tuple2[TauVariable,TauType]) => if(sub._1.equals(result,false)) sub._2 else result)
    case rho: RhoType => rho.map(tau => solve(tau))
    case _ => tau
  }
}

class SigmaSubstitution {
  val queue: Queue[Tuple2[TauVariable,GammaType]] = new Queue[Tuple2[TauVariable,GammaType]]()
  
  def substitute(x: TauVariable,y: GammaType): Unit = queue.enqueue((x,y))
  
  def solve(tau: TauType): GammaType = tau match {
    case range: GammaRange => solve(range.lowerBound)
    case alpha: TauVariable => {
      val result = queue.foldLeft[Option[GammaType]](None)((x: Option[GammaType],sub: Tuple2[TauVariable,GammaType]) => x match { case Some(gamma) => Some(gamma) case None => if(sub._1.equals(alpha,false)) Some(sub._2) else None })
      result match {
        case Some(gamma) => gamma
        case None => throw new Exception("Could not specialize generalized type " + tau.mangle + ".")
      }
    }
    case rho: RhoType => rho.map(tau => solve(tau))
    case gamma: GammaType => gamma
  }
}

class LatticeNode(g: GammaType) {
  val gamma = g
  protected var parents: List[LatticeNode] = Nil
  protected var children: List[LatticeNode] = Nil
  
  def subtype(n: LatticeNode): Boolean = {
    if(parents.map(parent => parent.subtype(n)).foldLeft(false)((x: Boolean,y: Boolean) => x || y) == false) {
      if(gamma.subtypes(n.gamma,false)) {
        parents = n :: parents
        n.supertype(this)
        true
      }
      else if(gamma.subtypes(n.gamma,true)) {
        val nodeMatch = new LatticeNode(gamma.matchSupertype(n.gamma) match { case gammaMatch: GammaType => gammaMatch case _ => throw new Exception("Generating a match between two gamma-types generated a non-gamma type.") })
        subtype(n)
      }
      else
        false
    }
    else
      false
  }
  
  def subtypes(n: LatticeNode): Boolean = parents.exists(parent => parent == n) || parents.map(parent => parent.subtypes(n)).foldLeft(false)((x: Boolean,y: Boolean) => x || y)
  
  def supertype(n: LatticeNode): Boolean = {
    if(children.map(child => child.supertype(n)).foldLeft(false)((x: Boolean,y: Boolean) => x || y) == false) {
      if(n.gamma.subtypes(gamma,false)) {
        children = n :: children
        n.subtype(this)
        true
      }
      else if(n.gamma.subtypes(gamma,true)) {
        val nodeMatch = new LatticeNode(n.gamma.matchSupertype(gamma) match { case gammaMatch: GammaType => gammaMatch case _ => throw new Exception("Generating a match between two gamma-types generated a non-gamma type.") })
        supertype(n)
      }
      else
        false
    }
    else
      false
  }
  
  def supertypes(n: LatticeNode): Boolean = children.exists(child => child == n) || children.map(child => child.supertypes(n)).foldLeft(false)((x: Boolean,y: Boolean) => x || y)
  
  def find(g: GammaType): Option[LatticeNode] = {
    if(gamma == g)
      Some(this)
    else
      children.map(child => child.find(g)).map(res => res match { case Some(result) => result case None => null }).find(search => search != null)
  }
  
  def getSupertypes: List[LatticeNode] = parents ++ parents.flatMap(parent => parent.getSupertypes)
  def getSubtypes: List[LatticeNode] = children ++ children.flatMap(child => child.getSubtypes)
}

class GammaLattice {
  val top = new LatticeNode(TopGamma)
  val bottom = new LatticeNode(BottomGamma)
  
  def meet(x: GammaType,y: GammaType): GammaType = {
    val xn: LatticeNode = find(x)
    val yn: LatticeNode = find(y)
    if(xn.subtypes(yn))
      xn.gamma
    else if(yn.subtypes(xn))
      yn.gamma
    else {
      val attempts = (xn.getSubtypes ++ yn.getSubtypes).filter(attempt => attempt.subtypes(xn) && attempt.subtypes(yn))
      if(attempts != Nil)
        attempts.sort((a: LatticeNode,b: LatticeNode) => a.supertypes(b)).first.gamma
      else
        BottomGamma
    }
  }
  
  def join(x: GammaType,y: GammaType): GammaType = {
    val xn: LatticeNode = find(x)
    val yn: LatticeNode = find(y)
    if(xn.subtypes(yn))
      yn.gamma
    else if(yn.subtypes(xn))
      xn.gamma
    else {
      val attempts = (xn.getSupertypes ++ yn.getSupertypes).filter(attempt => xn.subtypes(attempt) && yn.subtypes(attempt))
      if(attempts != Nil)
        attempts.sort((a: LatticeNode,b: LatticeNode) => a.subtypes(b)).first.gamma
      else
        TopGamma
    }
  }

  def find(gamma: GammaType): LatticeNode = {
    top.find(gamma) match {
      case Some(node) => node
      case None => add(gamma)
    }
  }
  def add(gamma: GammaType): LatticeNode = {
    val node = new LatticeNode(gamma)
    top.supertype(node)
    bottom.subtype(node)
    node
  }
}

class ConstraintSet {
  val stack: Stack[Constraint] = new Stack[Constraint]()
  
  def substitute(x: TauVariable,y: TauType): Unit = stack.map(constraint => constraint.substitute(x,y))
  
  def push(c: Constraint): Unit = stack.push(c)
  
  def pop: Constraint = stack.pop
  
  def isEmpty = stack.isEmpty
}

class RangeUnificationInstance {
  protected val constraints = new ConstraintSet()
  val lattice = new GammaLattice
  protected val result = new TauSubstitution
  
  def constrain(c: Constraint) = {
    constraints.push(c)
  }
  
  def substitute(x: TauVariable,y: TauType): Unit = {
    result.substitute(x,y)
    constraints.substitute(x,y)
  }
  
  def solve: TauSubstitution = {
    while(constraints.isEmpty != true) {
      val constraint = constraints.pop
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
}

case class LesserEq(x: TauType,y: TauType) extends Constraint(x,y) {
  override def infer(rui: RangeUnificationInstance): Unit = this match {
    case LesserEq(alpha: GammaRange,beta: GammaRange) => {
      rui.substitute(alpha,new GammaRange(Some(alpha.lowerBound),Some(rui.lattice.join(alpha.upperBound,beta.lowerBound))))
      rui.substitute(beta,new GammaRange(Some(rui.lattice.meet(alpha.upperBound,beta.lowerBound)),Some(beta.upperBound)))
    }
    case LesserEq(alpha: GammaRange,beta: TauVariable) => rui.substitute(beta,alpha)
    case LesserEq(alpha: TauVariable,beta: GammaRange) => rui.substitute(alpha,beta)
    case LesserEq(alpha: TauVariable,beta: TauVariable) => rui.substitute(alpha,beta)
    
    case LesserEq(alpha: GammaRange,beta: GammaType) => rui.substitute(alpha,new GammaRange(Some(alpha.lowerBound),Some(rui.lattice.join(alpha.upperBound,beta))))
    case LesserEq(alpha: GammaType,beta: GammaRange) => rui.substitute(beta,new GammaRange(Some(rui.lattice.meet(beta.lowerBound,alpha)),Some(beta.upperBound)))
    
    case LesserEq(alpha: FunctionArrow,beta: FunctionArrow) => {
      beta.domain.zip(alpha.domain).map(pair => rui.constrain(new LesserEq(pair._1,pair._2)))
      rui.constrain(new LesserEq(alpha.range,beta.range))
    }
    case LesserEq(alpha: RecordPi,beta: RecordPi) => {
      if(alpha.length >= beta.length)
        alpha.fields.zip(beta.fields).map(pair => rui.constrain(new Equal(pair._1.tau,pair._2.tau)))
      else
        throw new Exception("Type inference error: Pi types set as subtype does not have a greater number of fields than its purported supertype.")
    }
    case LesserEq(alpha: PrimitiveGamma,beta: PrimitiveGamma) => if(!alpha.subtypes(beta,false)) throw new Exception("Type inference error: Incorrect <: constraint on base types.")
    case _ => throw new Exception("Type inference error: Invalid type inequality.")
  }
}

case class Equal(x: TauType,y: TauType) extends Constraint(x,y) {
  override def infer(rui: RangeUnificationInstance): Unit = this match {
    case Equal(alpha: GammaRange,beta: GammaRange) => {
      rui.constrain(new Equal(alpha.lowerBound,beta.lowerBound))
      rui.constrain(new Equal(alpha.upperBound,beta.upperBound))
    }
    
    case Equal(alpha,beta: GammaRange) => throw new Exception("Type inference error: Rho ranges cannot equal any other type.")
    case Equal(alpha: GammaRange,beta) => throw new Exception("Type inference error: Rho ranges cannot equal any other type.")
    
    case Equal(alpha: TauVariable,beta: TauVariable) => rui.substitute(alpha,beta)
    case Equal(alpha: RhoType,beta: TauVariable) => rui.substitute(beta,alpha)
    case Equal(alpha: TauVariable,beta: RhoType) => rui.substitute(alpha,beta)
    
    case Equal(alpha: PrimitiveGamma,beta: PrimitiveGamma) => if(alpha != beta) throw new Exception("Type inference error: Two primitive types set equal to each other that are not equal.")
    case Equal(alpha: RecordPi,beta: RecordPi) => {
      if(alpha.length == beta.length)
        alpha.fields.zip(beta.fields).map(pair => rui.constrain(new Equal(pair._1.tau,pair._2.tau)))
      else
        throw new Exception("Type inference error: Pi types set as equal have different numbers of fields.")
    }
    case Equal(alpha: FunctionArrow,beta: FunctionArrow) => {
      alpha.domain.zip(beta.domain).map(pair => rui.constrain(new Equal(pair._1,pair._2)))
      rui.constrain(new Equal(alpha.range,beta.range))
    }
    
    case Equal(_,_) => throw new Exception("Type inference error: incompatible types equated to each other.")
  }
}
