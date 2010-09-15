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
    case range: GammaRange => solve(range.lowerBound)
    case alpha: TauVariable => queue.foldLeft[TauType](alpha)((result: TauType,sub: Tuple2[TauVariable,TauType]) => if(sub._1.equals(result)) sub._2 else result)
    case rho: RhoType => rho.map(tau => solve(tau))
    case _ => tau
  }
}

class TauSubstitution extends Substitution[TauVariable,TauType] {
  def substitute(x: TauVariable,y: TauType): Unit = {
    queue.enqueue((x,y))
    y match {
      case y: GammaRange => if(y.lowerBound.equals(y.upperBound)) substitute(y,y.lowerBound)
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

abstract class LatticeNode {
  val gamma: GammaType
  protected var parents: List[LatticeNode] = Nil
  protected var children: List[LatticeNode] = Nil
  
  def subtype(n: LatticeNode): Boolean = {
    if(parents.map(parent => parent.subtype(n)).foldLeft(false)((x: Boolean,y: Boolean) => x || y) == false && gamma.subtypes(n.gamma)) {
      parents = n :: parents
      n.supertype(this)
      true
    }
    else
      false
  }
  
  def subtypes(n: LatticeNode): Boolean = parents.exists(parent => parent == n) || parents.map(parent => parent.subtypes(n)).foldLeft(false)((x: Boolean,y: Boolean) => x || y)
  
  def supertype(n: LatticeNode): Boolean = {
    if(children.map(child => child.supertype(n)).foldLeft(false)((x: Boolean,y: Boolean) => x || y) == false && n.gamma.subtypes(gamma)) {
      children = n :: children
      n.subtype(this)
      true
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

class GammaNode(g: GammaType) extends LatticeNode {
  override val gamma: GammaType = g
}

class BetaNode(b: BetaType) extends LatticeNode {
  val beta = b
  override val gamma: GammaType = b.body
}

class GammaLattice(scope: Option[Module]) {
  val top: LatticeNode = add(TopGamma)
  val bottom: LatticeNode = add(BottomGamma)
  scope match {
    case Some(scope) => scope.symbols.foreach(pair => pair._2 match {
      case TypeDefinition(gamma: GammaType,_,_) => add(gamma)
    })
    case None => {}
  }
  
  //Meet is the greatest lower bound.
  def meet(x: GammaType,y: GammaType,rui: RangeUnificationInstance): GammaType = {
    val xn: LatticeNode = find(x)
    val yn: LatticeNode = find(y)
    if(xn.subtypes(yn))
      x
    else if(yn.subtypes(xn))
      y
    else {
      val attempts = (xn.getSubtypes ++ yn.getSubtypes).filter(attempt => attempt.subtypes(xn) && attempt.subtypes(yn))
      attempts.sort((a: LatticeNode,b: LatticeNode) => a.supertypes(b)).first match {
        case bn: BetaNode => {
          val result = bn.beta.freshlyInstantiate
          rui.constrain(new LesserEq(result,x))
          rui.constrain(new LesserEq(result,y))
          result
        }
        case gn: GammaNode => gn.gamma
      }
    }
  }
  
  //Join is the least upper bound.
  def join(x: GammaType,y: GammaType,rui: RangeUnificationInstance): GammaType = {
    val xn: LatticeNode = find(x)
    val yn: LatticeNode = find(y)
    if(xn.subtypes(yn))
      y
    else if(yn.subtypes(xn))
      x
    else {
      val attempts = (xn.getSupertypes ++ yn.getSupertypes).filter(attempt => xn.subtypes(attempt) && yn.subtypes(attempt))
      attempts.sort((a: LatticeNode,b: LatticeNode) => a.subtypes(b)).first match {
        case bn: BetaNode => {
          val result = bn.beta.freshlyInstantiate
          rui.constrain(new LesserEq(x,result))
          rui.constrain(new LesserEq(y,result))
          result
        }
        case gn: GammaNode => gn.gamma
      }
    }
  }

  def find(gamma: GammaType): LatticeNode = {
    top.find(gamma) match {
      case Some(node) => node
      case None => add(gamma)
    }
  }
  
  def add(gamma: GammaType): LatticeNode = {
    val node: LatticeNode = gamma match {
      case rho: RhoType => rho.generalize(new TauSubstitution) match {
        case gamma: GammaType => new GammaNode(gamma)
        case beta: BetaType => new BetaNode(beta)
      }
      case _ => new GammaNode(gamma)
    }
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

class RangeUnificationInstance(scope: Option[Module]) {
  protected val constraints = new ConstraintSet()
  val lattice = new GammaLattice(scope)
  protected val result = new TauSubstitution
  
  def constrain(c: Constraint) = {
    constraints.push(c)
    c.alpha match {
      case gamma: GammaType => lattice.add(gamma)
    }
    c.beta match {
      case gamma: GammaType => lattice.add(gamma)
    }
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
      rui.substitute(alpha,new GammaRange(Some(alpha.lowerBound),Some(rui.lattice.meet(alpha.upperBound,beta.lowerBound,rui))))
      rui.substitute(beta,new GammaRange(Some(rui.lattice.join(alpha.upperBound,beta.lowerBound,rui)),Some(beta.upperBound)))
    }
    case LesserEq(alpha: GammaRange,beta: TauVariable) => rui.substitute(beta,alpha)
    case LesserEq(alpha: TauVariable,beta: GammaRange) => rui.substitute(alpha,beta)
    case LesserEq(alpha: TauVariable,beta: TauVariable) => rui.substitute(alpha,beta)
    
    case LesserEq(alpha: GammaRange,beta: GammaType) => rui.substitute(alpha,new GammaRange(Some(alpha.lowerBound),Some(rui.lattice.meet(alpha.upperBound,beta,rui))))
    case LesserEq(alpha: GammaType,beta: GammaRange) => rui.substitute(beta,new GammaRange(Some(rui.lattice.join(beta.lowerBound,alpha,rui)),Some(beta.upperBound)))
    
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
    case LesserEq(alpha: PrimitiveGamma,beta: PrimitiveGamma) => if(!alpha.subtypes(beta)) throw new Exception("Type inference error: Incorrect <: constraint on base types.")
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
