package decac;

import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

class TauSubstitution {
  val queue: Queue[Tuple2[TauVariable,TauType]] = new Queue[Tuple2[TauVariable,TauType]]()
  
  def substitute(x: TauVariable,y: TauType): Unit = y match {
    case y: RhoRange => if(y.lowerBound.equals(y.upperBound)) substitute(y,y.lowerBound) else queue.enqueue((x,y))
    case _ => queue.enqueue((x,y))
  }
  
  def generalize(sigma: SigmaType): SigmaType = {
    val vars: Queue[TauVariable] = new Queue[TauVariable]()
    sigma.findUnconstrained(vars,this)
    vars.foldRight[SigmaType](sigma)((tvar: TauVariable,sigma: SigmaType) => new ForallSigma(tvar,sigma))
  }
  
  def solve(x: TauType): TauType = {
    var solution: TauType = x
    queue.foreach(elmnt => if(elmnt._1.equals(solution)) solution = elmnt._2)
    solution match {
      case rho: RhoType => rho
      case variable: TauVariable =>  variable
      case _ => throw new Exception("Inferred type is neither a parameterized type nor a simple type.")
    }
  }
}

class SigmaSubstitution(sub: TauSubstitution) {
  val original: TauSubstitution = sub
  var sigmas: List[Tuple2[SigmaType,SigmaType]] = Nil
  
  def generalize(sigma: SigmaType): SigmaType = sigmas.find(pair => pair._2 == sigma) match {
    case Some((s: SigmaType,t: SigmaType)) => t
    case None => {
      val vars: Queue[TauVariable] = new Queue[TauVariable]()
      sigma.findUnconstrained(vars,original)
      val newSigma = vars.foldRight[SigmaType](sigma)((tvar: TauVariable,sigma: SigmaType) => new ForallSigma(tvar,sigma))
      sigmas = (sigma,newSigma) :: sigmas
      return newSigma
    }
  }
  
  def solve(x: TauType): SigmaType = {
    var solution: SigmaType = original.solve(x)
    sigmas.foreach(element => if(element._1.equals(solution)) solution = element._2)
    return solution
  }
}

class ConstraintSet {
  val stack: Stack[Constraint] = new Stack[Constraint]()
  
  def substitute(x: TauVariable,y: TauType): Unit = {
    stack.map(constraint => constraint.substitute(x,y))
  }
  
  def push(c: Constraint): Unit = stack.push(c)
  
  def pop: Constraint = stack.pop
  
  def isEmpty = stack.isEmpty
}

class LatticeNode(r: RhoType) {
  val rho = r
  protected var parents: List[LatticeNode] = Nil
  protected var children: List[LatticeNode] = Nil
  
  def subtype(n: LatticeNode): Boolean = {
    if(parents.map(parent => parent.subtype(n)).foldLeft(false)((x: Boolean,y: Boolean) => x || y) == false) {
      if(rho.subtypes(n.rho,false)) {
        parents = n :: parents
        n.supertype(this)
        true
      }
      else if(rho.subtypes(n.rho,true)) {
        val nodeMatch = new LatticeNode(rho.generateMatch(n.rho) match { case rhoMatch: RhoType => rhoMatch case _ => throw new Exception("Generating a match between two rho-types generated a non-rho type.") })
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
      if(n.rho.subtypes(rho,false)) {
        children = n :: children
        n.subtype(this)
        true
      }
      else if(n.rho.subtypes(rho,true)) {
        val nodeMatch = new LatticeNode(n.rho.generateMatch(rho) match { case rhoMatch: RhoType => rhoMatch case _ => throw new Exception("Generating a match between two rho-types generated a non-rho type.") })
        supertype(n)
      }
      else
        false
    }
    else
      false
  }
  
  def supertypes(n: LatticeNode): Boolean = children.exists(child => child == n) || children.map(child => child.supertypes(n)).foldLeft(false)((x: Boolean,y: Boolean) => x || y)
  
  def find(r: RhoType): Option[LatticeNode] = {
    if(rho == r)
      Some(this)
    else
      children.map(child => child.find(r)).map(res => res match { case Some(result) => result case None => null }).find(search => search != null)
  }
  
  def getSupertypes: List[LatticeNode] = parents ++ parents.flatMap(parent => parent.getSupertypes)
  def getSubtypes: List[LatticeNode] = children ++ children.flatMap(child => child.getSubtypes)
}

class RhoLattice {
  val top = new LatticeNode(TopRho)
  val bottom = new LatticeNode(BottomRho)
  
  def meet(x: RhoType,y: RhoType): RhoType = {
    val xn: LatticeNode = find(x)
    val yn: LatticeNode = find(y)
    if(xn.subtypes(yn))
      xn.rho
    else if(yn.subtypes(xn))
      yn.rho
    else {
      val attempts = (xn.getSubtypes ++ yn.getSubtypes).filter(attempt => attempt.subtypes(xn) && attempt.subtypes(yn))
      if(attempts != Nil)
        attempts.sort((a: LatticeNode,b: LatticeNode) => a.supertypes(b)).first.rho
      else
        BottomRho
    }
  }
  
  def join(x: RhoType,y: RhoType): RhoType = {
    val xn: LatticeNode = find(x)
    val yn: LatticeNode = find(y)
    if(xn.subtypes(yn))
      yn.rho
    else if(yn.subtypes(xn))
      xn.rho
    else {
      val attempts = (xn.getSupertypes ++ yn.getSupertypes).filter(attempt => xn.subtypes(attempt) && yn.subtypes(attempt))
      if(attempts != Nil)
        attempts.sort((a: LatticeNode,b: LatticeNode) => a.subtypes(b)).first.rho
      else
        TopRho
    }
  }

  def find(rho: RhoType): LatticeNode = {
    top.find(rho) match {
      case Some(node) => node
      case None => add(rho)
    }
  }
  def add(rho: RhoType): LatticeNode = {
    val node = new LatticeNode(rho)
    top.supertype(node)
    bottom.subtype(node)
    node
  }
}

class RangeUnificationInstance {
  protected val constraints = new ConstraintSet()
  val lattice = new RhoLattice
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
  
  def substitute(from: TauVariable,to: TauType) = {
    alpha match {
      case a: TauVariable => if(a.equals(from)) alpha = to
      case a: RhoType => a.replace(from,to)
    }
    beta match {
      case b: TauVariable => if(b.equals(from)) beta = to
      case b: RhoType => b.replace(from,to)
    }
  }
  
  def infer(rui: RangeUnificationInstance): Unit
}

case class LesserEq(x: TauType,y: TauType) extends Constraint(x,y) {
  override def infer(rui: RangeUnificationInstance): Unit = this match {
    case LesserEq(alpha: RhoRange,beta: RhoRange) => {
      rui.substitute(alpha,new RhoRange(Some(alpha.lowerBound),Some(rui.lattice.join(alpha.upperBound,beta.lowerBound))))
      rui.substitute(beta,new RhoRange(Some(rui.lattice.meet(alpha.upperBound,beta.lowerBound)),Some(beta.upperBound)))
    }
    case LesserEq(alpha: RhoRange,beta: TauVariable) => rui.substitute(beta,alpha)
    case LesserEq(alpha: TauVariable,beta: RhoRange) => rui.substitute(alpha,beta)
    case LesserEq(alpha: TauVariable,beta: TauVariable) => rui.substitute(alpha,beta)
    
    case LesserEq(alpha: RhoRange,beta: RhoType) => rui.substitute(alpha,new RhoRange(Some(alpha.lowerBound),Some(rui.lattice.join(alpha.upperBound,beta))))
    case LesserEq(alpha: RhoType,beta: RhoRange) => rui.substitute(beta,new RhoRange(Some(rui.lattice.meet(beta.lowerBound,alpha)),Some(beta.upperBound)))
    
    case LesserEq(alpha: FunctionRho,beta: FunctionRho) => {
      beta.domain.zip(alpha.domain).map(pair => rui.constrain(new LesserEq(pair._1,pair._2)))
      rui.constrain(new LesserEq(alpha.range,beta.range))
    }
    case LesserEq(alpha: RecordRho,beta: RecordRho) => {
      if(alpha.length >= beta.length)
        alpha.fields.zip(beta.fields).map(pair => rui.constrain(new Equal(pair._1.tau,pair._2.tau)))
      else
        throw new Exception("Type inference error: Pi types set as subtype does not have a greater number of fields than its purported supertype.")
    }
    case LesserEq(alpha: PrimitiveRho,beta: PrimitiveRho) => if(!alpha.subtypes(beta,false)) throw new Exception("Type inference error: Incorrect <: constraint.")
    case _ => throw new Exception("Type inference error: Invalid type inequality.")
  }
}

case class Equal(x: TauType,y: TauType) extends Constraint(x,y) {
  override def infer(rui: RangeUnificationInstance): Unit = this match {
    case Equal(alpha: RhoRange,beta: RhoRange) => {
      rui.constrain(new Equal(alpha.lowerBound,beta.lowerBound))
      rui.constrain(new Equal(alpha.upperBound,beta.upperBound))
    }
    
    case Equal(alpha,beta: RhoRange) => throw new Exception("Type inference error: Rho ranges cannot equal any other type.")
    case Equal(alpha: RhoRange,beta) => throw new Exception("Type inference error: Rho ranges cannot equal any other type.")
    
    case Equal(alpha: TauVariable,beta: TauVariable) => rui.substitute(alpha,beta)
    case Equal(alpha: RhoType,beta: TauVariable) => rui.substitute(beta,alpha)
    case Equal(alpha: TauVariable,beta: RhoType) => rui.substitute(alpha,beta)
    
    case Equal(alpha: PrimitiveRho,beta: PrimitiveRho) => if(alpha != beta) throw new Exception("Type inference error: Two primitive types set equal to each other that are not equal.")
    case Equal(alpha: RecordRho,beta: RecordRho) => {
      if(alpha.length == beta.length)
        alpha.fields.zip(beta.fields).map(pair => rui.constrain(new Equal(pair._1.tau,pair._2.tau)))
      else
        throw new Exception("Type inference error: Pi types set as equal have different numbers of fields.")
    }
    case Equal(alpha: FunctionRho,beta: FunctionRho) => {
      alpha.domain.zip(beta.domain).map(pair => rui.constrain(new Equal(pair._1,pair._2)))
      rui.constrain(new Equal(alpha.range,beta.range))
    }
    
    case Equal(_,_) => throw new Exception("Type inference error: incompatible types equated to each other.")
  }
}
