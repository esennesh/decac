package decac;

import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

abstract class Substitution[A <: TauVariable,B <: TauType] {
  protected val queue: Queue[Tuple2[A,B]] = new Queue[Tuple2[A,B]]()
  
  def substitute(x: A,y: B): Unit = queue.enqueue((x,y))
  def isEmpty: Boolean = queue.isEmpty

  def solve(tau: TauType): TauType = tau match {
    case alpha: TauVariable => queue.foldLeft[TauType](alpha)((result: TauType,sub: Tuple2[TauVariable,TauType]) => if(TauOrdering.equiv(sub._1,result)) sub._2 else result) match {
      case range: GammaRange => solve(if(range.lowerBound == BottomGamma) range.upperBound else range.lowerBound)
      case tau: TauType => tau
    }
    case rho: RhoType => rho.map(tau => solve(tau))
    case _ => tau
  }
}

class TauSubstitution extends Substitution[TauVariable,TauType] {
  override def substitute(x: TauVariable,y: TauType): Unit = {
    super.substitute(x,y)
    y match {
      case y: GammaRange => if(TauOrdering.equiv(y.lowerBound,y.upperBound)) substitute(y,y.lowerBound)
      case _ => {}
    }
  }
}

class BetaSpecialization extends Substitution[BetaVariable,GammaType] {
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
  val stack = new Stack[Constraint]()
  
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
        case te: TypeException => throw new TypeException("Unsatisfiable type constraint: " + constraint.toString + "; " + te.error)
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
    alpha = alpha match {
      case a: RhoType => a.replace(from,to)
      case a: TauVariable => if(TauOrdering.equiv(a,from)) to else a
      case _ => alpha
    }
    beta = beta match {
      case b: RhoType => b.replace(from,to)
      case b: TauVariable => if(TauOrdering.equiv(b,from)) to else b
      case _ => beta
    }
  }
  
  def infer(rui: RangeUnificationInstance): Unit
  
  override def toString: String
}

class LesserEq(x: TauType,y: TauType) extends Constraint(x,y) {
  override def infer(rui: RangeUnificationInstance): Unit = (alpha,beta) match {
    //Cases for handling ranges and variables.
    case (alpha: GammaRange,beta: GammaRange) => if(alpha != beta) (alpha.lowerBound,alpha.upperBound,beta.lowerBound,beta.upperBound) match {
      case (gx: GammaType,TopGamma,BottomGamma,gy: GammaType) => rui.constrain(new LesserEq(gx,gy))
      case (_,TopGamma,gx: GammaType,_) => rui.substitute(alpha,alpha.refine(None,Some(gx),rui))
      case (_,gx: GammaType,BottomGamma,_) => rui.substitute(beta,beta.refine(Some(gx),None,rui))
      case (_,gx: GammaType,gy: GammaType,_) => {
        rui.substitute(alpha,alpha.refine(None,Some(SigmaLattice.meet(gx,gy,rui)),rui))
        rui.substitute(beta,beta.refine(Some(SigmaLattice.join(gx,gy,rui)),None,rui))
      }
    }
    case (alpha: GammaRange,beta: TauVariable) => rui.substitute(beta,alpha)
    case (alpha: TauVariable,beta: GammaRange) => rui.substitute(alpha,beta)
    case (alpha: TauVariable,beta: TauVariable) => if(alpha != beta) {
      if(rui.assumptions.contains(Subtype(beta,alpha)))
        throw new TypeException("Assumption " + beta.toString + " <: " + alpha.toString + " contradicts constraint.")
      else
        if(!rui.assumptions.contains(Subtype(alpha,beta)))
          rui.substitute(beta,alpha)
    }
    case (alpha: GammaRange,beta: GammaType) => rui.substitute(alpha,alpha.refine(None,Some(SigmaLattice.meet(alpha.upperBound,beta,rui)),rui))
    case (alpha: GammaType,beta: GammaRange) => rui.substitute(beta,beta.refine(Some(SigmaLattice.join(beta.lowerBound,alpha,rui)),None,rui))
    case (alpha: GammaType,beta: TauVariable) => rui.substitute(beta,beta.refine(Some(alpha),None,rui))
    case (alpha: TauVariable,beta: GammaType) => rui.substitute(alpha,alpha.refine(None,Some(beta),rui))
    //Cases for type constructors.
    case (alpha: ScopedPointer,beta: ScopedPointer) => {
      rui.constrain(new Equal(alpha.target,beta.target))
      if(!ScopeTypeOrdering.lt(alpha.scope,beta.scope))
        throw new TypeException("Reference type " + alpha.toString + " has smaller scope than " + beta.toString)
    }
    case (alpha: PointerType,beta: PointerType) => {
      if(alpha.target.tagged && beta.target.tagged)
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
      val shared = alpha.sumCases.map(cx => (cx,beta.sumCases.find(cy => cx.name.name == cy.name.name).get))
      assert(shared.forall(pair => pair._1.name.name == pair._2.name.name))
      shared.foreach(pair => rui.constrain(new Equal(pair._1.record,pair._2.record)))
    }
    case (alpha: FunctionArrow,beta: FunctionArrow) => {
      beta.domain.zip(alpha.domain).map(pair => rui.constrain(new LesserEq(pair._1,pair._2)))
      rui.constrain(new LesserEq(alpha.range,beta.range))
    }
    case (alpha: TauType,TopGamma) => true
    case (BottomGamma,beta: TauType) => true
    case (alpha: PrimitiveGamma,beta: PrimitiveGamma) => if(!TauOrdering.lteq(alpha,beta)) throw new TypeException("Type inference error: Incorrect <: constraint on base types.")
    case _ => {
      try {
        (new Equal(alpha,beta)).infer(rui)
      }
      catch  {
        case te: TypeException => throw new TypeException("Type inference error: " + alpha.toString + " </: " + beta.toString)
        case e: Exception => throw e
      }
    }
  }
  
  override def toString: String = alpha.toString + " <: " + beta.toString
}

class Equal(x: TauType,y: TauType) extends Constraint(x,y) {
  override def infer(rui: RangeUnificationInstance): Unit = (alpha,beta) match {
    case (alpha: GammaRange,beta: GammaRange) => {
      rui.constrain(new Equal(alpha.lowerBound,beta.lowerBound))
      rui.constrain(new Equal(alpha.upperBound,beta.upperBound))
    }
    
    case (alpha,beta: GammaRange) => {
      rui.constrain(new LesserEq(beta.lowerBound,alpha))
      rui.constrain(new LesserEq(alpha,beta.upperBound))
      rui.substitute(beta,alpha)
    }
    case (alpha: GammaRange,beta) => {
      rui.constrain(new LesserEq(alpha.lowerBound,beta))
      rui.constrain(new LesserEq(beta,alpha.upperBound))
      rui.substitute(alpha,beta)
    }
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
    case (alpha: ArrayType,beta: ArrayType) => {
      if(alpha.length == beta.length)
        rui.constrain(new Equal(alpha.element,beta.element))
      else
        throw new TypeException("Type inference error: Array types set as equal have different lengths.")
    }
    case (alpha: RecordProduct,beta: RecordProduct) => {
      if(alpha.length == beta.length) {
        val fields = alpha.fields.zip(beta.fields)
        if(!fields.forall(pair => pair._1.name == pair._2.name))
          throw new TypeException("Product fields in supposedly-equivalent types have different names.")
        fields.foreach(pair => rui.constrain(new Equal(pair._1.tau,pair._2.tau)))
      }
      else
        throw new TypeException("Type inference error: Record types set as equal have different numbers of fields.")
    }
    case (alpha: FunctionArrow,beta: FunctionArrow) => {
      alpha.domain.zip(beta.domain).map(pair => rui.constrain(new Equal(pair._1,pair._2)))
      rui.constrain(new Equal(alpha.range,beta.range))
    }
    case (alpha: SumType,beta: SumType) => {
      assert(alpha.sumCases.length == beta.sumCases.length)
      val shared = alpha.sumCases.zip(beta.sumCases)
      assert(shared.forall(pair => pair._1.name.name == pair._2.name.name))
      shared.foreach(pair => rui.constrain(new Equal(pair._1.record,pair._2.record)))
    }
    case (SkolemCall(alpha,aparams,aopen),SkolemCall(beta,bparams,bopen)) => {
      if(alpha != beta || aopen != bopen)
        throw new TypeException("Type inference error: " + alpha.toString + " =/= " + beta.toString)
      aparams.zip(bparams).map(pair => rui.constrain(new Equal(pair._1,pair._2)))
    }
    
    case (_,_) => throw new TypeException("Type inference error: " + alpha.toString + " =/= " + beta.toString)
  }
  
  override def toString: String = alpha.toString + " = " + beta.toString
}
