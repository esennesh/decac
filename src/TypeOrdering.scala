package decac;

import scala.collection.mutable.Lattice
import scala.collection.mutable.GraphLattice
import scala.collection.mutable.Stack

object TaggedProductEquivalence {
  def equiv(gx: TaggedProduct,gy: TaggedProduct): Boolean = {
    gx.constructor == gy.constructor && TauOrdering.equiv(gx.record,gy.record)
  }
}

object TauOrdering extends PartialOrdering[TauType] {
  protected val assumptions: Stack[Tuple2[TauType,TauType]] = new Stack[Tuple2[TauType,TauType]]()
  override def lt(x: TauType,y: TauType): Boolean = (x,y) match {
    case (px: ScopedPointer,py: ScopedPointer) => equiv(px.target,py.target) && ScopeTypeOrdering.lt(py.scope,px.scope)
    case (px: PointerType,py: PointerType) => {
      if(px.target.tagged && py.target.tagged)
        lt(px.target,py.target)
      else
        equiv(px.target,py.target)
    }
    case (rx: RecursiveMu,ry: RecursiveMu) => {
      val unfoldx = rx.derecurse
      val unfoldy = ry.derecurse
      assumptions.push((unfoldx._1,unfoldy._1))
      val result = lt(unfoldx._2,unfoldy._2)
      assumptions.pop
      result
    }
    case (rx: RhoType,ry: RecursiveMu) => lt(rx,ry.unfold)
    case (sx: SumType,sy: SumType) => sx.sumCases.forall(gpx => sy.sumCases.exists(gpy => TaggedProductEquivalence.equiv(gpx,gpy)))
    case (ry: GammaRange,y: TauType) => lt(if(ry.upperBound != TopGamma) ry.upperBound else ry.lowerBound,y)
    case (x: TauType,ry: GammaRange) => lt(x,if(ry.lowerBound != BottomGamma) ry.lowerBound else ry.upperBound)
    case (fx: FunctionArrow,fy: FunctionArrow) => fx.domain.zip(fy.domain).forall(pair => gt(pair._1,pair._2)) && lt(fx.range,fy.range)
    case (nx: NumericalGamma,ny: NumericalGamma) => nx.parent match {
      case Some(parent) => lteq(parent,ny)
      case None => false
    }
    case (x: TauType,bvar: BetaVariable) => true
    case (gx: GammaType,TopGamma) => gx != TopGamma
    case (BottomGamma,gy: GammaType) => gy != BottomGamma
    case _ => assumptions.contains((x,y))
  }
  override def gt(x: TauType,y: TauType): Boolean = lt(y,x)
  override def equiv(x: TauType,y: TauType): Boolean = (x,y) match {
    case (sx: SumType,sy: SumType) => lt(sx,sy) && lt(sy,sx)
    case (fx: FunctionArrow,fy: FunctionArrow) => fx.domain.zip(fy.domain).forall(pair => equiv(pair._1,pair._2)) && equiv(fx.range,fy.range)
    case (recx: RecordProduct,recy: RecordProduct) => (recx.length == recy.length) && recx.fields.zip(recy.fields).forall(pair => equiv(pair._1.tau,pair._2.tau))
    case (recx: RecursiveMu,recy: RecursiveMu) => {
      val alpha = new TauVariable
      val unrecx = recx.map(tau => if(tau == recx) alpha else tau)
      val unrecy = recy.map(tau => if(tau == recy) alpha else tau)
      equiv(unrecx,unrecy)
    }
    case (recx: RecursiveMu,rhoy: RhoType) => equiv(recx.unfold,rhoy)
    case (rhox: RhoType,recy: RecursiveMu) => equiv(rhox,recy.unfold)
    case (gx: GammaType,ry: GammaRange) => gt(gx,ry.lowerBound) && lt(gx,ry.upperBound)
    case (rx: GammaRange,gy: GammaType) => gt(gy,rx.lowerBound) && lt(gy,rx.upperBound)
    case (x: TauType,py: BetaVariable) => true
    case (px: BetaVariable,y: TauType) => true
    case (x: TauType,y: TauType) => x == y
  }
  override def gteq(x: TauType,y: TauType): Boolean = gt(x,y) || equiv(x,y)
  override def lteq(x: TauType,y: TauType): Boolean = lt(x,y) || equiv(x,y) 
  override def tryCompare(x: TauType,y: TauType): Option[Int] = {
    if(gt(x,y))
      Some(1)
    else if(lt(x,y))
      Some(-1)
    else if(equiv(x,y))
      Some(0)
    else
      None
  }
}

object SigmaOrdering extends PartialOrdering[SigmaType] {
  override def lt(x: SigmaType,y: SigmaType): Boolean = (x,y) match {
    case (gx: GammaType,gy: GammaType) => TauOrdering.lt(gx,gy)
    case (gx: GammaType,by: BetaType) => TauOrdering.lt(gx,by.body)
    case (bx: BetaType,by: BetaType) => TauOrdering.lt(bx.body,by.body)
    case _ => false
  }
  override def gt(x: SigmaType,y: SigmaType): Boolean = lt(y,x)
  override def equiv(x: SigmaType,y: SigmaType): Boolean = (x,y) match {
    case (gx: GammaType,gy: GammaType) => TauOrdering.equiv(gx,gy)
    case (bx: BetaType,by: BetaType) => TauOrdering.equiv(bx.body,by.body)
  }
  override def gteq(x: SigmaType,y: SigmaType): Boolean = gt(x,y) || equiv(x,y)
  override def lteq(x: SigmaType,y: SigmaType): Boolean = lt(x,y) || equiv(x,y) 
  override def tryCompare(x: SigmaType,y: SigmaType): Option[Int] = {
    if(gt(x,y))
      Some(1)
    else if(lt(x,y))
      Some(-1)
    else if(equiv(x,y))
      Some(0)
    else
      None
  }
}

object ScopeTypeOrdering extends PartialOrdering[ScopeType] {
  override def lt(x: ScopeType,y: ScopeType): Boolean = (x,y) match {
    case (gx: GlobalScopeType,gy: GlobalScopeType) => (gx.scope,gy.scope) match {
      case (Some(modx),Some(mody)) => modx.enclosed(mody)
      case _ => false
    }
    case (ax: ArgumentScopeType,ly: LexicalScopeType) => ax.callerScope match {
      case Some(callerScopeType) => lt(callerScopeType,ly)
      case None => false
    }
    case (lx: LexicalScopeType,gy: GlobalScopeType) => true
    case (lx: LexicalScopeType,ly: LexicalScopeType) => lx.scope.enclosed(ly.scope)
    case _ => false
  }
  override def gt(x: ScopeType,y: ScopeType): Boolean = lt(y,x)
  override def equiv(x: ScopeType,y: ScopeType): Boolean = (x,y) match {
    case (gx: GlobalScopeType,gy: GlobalScopeType) => (gx.scope,gy.scope) match {
      case (Some(modx),Some(mody)) => modx == mody
      case _ => true
    }
    case (lx: LexicalScopeType,ly: LexicalScopeType) => lx.scope == ly.scope
    case _ => false
  }
  override def gteq(x: ScopeType,y: ScopeType): Boolean = gt(x,y) || equiv(x,y)
  override def lteq(x: ScopeType,y: ScopeType): Boolean = lt(x,y) || equiv(x,y) 
  override def tryCompare(x: ScopeType,y: ScopeType): Option[Int] = {
    if(gt(x,y))
      Some(1)
    else if(lt(x,y))
      Some(-1)
    else if(equiv(x,y))
      Some(0)
    else
      None
  }
}

object SigmaLattice {
  protected val lattice = new GraphLattice[SigmaType](TopGamma,BottomGamma,SigmaOrdering)
  def addModule(module: Module): Unit = {
    for(pair <- module.symbols) pair._2 match { case defin: TypeDefinition => lattice.add(defin.sigma) case _ => {} }
  }
  
  def add(sigma: SigmaType): Boolean = sigma match {
    case beta: BetaType => lattice.add(beta)
    case gamma: GammaType => gamma match {
      case rho: RhoType => lattice.add(rho.generalize(new TauSubstitution))
      case _ => lattice.add(gamma)
    }
  }
  
  def join(x: GammaType,y: GammaType,rui: RangeUnificationInstance): GammaType = lattice.join(x,y) match {
    case beta: BetaType => {
      val result = beta.freshlyInstantiate
      rui.constrain(new LesserEq(x,result))
      rui.constrain(new LesserEq(y,result))
      result
    }
    case gamma: GammaType => gamma
  }
  
  def meet(x: GammaType,y: GammaType,rui: RangeUnificationInstance): GammaType = lattice.meet(x,y) match {
    case beta: BetaType => {
      val result = beta.freshlyInstantiate
      rui.constrain(new LesserEq(result,x))
      rui.constrain(new LesserEq(result,y))
      result
    }
    case gamma: GammaType => gamma
  }
}
