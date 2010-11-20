package decac

import scala.Math
import jllvm.LLVMType
import jllvm.LLVMIntegerType

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
  
  def assignRepresentation(s: Int): Int

  def rttiSize: Int = parents match {
    case Nil => Math.ceil(Math.log(calculateRttiSize) / Math.log(2)).toInt
    case parent :: _ => parent.rttiSize
  }
  
  protected def calculateRttiSize: Int = children.map(child => child.calculateRttiSize).foldLeft(0)((x,y) => x + y)

  def represent: LLVMType
}

class GammaNode(g: GammaType) extends LatticeNode {
  override val gamma: GammaType = g
  protected var representation: Option[Int] = None

  override def assignRepresentation(s: Int): Int = {
    representation = Some(s)
    val start = gamma match {
      case enum: EnumeratedGamma => enum.represent(s)
      case _ => s
    }
    if(children != Nil)
      children.map(child => child.assignRepresentation(start + 1)).last
    else
      start + 1
  }
  
  def getRepresentation: Int = representation match {
    case Some(rep) => rep
    case None => throw new Exception("Gamma type does not yet have an rtti value.")
  }
  
  override protected def calculateRttiSize: Int = {
    1 + super.calculateRttiSize
  }
  
  override def represent: LLVMType = new LLVMIntegerType(rttiSize)
}

class BetaNode(b: BetaType) extends LatticeNode {
  val beta = b
  override val gamma: GammaType = b.body
  
  override def assignRepresentation(s: Int): Int = {
    if(children != Nil)
      children.map(child => child.assignRepresentation(s)).last
    else
      s
  }
  
  override def represent: LLVMType = throw new Exception("Cannot compile a representation value for a beta type!")
}

object SigmaLattice {
  val top: LatticeNode = new GammaNode(TopGamma)
  val bottom: LatticeNode = new GammaNode(BottomGamma)
  
  def addModule(module: Module): Unit = {
    for(pair <- module.symbols) pair._2 match { case TypeDefinition(sigma: SigmaType,_,_) => add(sigma) }
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
  
  def add(sigma: SigmaType): LatticeNode = {
    val node: LatticeNode = sigma match {
      case rho: RhoType => rho.generalize(new TauSubstitution) match {
        //We could recurse here, but rho types without free variables would cause an infinite loop.
        case gamma: GammaType => new GammaNode(gamma)
        case beta: BetaType => new BetaNode(beta)
      }
      case gamma: GammaType => new GammaNode(gamma)
      case beta: BetaType => new BetaNode(beta)
    }
    top.supertype(node)
    bottom.subtype(node)
    node
  }
}
