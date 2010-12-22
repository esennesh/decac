package decac

import scala.math
import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import jllvm.LLVMType
import jllvm.LLVMIntegerType

abstract class LatticeNode {
  val gamma: GammaType
  protected val parents: Set[LatticeNode] = new HashSet[LatticeNode]()
  protected val children: Set[LatticeNode] = new HashSet[LatticeNode]()
  
  def subtype(n: LatticeNode): Boolean = {
    if(!parents.exists(parent => parent.subtype(n)) && gamma.subtypes(n.gamma)) {
      parents.add(n)
      n.children.add(this)
      true
    }
    else
      false
  }
  
  def subtypes(n: LatticeNode): Boolean = parents.exists(parent => parent == n) || parents.map(parent => parent.subtypes(n)).foldLeft(false)((x: Boolean,y: Boolean) => x || y)
  
  def supertype(n: LatticeNode): Boolean = {
    if(!children.exists(child => child.supertype(n)) && n.gamma.subtypes(gamma)) {
      children.add(n)
      n.parents.add(this)
      true
    }
    else
      false
  }
  
  def supertypes(n: LatticeNode): Boolean = children.exists(child => child == n) || children.map(child => child.supertypes(n)).foldLeft(false)((x: Boolean,y: Boolean) => x || y)
  
  def find(g: GammaType): Option[LatticeNode] = {
    if(gamma.equals(g))
      Some(this)
    else
      children.map(child => child.find(g)).foldLeft[Option[LatticeNode]](None)((x: Option[LatticeNode],y: Option[LatticeNode]) => if(x != None) x else if(y != None) y else None)
  }
  
  def getSupertypes: List[LatticeNode] = (parents ++ parents.flatMap(parent => parent.getSupertypes)).toList
  def getSubtypes: List[LatticeNode] = (children ++ children.flatMap(child => child.getSubtypes)).toList
  
  def assignRepresentation(s: Int): Int

  def rttiSize: Int = {
    if(parents == parents.empty)
      math.ceil(math.log(calculateRttiSize) / math.log(2)).toInt
    else
      parents.head.rttiSize
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
    for(pair <- module.symbols) pair._2 match { case TypeDefinition(sigma: SigmaType,_,_) => add(sigma) case _ => {} }
  }
  
  //Meet is the greatest lower bound.
  def meet(x: GammaType,y: GammaType,rui: RangeUnificationInstance): GammaType = {
    val xn: LatticeNode = find(x)
    val yn: LatticeNode = find(y)
    if(x.subtypes(y) || x.equals(y))
      x
    else if(y.subtypes(x))
      y
    else {
      val attempts = (xn.getSubtypes ++ yn.getSubtypes).filter(attempt => attempt.subtypes(xn) && attempt.subtypes(yn))
      attempts.sortWith((a: LatticeNode,b: LatticeNode) => a.supertypes(b)).head match {
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
      attempts.sortWith((a: LatticeNode,b: LatticeNode) => a.subtypes(b)).head match {
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
  
  protected def add(sigma: SigmaType): LatticeNode = {
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
