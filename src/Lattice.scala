package scala.collection.mutable

import scala.collection.mutable.Set.Coll
import scala.collection.mutable.HashSet.Coll
import scala.math.PartialOrdering

protected class LatticeNode[E](v: E,po: PartialOrdering[E]) {
  val value: E = v
  protected val ordering = po
  protected val parents: HashSet[LatticeNode[E]] = new HashSet[LatticeNode[E]]()
  protected val children: HashSet[LatticeNode[E]] = new HashSet[LatticeNode[E]]()
  
  def placeAbove(n: LatticeNode[E]): Boolean = {
    if(!parents.exists(parent => parent.placeAbove(n)) && ordering.lt(value,n.value)) {
      parents.add(n)
      n.children.add(this)
      true
    }
    else
      false
  }
  
  def placeBelow(n: LatticeNode[E]): Boolean = {
    if(!children.exists(child => child.placeBelow(n)) && ordering.gt(value,n.value)) {
      children.add(n)
      n.parents.add(this)
      true
    }
    else
      false
  }
  
  def remove(elem: E): Unit = {
    if(value == elem) {
      for(child <- children)
        for(parent <- parents)
          parent.children.add(child)
      for(parent <- parents)
        parent.children.remove(this)
      for(child <- children)
        child.parents.remove(this)
    }
    else
      for(child <- children)
        child.remove(elem)
  }
  
  def locate(elem: E): Option[LatticeNode[E]] = {
    if(ordering.equiv(elem,value))
      Some(this)
    else
      children.map(child => child.locate(elem)).find(possibility => possibility != None) match {
        case Some(Some(e)) => Some(e)
        case _ => None
      }
  }
  
  def greaterElements: Set[LatticeNode[E]] = parents ++ parents.flatMap(parent => parent.greaterElements)
  def lesserElements: Set[LatticeNode[E]] = children ++ children.flatMap(child => child.lesserElements)

  def flatten(s: Set[E]): Unit = {
    s.add(value)
    for(child <- children)
      if(!s.contains(child.value))
        child.flatten(s)
  }
}

trait Lattice[E] extends Set[E] {
  val top: E
  val bottom: E
  val ordering: PartialOrdering[E]
  
  def join(x: E,y: E): E
  def meet(x: E,y: E): E
}

class GraphLattice[E](t: E,b: E,po: PartialOrdering[E]) extends Lattice[E] {
  protected val topNode: LatticeNode[E] = new LatticeNode[E](t,po)
  protected val bottomNode: LatticeNode[E] = new LatticeNode[E](b,po)
  override val top: E = topNode.value
  override val bottom: E = bottomNode.value
  override val ordering: PartialOrdering[E] = po
  assert(ordering.lt(bottom,top))
  bottomNode.placeBelow(topNode)
  topNode.placeBelow(bottomNode)
  
  override def contains(key: E): Boolean = topNode.locate(key) match {
    case Some(node) => true
    case None => false
  }
 
  override def iterator: Iterator[E] = {
    val result = new HashSet[E]()
    topNode.flatten(result)
    result.iterator
  }
  
  override def +(elem: E): GraphLattice[E] = {
    assert(po.lteq(elem,topNode.value))
    val result = new GraphLattice[E](topNode.value,bottomNode.value,ordering)
    foreach(value => result.add(value))
    result.add(elem)
    result
  }
  
  override def -(elem: E): GraphLattice[E] = {
    assert(po.gteq(elem,bottomNode.value))
    val result = new GraphLattice[E](topNode.value,bottomNode.value,ordering)
    foreach(value => if(value != elem) result.add(value))
    result
  }
  
  protected def locate(elem: E): LatticeNode[E] = topNode.locate(elem) match {
    case Some(node) => node
    case None => {
      val node = new LatticeNode(elem,ordering)
      topNode.placeBelow(node)
      bottomNode.placeAbove(node)
      node
    }
  }
  
  override def add(elem: E): Boolean = topNode.locate(elem) match {
    case Some(node) => false
    case None => {
      locate(elem)
      true
    }
  }
  
  override def +=(elem: E): GraphLattice.this.type = {
    add(elem)
    this
  }
  
  override def -=(elem: E): GraphLattice.this.type = {
    topNode.remove(elem)
    this
  }
  
  override def join(x: E,y: E): E = {
    if(ordering.lteq(x,y))
      y
    else if(ordering.lteq(y,x))
      x
    else {
      val xn = locate(x)
      val yn = locate(y)
      
      val attempts = (xn.greaterElements ++ yn.greaterElements).filter(a => ordering.lt(x,a.value) && ordering.lt(y,a.value))
      attempts.toList.sortWith((a: LatticeNode[E],b: LatticeNode[E]) => ordering.lt(a.value,b.value)).head.value
    }
  }
  
  override def meet(x: E,y: E): E = {
    if(ordering.lteq(x,y))
      x
    else if(ordering.lteq(y,x))
      y
    else {
      val xn = locate(x)
      val yn = locate(y)
      
      val attempts = (xn.lesserElements ++ yn.lesserElements).filter(a => ordering.lt(a.value,x) && ordering.lt(a.value,y))
      attempts.toList.sortWith((a: LatticeNode[E],b: LatticeNode[E]) => ordering.gt(a.value,b.value)).head.value
    }
  }
}
