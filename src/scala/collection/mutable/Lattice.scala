package scala.collection.mutable

import scala.collection.mutable
import scala.math.PartialOrdering

protected class LatticeNode[E](val value: E)(implicit po: PartialOrdering[E]) {
  protected val ordering = po
  protected val parents: mutable.HashSet[mutable.LatticeNode[E]] = new mutable.HashSet[mutable.LatticeNode[E]]()
  protected val children: mutable.HashSet[mutable.LatticeNode[E]] = new mutable.HashSet[mutable.LatticeNode[E]]()
  
  def placeAbove(n: mutable.LatticeNode[E]): Boolean = {
    if(!parents.exists(parent => parent.placeAbove(n)) && ordering.lt(value,n.value)) {
      parents.add(n)
      n.children.add(this)
      true
    }
    else
      false
  }
  
  def placeBelow(n: mutable.LatticeNode[E]): Boolean = {
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
  
  def find(elem: E): Option[mutable.LatticeNode[E]] = {
    if(ordering.equiv(elem,value))
      Some(this)
    else
      children.map(child => child.find(elem)).foldLeft[Option[mutable.LatticeNode[E]]](None)((x: Option[mutable.LatticeNode[E]],y: Option[mutable.LatticeNode[E]]) => if(x != None) x else if(y != None) y else None)
  }
  
  def greaterElements: mutable.Set[mutable.LatticeNode[E]] = parents ++ parents.flatMap(parent => parent.greaterElements)
  def lesserElements: mutable.Set[mutable.LatticeNode[E]] = children ++ children.flatMap(child => child.lesserElements)

  def flatten(s: mutable.Set[E]): Unit = {
    s.add(value)
    for(child <- children)
      if(!s.contains(child.value))
        child.flatten(s)
  }
}

trait Lattice[E] {
  val ordering: PartialOrdering[E]
  
  def join(x: E,y: E): E
  def meet(x: E,y: E): E
}

trait LatticeOrdering[E] extends mutable.Lattice[E] with PartialOrdering[E] {
  override val ordering: PartialOrdering[E] = this
}

trait LatticeSet[E] extends mutable.Lattice[E] with mutable.Set[E] {
  val top: E
  val bottom: E
}

class GraphLattice[E](override val top: E,override val bottom: E)(implicit override val ordering: PartialOrdering[E]) extends mutable.LatticeSet[E] {
  protected val topNode: mutable.LatticeNode[E] = new mutable.LatticeNode[E](top)(ordering)
  protected val bottomNode: mutable.LatticeNode[E] = new mutable.LatticeNode[E](bottom)(ordering)
  assert(ordering.lt(bottom,top))
  
  override def contains(key: E): Boolean = topNode.find(key) match {
    case Some(node) => true
    case None => false
  }
 
  override def iterator: Iterator[E] = {
    val result = new mutable.HashSet[E]()
    topNode.flatten(result)
    result.iterator
  }
  
  override def +(elem: E): mutable.GraphLattice[E] = {
    assert(ordering.lteq(elem,topNode.value))
    val result = new mutable.GraphLattice[E](topNode.value,bottomNode.value)(ordering)
    foreach(value => result.add(value))
    result.add(elem)
    result
  }
  
  override def -(elem: E): mutable.GraphLattice[E] = {
    assert(ordering.gteq(elem,bottomNode.value))
    val result = new mutable.GraphLattice[E](topNode.value,bottomNode.value)(ordering)
    foreach(value => if(value != elem) result.add(value))
    result
  }
  
  protected def find(elem: E): mutable.LatticeNode[E] = topNode.find(elem) match {
    case Some(node) => node
    case None => {
      val node = new mutable.LatticeNode(elem)(ordering)
      topNode.placeBelow(node)
      bottomNode.placeAbove(node)
      node
    }
  }
  
  override def add(elem: E): Boolean = topNode.find(elem) match {
    case Some(node) => false
    case None => {
      find(elem)
      true
    }
  }
  
  override def +=(elem: E): this.type = {
    add(elem)
    this
  }
  
  override def -=(elem: E): this.type = {
    topNode.remove(elem)
    this
  }
  
  override def join(x: E,y: E): E = {
    if(ordering.lteq(x,y))
      y
    else if(ordering.lteq(y,x))
      x
    else {
      val xn = find(x)
      val yn = find(y)
      
      val attempts = (xn.greaterElements ++ yn.greaterElements).filter(a => ordering.lt(x,a.value) && ordering.lt(y,a.value))
      attempts.toList.sortWith((a: mutable.LatticeNode[E],b: mutable.LatticeNode[E]) => ordering.lt(a.value,b.value)).head.value
    }
  }
  
  override def meet(x: E,y: E): E = {
    if(ordering.lteq(x,y))
      x
    else if(ordering.lteq(y,x))
      y
    else {
      val xn = find(x)
      val yn = find(y)
      
      val attempts = (xn.lesserElements ++ yn.lesserElements).filter(a => ordering.lt(a.value,x) && ordering.lt(a.value,y))
      attempts.toList.sortWith((a: mutable.LatticeNode[E],b: mutable.LatticeNode[E]) => ordering.gt(a.value,b.value)).head.value
    }
  }
}
