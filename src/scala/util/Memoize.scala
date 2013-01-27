package scala.util

import scala.collection.mutable.Map

class Memoize1[T, R](f: T => R) extends (T => R) {
  private var vals = Map.empty[T, R]

  def values = vals.values
  def apply(x: T): R = 
    if (vals contains x) 
      vals(x)
    else {
      val res = f(x)
      vals + ((x, res))
      res
    }
}
 
object Memoize1 {
  def apply[T, R](f: T => R) = new Memoize1(f)
}

class Memoize2[T, U, R](f: (T,U) => R) extends ((T,U) => R) {
  private var vals = Map.empty[(T,U), R]
  
  def values = vals.values
  def apply(x: T,y: U): R =
    if(vals contains (x,y))
      vals((x,y))
    else {
      val res = f(x,y)
      vals + (((x, y), res))
      res
    }
}

object Memoize2 {
  def apply[T, U, R](f: (T,U) => R) = new Memoize2(f)
}
