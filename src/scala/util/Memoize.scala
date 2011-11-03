package scala.util

import scala.collection.mutable.Map

class Memoize1[T, R](f: T => R) extends (T => R) {
  private var vals = Map.empty[T, R]

  def apply(x: T): R = {
    if (vals.contains(x)) {
      vals(x)
    }
    else {
      val y = f(x)
      vals + ((x, y))
      y
    }
  }
}
 
object Memoize1 {
  def apply[T, R](f: T => R) = new Memoize1(f)
}
