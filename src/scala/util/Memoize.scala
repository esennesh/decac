package scala.util

import scala.collection.mutable.Map

class Memoize1[T, R](f: T => R) extends (T => R) {
  protected var vals = Map.empty[T, R]

  def values = vals.values
  def apply(x: T): R = 
    if (vals contains x) 
      vals(x)
    else {
      val res = f(x)
      vals += ((x, res))
      res
    }
}
 
object Memoize1 {
  def apply[T, R](f: T => R) = new Memoize1(f)
}

class InitializedMemoize1[T, R](f: T => R,init: (T,R) => Unit) extends Memoize1[T,R](f) {
  override def apply(x: T): R = 
    if (vals contains x) 
      vals(x)
    else {
      val res = f(x)
      vals += ((x, res))
      init(x,res)
      res
    }
}

object InitializedMemoize1 {
  def apply[T, R](f: T => R,init: (T,R) => Unit) = new InitializedMemoize1(f,init)
}

class Memoize2[T, U, R](f: (T,U) => R) extends ((T,U) => R) {
  private var vals = Map.empty[(T,U), R]
  
  def values = vals.values
  def apply(x: T,y: U): R =
    if(vals contains (x,y))
      vals((x,y))
    else {
      val res = f(x,y)
      vals += (((x, y), res))
      res
    }
}

object Memoize2 {
  def apply[T, U, R](f: (T,U) => R) = new Memoize2(f)
}

class Memoize3[S, T, U, R](f: (S, T, U) => R) extends ((S, T, U) => R) {
  private var vals = Map.empty[(S, T, U), R]
  
  def values = vals.values
  def apply(x: S, y: T, z: U): R =
    if(vals contains (x,y,z))
      vals((x, y, z))
    else {
      val res = f(x, y, z)
      vals += (((x, y, z), res))
      res
    }
}

object Memoize3 {
  def apply[S, T, U, R](f: (S, T, U) => R) = new Memoize3(f)
}

class Memoize4[A, B, C, D, R](f: (A, B, C, D) => R) extends ((A, B, C, D) => R) {
  private var vals = Map.empty[(A, B, C, D), R]
  
  def values = vals.values
  def apply(x: A, y: B, z: C, w: D): R =
    if(vals contains (x,y,z,w))
      vals((x, y, z, w))
    else {
      val res = f(x, y, z, w)
      vals += (((x, y, z, w), res))
      res
    }
}

object Memoize4 {
  def apply[A, B, C, D, R](f: (A, B, C, D) => R) = new Memoize4(f)
}

class Memoize5[A, B, C, D, E, R](f: (A, B, C, D, E) => R) extends ((A, B, C, D, E) => R) {
  private var vals = Map.empty[(A, B, C, D, E), R]
  
  def values = vals.values
  def apply(x: A, y: B, z: C, w: D, u: E): R =
    if(vals contains (x,y,z,w,u))
      vals((x, y, z, w, u))
    else {
      val res = f(x, y, z, w, u)
      vals += (((x, y, z, w, u), res))
      res
    }
}

object Memoize5 {
  def apply[A, B, C, D, E, R](f: (A, B, C, D, E) => R) = new Memoize5(f)
}
