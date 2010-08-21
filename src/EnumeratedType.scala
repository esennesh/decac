package decac

import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import java.lang.Math
import jllvm.LLVMType
import jllvm.LLVMIntegerType

class EnumeratedGamma(p: Option[EnumeratedGamma],syms: List[String]) extends PrimitiveGamma {
  val parent: Option[EnumeratedGamma] = p
  val symbols: List[String] = syms
  var values: List[Int] = Nil
  var childEnums: Set[EnumeratedGamma] = new HashSet[EnumeratedGamma]()
  
  protected def assign_symbol_values(s: Int): Int = {
    values = Nil
    var start = s
    var i = 0
    for(symbol <- symbols) {
      values = values ::: List(i)
      i += 1
    }
    start += values.length
    for(child <- childEnums)
      start = child.assign_symbol_values(start)
    return start
  }
  
  def assign_values(): Unit = parent match {
    case None => assign_symbol_values(0)
    case Some(parent) => parent.assign_values
  }
  
  protected def calculateRepresentationSize: Int = {
    values.length + childEnums.map(child => child.calculateRepresentationSize).foldLeft(0)((x,y) => x + y)
  }
  
  protected def representationSize: Int = parent match {
    case None => calculateRepresentationSize
    case Some(par) => par.representationSize
  }
  
  override def subtypes(tau: TauType,possibly: Boolean) = tau match {
    case enum: EnumeratedGamma => parent match {
      case Some(par) => enum == par || par.subtypes(enum,possibly)
      case None => false
    }
    case range: GammaRange => subtypes(range.lowerBound,possibly)
    case tvar: TauVariable => possibly
    case _ => false
  }
  
  override def compile: LLVMType = new LLVMIntegerType(representationSize)
}

object BooleanGamma extends EnumeratedGamma(None,List("true","false"))
