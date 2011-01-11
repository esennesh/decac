package decac

import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue
import scala.math
import jllvm.LLVMType
import jllvm.LLVMIntegerType

class EnumeratedGamma(parent: Option[EnumeratedGamma],syms: List[String]) extends PrimitiveGamma {
  val symbols: List[String] = syms
  protected var representations: Option[List[Int]] = None
  val childSymbols: Queue[String] = new Queue[String]()
  parent match {
    case Some(parent) => parent.childSymbols ++ symbols
    case None => None
  }
  
  def represent(start: Int): Int = {
    representations = Some(List.range(start,start + symbols.length))
    start + symbols.length
  }
  
  override def subtypes(tau: TauType) = tau match {
    case enum: EnumeratedGamma => enum.symbols.length >= symbols.length && symbols.map(sym => enum.symbols.exists(x => x == sym)).foldLeft(true)((x: Boolean,y: Boolean) => x && y)
    case range: GammaRange => subtypes(range.lowerBound)
    case bvar: BetaVariable => true
    case tvar: TauVariable => false
    case _ => false
  }
  
  def representationSize: Int = {
    math.ceil(math.log(symbols.length + childSymbols.length) / math.log(2)).toInt
  }
  
  override def compile: LLVMType = new LLVMIntegerType(representationSize)
}

object BooleanGamma extends EnumeratedGamma(None,List("true","false")) {
  definition = Some(new TypeDefinition(this,"boolean",GlobalScope))
}
