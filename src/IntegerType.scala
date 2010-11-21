package decac

import scala.math
import jllvm.LLVMType
import jllvm.LLVMIntegerType

abstract class NumericalGamma(n: String,p: Option[NumericalGamma]) extends PrimitiveGamma {
  def signed: Boolean
  val name: String = n
  val parent: Option[NumericalGamma] = p
  define(new TypeDefinition(this,name,GlobalScope))
  
  override def subtypes(tau: TauType) = tau match {
    case ntype: NumericalGamma => parent match {
      case Some(par) => par == ntype || par.subtypes(tau)
      case None => false
    }
    case range: GammaRange => subtypes(range.lowerBound)
    case bvar: BetaVariable => true
    case _ => false
  }
}

abstract class IntegerGamma(n: String,p: Option[NumericalGamma]) extends NumericalGamma(n,p) {
  def floor: Long
  def ceiling: Long
  
  var compiledType: Option[LLVMIntegerType] = None
  
  override def compile: LLVMIntegerType = compiledType match {
    case None => {
      val myType = new LLVMIntegerType(math.floor(math.log(ceiling - floor) / math.log(2)).toInt + 1)
      compiledType = Some(myType)
      return myType
    }
    case Some(myType) => myType
  }
}

object IntegerConstants {
  def raise(n: Int,pow: Int): Long = {
    if(pow == 1)
      n
    else
      n * raise(n,pow-1)
  }
  val max_longnat = raise(2,64) - 1
  val max_longInt = raise(2,63) - 1
  val min_longInt = -raise(2,63)
  val max_nat = raise(2,32) - 1
  val max_Int = raise(2,31) - 1
  val min_Int = -raise(2,31)
  val max_snat = raise(2,16) - 1
  val max_sInt = raise(2,15) - 1
  val min_sInt = -raise(2,15)
  val max_byte = raise(2,8) - 1
  val max_octet = raise(2,7) - 1
  val min_octet = -raise(2,7)
  val min_unsigned = 0
}

abstract class UnsignedIntegerGamma(n: String,p: Option[NumericalGamma]) extends IntegerGamma(n,p)

object LongInteger extends IntegerGamma("integer",Some(FP128Gamma)) {
  override def signed: Boolean = true
  override def floor: Long = -IntegerConstants.raise(2,64)
  override def ceiling: Long = IntegerConstants.max_longnat
}

object LongNat extends UnsignedIntegerGamma("longnat",Some(LongInteger)) {
  override def signed: Boolean = false
  override def floor: Long = IntegerConstants.min_unsigned
  override def ceiling: Long = IntegerConstants.max_longnat
}

object LongInt extends IntegerGamma("longint",Some(LongInteger)) {
  override def signed: Boolean = true
  override def floor: Long = IntegerConstants.min_longInt
  override def ceiling: Long = IntegerConstants.max_longInt
}

object Nat extends UnsignedIntegerGamma("nat",Some(LongNat)) {
  override def ceiling: Long = IntegerConstants.max_nat
  override def floor: Long = IntegerConstants.min_unsigned
  override def signed: Boolean = false
}

object Int extends IntegerGamma("int",Some(LongInt)) {
  override def floor: Long = IntegerConstants.min_Int
  override def ceiling: Long = IntegerConstants.max_Int
  override def signed: Boolean = true
}

object SNat extends UnsignedIntegerGamma("snat",Some(Nat)) {
  override def ceiling: Long = IntegerConstants.max_snat
  override def floor: Long = IntegerConstants.min_unsigned
  override def signed: Boolean = false
}

object SInt extends IntegerGamma("sint",Some(Int)) {
  override def floor: Long = IntegerConstants.min_sInt
  override def ceiling: Long = IntegerConstants.max_sInt
  override def signed: Boolean = true
}

object Byte extends UnsignedIntegerGamma("byte",Some(SNat)) {
  override def floor: Long = IntegerConstants.min_unsigned
  override def ceiling: Long = IntegerConstants.max_byte
  override def signed: Boolean = false
}

object Octet extends IntegerGamma("octet",Some(SInt)) {
  override def floor: Long = IntegerConstants.min_octet
  override def ceiling: Long = IntegerConstants.max_octet
  override def signed: Boolean = true
}
