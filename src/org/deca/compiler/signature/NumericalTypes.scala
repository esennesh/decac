package org.deca.compiler.signature

import scala.collection.immutable.HashSet
import scala.math
import org.jllvm._
import org.deca.compiler.definition._

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

abstract class NumericalType(val name: String,val parent: Option[NumericalType]) extends MonoType {
  def signed: Boolean
  new TypeDefinition(new TypeExpressionConstructor(Nil,this),name,GlobalScope)
  
  def enclosedIn(n: NumericalType): Boolean = parent match {
    case Some(p) => p == n || (p enclosedIn n)
    case None => false
  }
  override def variables: Set[SignatureVariable] = Set.empty
  override def toString: String = name
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = HashSet.empty
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = HashSet.empty
}

abstract class RealType(n: String,p: Option[RealType]) extends NumericalType(n,p) {
  override val parent: Option[RealType] = p
  override def signed: Boolean = true
}

object FP128Type extends RealType("quadruple",None) {
  override val compile = new LLVMFP128Type
}

object DoubleType extends RealType("double",Some(FP128Type)) {
  override val compile = new LLVMDoubleType
}

object FloatType extends RealType("float",Some(DoubleType)) {
  override val compile = new LLVMFloatType
}

abstract class IntegerType(n: String,p: Option[NumericalType]) extends NumericalType(n,p) {
  def floor: Long
  def ceiling: Long
  override def signed = true
  override val compile: LLVMIntegerType = new LLVMIntegerType(math.floor(math.log(ceiling - floor) / math.log(2)).toInt + 1)
}

object IntegerTypeConstants {
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

abstract class UnsignedIntegerType(n: String,p: Option[NumericalType]) extends IntegerType(n,p) {
  override def signed = false
}

object LongInteger extends IntegerType("integer",Some(FP128Type)) {
  override def signed: Boolean = true
  override def floor: Long = -IntegerTypeConstants.raise(2,64)
  override def ceiling: Long = IntegerTypeConstants.max_longnat
}

object LongNat extends UnsignedIntegerType("longnat",Some(LongInteger)) {
  override def signed: Boolean = false
  override def floor: Long = IntegerTypeConstants.min_unsigned
  override def ceiling: Long = IntegerTypeConstants.max_longnat
}

object LongInt extends IntegerType("longint",Some(LongInteger)) {
  override def signed: Boolean = true
  override def floor: Long = IntegerTypeConstants.min_longInt
  override def ceiling: Long = IntegerTypeConstants.max_longInt
}

object Nat extends UnsignedIntegerType("nat",Some(LongNat)) {
  override def ceiling: Long = IntegerTypeConstants.max_nat
  override def floor: Long = IntegerTypeConstants.min_unsigned
  override def signed: Boolean = false
}

object Int extends IntegerType("int",Some(LongInt)) {
  override def floor: Long = IntegerTypeConstants.min_Int
  override def ceiling: Long = IntegerTypeConstants.max_Int
  override def signed: Boolean = true
}

object SNat extends UnsignedIntegerType("snat",Some(Nat)) {
  override def ceiling: Long = IntegerTypeConstants.max_snat
  override def floor: Long = IntegerTypeConstants.min_unsigned
  override def signed: Boolean = false
}

object SInt extends IntegerType("sint",Some(Int)) {
  override def floor: Long = IntegerTypeConstants.min_sInt
  override def ceiling: Long = IntegerTypeConstants.max_sInt
  override def signed: Boolean = true
}

object Byte extends UnsignedIntegerType("byte",Some(SNat)) {
  override def floor: Long = IntegerTypeConstants.min_unsigned
  override def ceiling: Long = IntegerTypeConstants.max_byte
  override def signed: Boolean = false
}

object Octet extends IntegerType("octet",Some(SInt)) {
  override def floor: Long = IntegerTypeConstants.min_octet
  override def ceiling: Long = IntegerTypeConstants.max_octet
  override def signed: Boolean = true
}
