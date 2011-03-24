package decac;

import jllvm._

class ArrayType(e: TauType,len: Option[Int]) extends RhoType {
  val element: TauType = e
  val length = len match {
    case Some(i) => {
      assert(i > 0)
      Some(i)
    }
    case None => None
  }
  val dynamic: Boolean = length == None
  override def map(f: (TauType) => TauType): RhoType = new ArrayType(f(element),length)
  override def tagged: Boolean = false
  override def contents: List[TauType] = element :: Nil
  
  override def mangle: String = element.toString + "[]"
  override def compile: LLVMType = {
    val contents = element match {
      case gamma: GammaType => gamma.compile
      case _ => throw new TypeException("Cannot compile a dynamic array type with a non-ground element type.")
    }
    length match {
      case Some(i) => new LLVMArrayType(contents,i)
      case None => {
        val lengthTag = Nat.compile
        val array = new LLVMArrayType(contents,0)
        new LLVMPointerType(new LLVMStructType(List(lengthTag,array).toArray,true),0)
      }
    }
  }
}
