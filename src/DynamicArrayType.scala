package decac;

import jllvm._

class DynamicArrayType(e: TauType) extends RhoType {
  val element: TauType = e
  override def map(f: (TauType) => TauType): RhoType = new DynamicArrayType(f(element))
  override def tagged: Boolean = false
  override def contents: List[TauType] = element :: Nil
  
  override def mangle: String = element.toString + "[]"
  override def compile: LLVMPointerType = {
    val length = Nat.compile
    val array = element match {
      case gamma: GammaType => new LLVMArrayType(gamma.compile,0)
      case _ => throw new TypeException("Cannot compile a dynamic array type with a non-ground element type.")
    }
    new LLVMPointerType(new LLVMStructType(List(length,array).toArray,true),0)
  }
}
