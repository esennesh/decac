package decac;

import jllvm.LLVMType
import jllvm.LLVMStructType
import jllvm.LLVMPointerType

class PointerType(tau: TauType) extends RhoType {
  val target: TauType = tau
  
  override def tagged: Boolean = false
  override def contents: List[TauType] = target :: Nil
  
  override def compile: LLVMType = target match {
    case gamma: GammaType => new LLVMPointerType(gamma.compile,0)
    case _ => throw new Exception("Cannot compile a pointer to non-gamma type " + target.toString)
  }
  
  override def map(f: (TauType) => TauType): PointerType = {
    new PointerType(f(target))
  }
  
  override def filter(p: (TauType) => Boolean): List[TauType] = target match {
    case rho: RhoType => rho.filter(p)
    case _ => if(p(target) == true) target :: Nil else Nil
  }
  
  override def mangle: String = target.toString + "*"
}

class ScopedPointer(tau: TauType,st: ScopeType) extends PointerType(tau) {
  val scope: ScopeType = st
  
  override def map(f: (TauType) => TauType): ScopedPointer = {
    new ScopedPointer(f(target),scope)
  }
  
  override def filter(p: (TauType) => Boolean): List[TauType] = target match {
    case rho: RhoType => rho.filter(p)
    case _ => if(p(target) == true) target :: Nil else Nil
  }
  
  override def mangle: String = scope.toString + "|-" + target.toString + "*"
}
