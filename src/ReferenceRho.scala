package decac;

import jllvm.LLVMType
import jllvm.LLVMStructType
import jllvm.LLVMPointerType

class ReferenceRho(tau: TauType,optional: Boolean,st: ScopeType) extends RhoType {
  val target: TauType = tau
  val nullable: Boolean = optional
  val scope: ScopeType = st
  
  override def subtypes(tau: TauType): Boolean = tau match {
    case ref: ReferenceRho => target.subtypes(ref.target) && ref.scope.subtypes(scope)
    case range: GammaRange => subtypes(range.lowerBound)
    case bvar: BetaVariable => true
    case tvar: TauVariable => false
    case _ => false
  }
  
  override def compile: LLVMType = target match {
    case gamma: GammaType => {
      val pointer = new LLVMPointerType(gamma.compile,0)
      if(scope == RegionalScopeType)
        new LLVMStructType((new LLVMPointerType(Nat.compile,0) :: pointer :: Nil).toArray,true)
      else
        pointer
    }
    case _ => throw new Exception("Cannot compile a reference to non-gamma type " + target.mangle)
  }
  
  override def replace(from: TauVariable,to: TauType): ReferenceRho = {
    map(tau => tau match { case tvar: TauVariable => if(tvar == from) to else target case _ => tau })
  }
  
  override def map(f: (TauType) => TauType): ReferenceRho = {
    new ReferenceRho(f(target),nullable,scope)
  }
  
  override def scopeMap(f: (ScopeType) => ScopeType): ReferenceRho = new ReferenceRho(target,nullable,f(scope))
  
  override def filter(p: (TauType) => Boolean): List[TauType] = target match {
    case rho: RhoType => rho.filter(p)
    case _ => if(p(target) == true) target :: Nil else Nil
  }
  
  override def mangle: String = target.mangle + "*(-" + scope.toString
}

object NullRho extends ReferenceRho(TopGamma,true,new GlobalScopeType(None)) {
  override def subtypes(tau: TauType): Boolean = tau match {
    case ref: ReferenceRho => ref.nullable
    case range: GammaRange => subtypes(range.lowerBound)
    case bvar: BetaVariable => true
    case tvar: TauVariable => false
    case _ => false
  }
}
