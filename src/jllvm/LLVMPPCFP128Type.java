package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueType;
import jllvm.llvm.LLVMTypeKind;

public class LLVMPPCFP128Type extends LLVMRealType {
	public LLVMPPCFP128Type() {
		super(Core.LLVMPPCFP128Type());
	}
	
	public LLVMPPCFP128Type(SWIGTYPE_p_LLVMOpaqueType t) {
		super(t);
		assert(Core.LLVMGetTypeKind(t) == LLVMTypeKind.LLVMPPC_FP128TypeKind);
	}
}
