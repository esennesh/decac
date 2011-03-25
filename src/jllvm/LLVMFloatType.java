package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueType;
import jllvm.llvm.LLVMTypeKind;

public class LLVMFloatType extends LLVMRealType {
	public LLVMFloatType() {
		super(Core.LLVMFloatType());
	}
	
	public LLVMFloatType(SWIGTYPE_p_LLVMOpaqueType t) {
		super(t);
		assert(Core.LLVMGetTypeKind(t) == LLVMTypeKind.LLVMFloatTypeKind);
	}
}
