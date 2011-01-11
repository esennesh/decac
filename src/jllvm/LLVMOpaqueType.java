package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueType;
import jllvm.llvm.LLVMTypeKind;

public class LLVMOpaqueType extends LLVMType {
	public LLVMOpaqueType() {
		super(Core.LLVMOpaqueType());
	}
	
	public LLVMOpaqueType(SWIGTYPE_p_LLVMOpaqueType t) {
		super(t);
		assert(Core.LLVMGetTypeKind(t) == LLVMTypeKind.LLVMOpaqueTypeKind);
	}
}
