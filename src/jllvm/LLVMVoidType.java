package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueType;
import jllvm.llvm.LLVMTypeKind;

public class LLVMVoidType extends LLVMType {
	public LLVMVoidType() {
		super(Core.LLVMVoidType());
	}
	
	public LLVMVoidType(SWIGTYPE_p_LLVMOpaqueType t) {
		super(t);
		assert(Core.LLVMGetTypeKind(t) == LLVMTypeKind.LLVMVoidTypeKind);
	}
}
