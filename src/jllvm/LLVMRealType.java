package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueType;
import jllvm.llvm.LLVMTypeKind;

public class LLVMRealType extends LLVMType {
	public LLVMRealType(SWIGTYPE_p_LLVMOpaqueType tr) {
		super(tr);
		LLVMTypeKind kind = getTypeKind();
		assert(kind == LLVMTypeKind.LLVMFloatTypeKind || kind == LLVMTypeKind.LLVMDoubleTypeKind || kind == LLVMTypeKind.LLVMX86_FP80TypeKind || kind == LLVMTypeKind.LLVMFP128TypeKind || kind == LLVMTypeKind.LLVMPPC_FP128TypeKind);
	}
}
