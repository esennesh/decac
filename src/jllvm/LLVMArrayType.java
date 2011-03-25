package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueType;
import jllvm.llvm.LLVMTypeKind;

/* Implements all methods for array types specified in Core.h */
public class LLVMArrayType extends LLVMSequenceType {
	public LLVMArrayType(LLVMType element_type,long num_elements) {
		super(Core.LLVMArrayType(element_type.getInstance(),num_elements));
	}
	
	public LLVMArrayType(SWIGTYPE_p_LLVMOpaqueType t) {
		super(t);
		assert(Core.LLVMGetTypeKind(t) == LLVMTypeKind.LLVMArrayTypeKind);
	}
	
	public long getLength() {
		return Core.LLVMGetArrayLength(instance);
	}
}
