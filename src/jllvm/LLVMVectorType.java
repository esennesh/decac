package jllvm;

import jllvm.llvm.Core;

/* Implements all methods for vector types specified in Core.h */
public class LLVMVectorType extends LLVMSequenceType {
	public LLVMVectorType(LLVMType element_type,long num_elements) {
		super(Core.LLVMVectorType(element_type.getInstance(),num_elements));
	}
	
	public long getSize() {
		return Core.LLVMGetVectorSize(instance);
	}
}
