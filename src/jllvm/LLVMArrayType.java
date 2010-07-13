package jllvm;

import jllvm.llvm.Core;

/* Implements all methods for array types specified in Core.h */
public class LLVMArrayType extends LLVMSequenceType {
	public LLVMArrayType(LLVMType element_type,long num_elements) {
		super(Core.LLVMArrayType(element_type.getInstance(),num_elements));
	}
	
	public long getLength() {
		return Core.LLVMGetArrayLength(instance);
	}
}
