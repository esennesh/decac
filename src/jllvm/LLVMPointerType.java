package jllvm;

import jllvm.llvm.Core;

/* Implements all methods for pointer types specified in Core.h */
public class LLVMPointerType extends LLVMSequenceType {
	public LLVMPointerType(LLVMType element_type,int addressSpace) {
		super(Core.LLVMPointerType(element_type.getInstance(),addressSpace));
	}
	
	public long getAddressSpace() {
		return Core.LLVMGetPointerAddressSpace(instance);
	}
}
