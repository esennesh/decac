package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueType;
import jllvm.llvm.LLVMTypeKind;

/* Implements all methods for pointer types specified in Core.h */
public class LLVMPointerType extends LLVMSequenceType {
	public LLVMPointerType(LLVMType element_type,int addressSpace) {
		super(Core.LLVMPointerType(element_type.getInstance(),addressSpace));
	}
	
	public LLVMPointerType(SWIGTYPE_p_LLVMOpaqueType t) {
		super(t);
		assert(Core.LLVMGetTypeKind(t) == LLVMTypeKind.LLVMPointerTypeKind);
	}
	
	public long getAddressSpace() {
		return Core.LLVMGetPointerAddressSpace(instance);
	}
}
