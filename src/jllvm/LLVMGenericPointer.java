package jllvm;

import jllvm.llvm.ExecutionEngine;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueGenericValue;
import jllvm.llvm.SWIGTYPE_p_p_LLVMOpaqueGenericValue;
import jllvm.llvm.SWIGTYPE_p_void;

public class LLVMGenericPointer extends LLVMGenericValue {
	public LLVMGenericPointer(SWIGTYPE_p_void p) {
		super(ExecutionEngine.LLVMCreateGenericValueOfPointer(p));
	}
	
	public SWIGTYPE_p_void toPointer(boolean isSigned) {
		return ExecutionEngine.LLVMGenericValueToPointer(instance);
	}
}
