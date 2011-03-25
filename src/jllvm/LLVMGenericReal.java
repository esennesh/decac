package jllvm;

import jllvm.llvm.ExecutionEngine;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueGenericValue;
import jllvm.llvm.SWIGTYPE_p_p_LLVMOpaqueGenericValue;
import jllvm.llvm.SWIGTYPE_p_void;

public class LLVMGenericReal extends LLVMGenericValue {
	public LLVMGenericReal(LLVMRealType t,double n) {
		super(ExecutionEngine.LLVMCreateGenericValueOfFloat(t.getInstance(),n));
	}
	
	public double toReal(LLVMRealType t) {
		return ExecutionEngine.LLVMGenericValueToFloat(t.getInstance(),instance);
	}
}
