package jllvm;

import jllvm.llvm.ExecutionEngine;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueGenericValue;
import jllvm.llvm.SWIGTYPE_p_p_LLVMOpaqueGenericValue;
import java.math.BigInteger;

public class LLVMGenericInt extends LLVMGenericValue {
	public LLVMGenericInt(LLVMType t,java.math.BigInteger n,boolean isSigned) {
		super(ExecutionEngine.LLVMCreateGenericValueOfInt(t.getInstance(),n,isSigned ? 1 : 0));
	}
	
	public java.math.BigInteger toInt(boolean isSigned) {
		return ExecutionEngine.LLVMGenericValueToInt(instance,isSigned ? 1 : 0);
	}
	
	public long intWidth() {
		return ExecutionEngine.LLVMGenericValueIntWidth(instance);
	}
}
