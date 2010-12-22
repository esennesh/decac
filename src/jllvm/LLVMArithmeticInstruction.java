package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueValue;

public class LLVMArithmeticInstruction extends LLVMInstruction {
	private boolean fpType;
	
	public boolean isFloatingPoint() {
		return fpType;
	}
	
	public LLVMArithmeticInstruction(SWIGTYPE_p_LLVMOpaqueValue val,boolean fp) {
		super(val);
		fpType = fp;
	}
}
