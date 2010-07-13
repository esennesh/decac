package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueValue;
import jllvm.LLVMValue;

/* Core.h doesn't specify anything about the type User, so there's pretty much nothing here. */
public class LLVMUser extends LLVMValue {
	protected LLVMUser() {
		instance = null;
	}
	
	public LLVMUser(SWIGTYPE_p_LLVMOpaqueValue val) {
		super(Core.LLVMIsAUser(val));
	}
}
