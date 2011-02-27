package jllvm;

import jllvm.llvm.Core;

public class LLVMUndefinedValue extends LLVMValue {
	public LLVMUndefinedValue(LLVMType t) {
		instance = Core.LLVMGetUndef(t.getInstance());
	}
}
