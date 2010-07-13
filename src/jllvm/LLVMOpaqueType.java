package jllvm;

import jllvm.llvm.Core;

public class LLVMOpaqueType extends LLVMType {
	public LLVMOpaqueType() {
		super(Core.LLVMOpaqueType());
	}
}
