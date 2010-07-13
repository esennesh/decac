package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaquePassManager;

public abstract class LLVMPassManager {
	protected SWIGTYPE_p_LLVMOpaquePassManager instance;
	
	public SWIGTYPE_p_LLVMOpaquePassManager getInstance() {
		return instance;
	}
	
	protected void finalize() {
		Core.LLVMDisposePassManager(instance);
	}
}
