package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.Target;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaquePassManager;

public abstract class LLVMPassManager {
	protected SWIGTYPE_p_LLVMOpaquePassManager instance;
	
	public SWIGTYPE_p_LLVMOpaquePassManager getInstance() {
		return instance;
	}
	
	public void addTargetData(LLVMTargetData target) {
		Target.LLVMAddTargetData(target.getInstance(),instance);
	}
	
	protected void finalize() {
		Core.LLVMDisposePassManager(instance);
	}
}
