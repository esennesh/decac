package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueValue;
import jllvm.llvm.LLVMAttribute;

public class LLVMArgument extends LLVMValue {
	public LLVMArgument() {
		instance = null;
	}
	
	public LLVMArgument(SWIGTYPE_p_LLVMOpaqueValue val) {
		instance = val;
	}
	
	public LLVMFunction getParent() {
		return LLVMFunction.getFunction(Core.LLVMGetParamParent(instance));
	}
	
	public LLVMArgument getNextParameter() {
		return new LLVMArgument(Core.LLVMGetNextParam(instance));
	}
	
	public LLVMArgument getPreviousParameter() {
		return new LLVMArgument(Core.LLVMGetPreviousParam(instance));
	}
	
	public void addAttribute(LLVMAttribute attr) {
		Core.LLVMAddAttribute(instance,attr);
	}
	
	public void removeAttribute(LLVMAttribute attr) {
		Core.LLVMRemoveAttribute(instance,attr);
	}
	
	public void setParameterAlignment(long alignment) {
		Core.LLVMSetParamAlignment(instance,alignment);
	}
}
