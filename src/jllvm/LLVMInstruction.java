package jllvm;

import jllvm.llvm.Core;
import jllvm.LLVMBasicBlock;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueValue;

public class LLVMInstruction extends LLVMUser {
	public LLVMInstruction(SWIGTYPE_p_LLVMOpaqueValue val) {
		instance = val;
		llvm_values.put(instance,this);
	}

	protected LLVMInstruction() {
		instance = null;
	}
	
	public LLVMBasicBlock getParent() {
		return LLVMBasicBlock.getBasicBlock(Core.LLVMGetInstructionParent(instance));
	}
	
	public LLVMInstruction getNextInstruction() {
		return new LLVMInstruction(Core.LLVMGetNextInstruction(instance));
	}
	
	public LLVMInstruction getPreviousInstruction() {
		return new LLVMInstruction(Core.LLVMGetPreviousInstruction(instance));
	}
}
