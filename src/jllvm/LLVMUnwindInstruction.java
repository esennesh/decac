package jllvm;

import jllvm.llvm.Core;

public class LLVMUnwindInstruction extends LLVMTerminatorInstruction {
	public LLVMUnwindInstruction(LLVMInstructionBuilder builder) {
		instance = Core.LLVMBuildUnwind(builder.getInstance());
	}
}
