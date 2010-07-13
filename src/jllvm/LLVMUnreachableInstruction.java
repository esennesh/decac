package jllvm;

import jllvm.llvm.Core;

public class LLVMUnreachableInstruction extends LLVMTerminatorInstruction {
	public LLVMUnreachableInstruction(LLVMInstructionBuilder builder) {
		instance = Core.LLVMBuildUnreachable(builder.getInstance());
	}
}
