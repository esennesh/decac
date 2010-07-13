package jllvm;

import jllvm.llvm.Core;

public abstract class LLVMCastInstruction extends LLVMInstruction {
	protected LLVMType destination;
	
	public LLVMType getDestination() {
		return destination;
	}
	
	public LLVMCastInstruction(LLVMType destType) {
		destination = destType;
	}
}
