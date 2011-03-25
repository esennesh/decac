package jllvm;

import jllvm.llvm.Core;

public class LLVMStoreInstruction extends LLVMInstruction {
	public LLVMStoreInstruction(LLVMInstructionBuilder builder,LLVMValue value,LLVMValue pointer) {
		super(Core.LLVMBuildStore(builder.getInstance(),value.getInstance(),pointer.getInstance()));
		assert(pointer.typeOf() instanceof LLVMPointerType);
	}
}
