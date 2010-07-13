package jllvm;

import jllvm.llvm.Core;

public class LLVMStoreInstruction extends LLVMInstruction {
	public LLVMStoreInstruction(LLVMInstructionBuilder builder,LLVMValue value,LLVMValue pointer) {
		assert(pointer.typeOf() instanceof LLVMPointerType);
		instance = Core.LLVMBuildStore(builder.getInstance(),value.getInstance(),pointer.getInstance());
	}
}
