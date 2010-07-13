package jllvm;

import jllvm.llvm.Core;

public class LLVMUnsignedRemainderInstruction extends LLVMArithmeticInstruction {
	public LLVMUnsignedRemainderInstruction(LLVMInstructionBuilder builder,LLVMValue lhs,LLVMValue rhs,String name) {
		super(Core.LLVMBuildURem(builder.getInstance(),lhs.getInstance(),rhs.getInstance(),name));
	}
}
