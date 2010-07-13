package jllvm;

import jllvm.llvm.Core;

public class LLVMAddInstruction extends LLVMArithmeticInstruction {
	public LLVMAddInstruction(LLVMInstructionBuilder builder,LLVMValue lhs,LLVMValue rhs,String name) {
		super(Core.LLVMBuildAdd(builder.getInstance(),lhs.getInstance(),rhs.getInstance(),name));
	}
}
