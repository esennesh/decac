package jllvm;

import jllvm.llvm.Core;

public class LLVMSubtractInstruction extends LLVMArithmeticInstruction {
	public LLVMSubtractInstruction(LLVMInstructionBuilder builder,LLVMValue lhs,LLVMValue rhs,String name) {
		super(Core.LLVMBuildSub(builder.getInstance(),lhs.getInstance(),rhs.getInstance(),name));
	}
}
