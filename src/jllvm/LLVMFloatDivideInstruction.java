package jllvm;

import jllvm.llvm.Core;

public class LLVMFloatDivideInstruction extends LLVMArithmeticInstruction {
	public LLVMFloatDivideInstruction(LLVMInstructionBuilder builder,LLVMValue lhs,LLVMValue rhs,String name) {
		super(Core.LLVMBuildFDiv(builder.getInstance(),lhs.getInstance(),rhs.getInstance(),name));
	}
}
