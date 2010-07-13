package jllvm;

import jllvm.llvm.Core;

public class LLVMFloatRemainderInstruction extends LLVMArithmeticInstruction {
	public LLVMFloatRemainderInstruction(LLVMInstructionBuilder builder,LLVMValue lhs,LLVMValue rhs,String name) {
		super(Core.LLVMBuildFRem(builder.getInstance(),lhs.getInstance(),rhs.getInstance(),name));
	}
}
