package jllvm;

import jllvm.llvm.Core;

public class LLVMSignedRemainderInstruction extends LLVMArithmeticInstruction {
	public LLVMSignedRemainderInstruction(LLVMInstructionBuilder builder,LLVMValue lhs,LLVMValue rhs,String name) {
		super(Core.LLVMBuildSRem(builder.getInstance(),lhs.getInstance(),rhs.getInstance(),name));
	}
}
