package jllvm;

import jllvm.llvm.Core;

public class LLVMSignedDivideInstruction extends LLVMArithmeticInstruction {
	public LLVMSignedDivideInstruction(LLVMInstructionBuilder builder,LLVMValue lhs,LLVMValue rhs,String name) {
		super(Core.LLVMBuildSDiv(builder.getInstance(),lhs.getInstance(),rhs.getInstance(),name));
	}
}
