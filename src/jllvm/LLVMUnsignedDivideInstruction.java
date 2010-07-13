package jllvm;

import jllvm.llvm.Core;

public class LLVMUnsignedDivideInstruction extends LLVMArithmeticInstruction {
	public LLVMUnsignedDivideInstruction(LLVMInstructionBuilder builder,LLVMValue lhs,LLVMValue rhs,String name) {
		super(Core.LLVMBuildUDiv(builder.getInstance(),lhs.getInstance(),rhs.getInstance(),name));
	}
}
