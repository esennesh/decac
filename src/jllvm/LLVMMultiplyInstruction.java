package jllvm;

import jllvm.llvm.Core;

public class LLVMMultiplyInstruction extends LLVMArithmeticInstruction {
	public LLVMMultiplyInstruction(LLVMInstructionBuilder builder,LLVMValue lhs,LLVMValue rhs,String name) {
		super(Core.LLVMBuildMul(builder.getInstance(),lhs.getInstance(),rhs.getInstance(),name));
	}
}
