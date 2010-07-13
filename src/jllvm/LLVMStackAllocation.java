package jllvm;

import jllvm.llvm.Core;

public class LLVMStackAllocation extends LLVMAllocationInstruction {
	public LLVMStackAllocation(LLVMInstructionBuilder builder,LLVMType type,LLVMValue number,String name) {
		if(number != null) {
			assert(number.typeOf() == LLVMIntegerType.i32);
			instance = Core.LLVMBuildArrayAlloca(builder.getInstance(),type.getInstance(),number.getInstance(),name);
		}
		else
			instance = Core.LLVMBuildAlloca(builder.getInstance(),type.getInstance(),name);
	}
}
