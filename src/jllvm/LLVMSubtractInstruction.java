package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueValue;

public class LLVMSubtractInstruction extends LLVMArithmeticInstruction {
	private static SWIGTYPE_p_LLVMOpaqueValue buildInstruction(LLVMInstructionBuilder builder,LLVMValue lhs,LLVMValue rhs,boolean fp,String name) {
		if(fp)
			return Core.LLVMBuildFSub(builder.getInstance(),lhs.getInstance(),rhs.getInstance(),name);
		else
			return Core.LLVMBuildSub(builder.getInstance(),lhs.getInstance(),rhs.getInstance(),name);
	}
	
	public LLVMSubtractInstruction(LLVMInstructionBuilder builder,LLVMValue lhs,LLVMValue rhs,boolean fp,String name) {
		super(buildInstruction(builder,lhs,rhs,fp,name),fp);
	}
}
