package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.LLVMIntPredicate;

public class LLVMIntegerComparison extends LLVMComparisonInstruction {
	public LLVMIntegerComparison(LLVMInstructionBuilder builder,LLVMIntPredicate Op,LLVMValue lhs,LLVMValue rhs,String name) {
		instance = Core.LLVMBuildICmp(builder.getInstance(),Op,lhs.getInstance(),rhs.getInstance(),name);
	}
}
