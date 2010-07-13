package jllvm;

import jllvm.llvm.Core;

public class LLVMExtendCast extends LLVMCastInstruction {
	public enum ExtendType { ZERO,SIGN,FLOAT };
	
	protected ExtendType instructionType;
	
	public ExtendType getInstructionType() {
		return instructionType;
	}
	
	public LLVMExtendCast(ExtendType type,LLVMInstructionBuilder builder,LLVMValue val,LLVMIntegerType destType,String name) {
		super(destType);
		assert(val.typeOf() instanceof LLVMIntegerType);
		instructionType = type;
		switch(type) {
			case ZERO:
				instance = Core.LLVMBuildZExt(builder.getInstance(),val.getInstance(),destType.getInstance(),name);
			case SIGN:
				instance = Core.LLVMBuildSExt(builder.getInstance(),val.getInstance(),destType.getInstance(),name);
			case FLOAT:
				instance = Core.LLVMBuildFPExt(builder.getInstance(),val.getInstance(),destType.getInstance(),name);
		}
	}
}
