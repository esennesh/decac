package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueType;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueValue;

public class LLVMConstantReal extends LLVMConstant {
	public LLVMConstantReal(LLVMType realType,double N) {
		super(Core.LLVMConstReal(realType.getInstance(),N));
	}
	
	public LLVMConstantReal(LLVMType realType,String text) {
		super(Core.LLVMConstRealOfString(realType.getInstance(),text));
	}
	
	public LLVMConstantExpression truncate(LLVMRealType targetType) {
		return new LLVMConstantExpression(Core.LLVMConstFPTrunc(instance,targetType.getInstance()));
	}
	
	public LLVMConstantExpression extend(LLVMRealType targetType) {
		return new LLVMConstantExpression(Core.LLVMConstFPExt(instance,targetType.getInstance()));
	}
	
	public LLVMConstantExpression realToInteger(LLVMIntegerType targetType,boolean signed) {
		if(signed)
			return new LLVMConstantExpression(Core.LLVMConstFPToSI(instance,targetType.getInstance()));
		else
			return new LLVMConstantExpression(Core.LLVMConstFPToUI(instance,targetType.getInstance()));
	}
}
