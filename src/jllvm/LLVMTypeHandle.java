package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueTypeHandle;

/* Implements the methods on type handles from Core.h */
public class LLVMTypeHandle {
	protected SWIGTYPE_p_LLVMOpaqueTypeHandle instance;
	
	public LLVMTypeHandle(LLVMType potentiallyAbstract) {
		instance = Core.LLVMCreateTypeHandle(potentiallyAbstract.getInstance());
	}
	
	public static void refineType(LLVMType abstractType,LLVMType concreteType) {
		Core.LLVMRefineType(abstractType.getInstance(),concreteType.getInstance());
	}
	
	public LLVMType resolve() {
		return LLVMType.getType(Core.LLVMResolveTypeHandle(instance));
	}
	
	protected void finalize() {
		Core.LLVMDisposeTypeHandle(instance);
	}
}
