package jllvm;

import jllvm.llvm.Core;
import jllvm.llvm.LLVMTypeKind;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueType;
import java.util.*;

/* Implements all methods from Core.h dealing with the base class Type. */
public class LLVMType {
	protected static HashMap<SWIGTYPE_p_LLVMOpaqueType,LLVMType> llvm_types;
	protected SWIGTYPE_p_LLVMOpaqueType instance;
	
	public LLVMTypeKind getTypeKind() {
		return Core.LLVMGetTypeKind(instance);
	}
	
	public SWIGTYPE_p_LLVMOpaqueType getInstance() {
		return instance;
	}
	
	public boolean equals(Object obj) {
		if(obj instanceof LLVMType)
			return ((LLVMType)obj).instance == instance;
		else
			return false;
	}
	
	protected LLVMType() {
		instance = null;
		if(llvm_types == null)
			llvm_types = new HashMap<SWIGTYPE_p_LLVMOpaqueType,LLVMType>();
	}
	
	public LLVMType(SWIGTYPE_p_LLVMOpaqueType tr) {
		instance = tr;
		if(llvm_types == null)
			llvm_types = new HashMap<SWIGTYPE_p_LLVMOpaqueType,LLVMType>();
		llvm_types.put(instance,this);
	}
	
	public static LLVMType getType(SWIGTYPE_p_LLVMOpaqueType tr) {
		if(llvm_types == null)
			llvm_types = new HashMap<SWIGTYPE_p_LLVMOpaqueType,LLVMType>();
		LLVMType result = llvm_types.get(tr);
		assert(result != null);
		return result;
	}
}
