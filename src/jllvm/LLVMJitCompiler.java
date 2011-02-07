package jllvm;

import jllvm.llvm.ExecutionEngine;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueValue;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueExecutionEngine;
import jllvm.llvm.SWIGTYPE_p_p_LLVMOpaqueExecutionEngine;

public class LLVMJitCompiler extends LLVMExecutionEngine {
	//LLVMCreateJITCompilerForModule
	public LLVMInterpreter(LLVMModule mod) {
		SWIGTYPE_p_p_LLVMOpaqueExecutionEngine engines = ExecutionEngine.new_LLVMExecutionEngineRefArray(1);
		SWIGTYPE_p_p_char outerrs = ExecutionEngine.new_StringArray(1);
		int success = ExecutionEngine.LLVMCreateJITCompilerForModule(engines,mod.getInstance(),outerrs);
		String outerr = ExecutionEngine.StringArray_getitem(outerrs,0);
		ExecutionEngine.delete_StringArray(outerrs); outerrs = null;
		instance = ExecutionEngine.LLVMExecutionEngineRefArray_getitem(engines,0);
		ExecutionEngine.delete_LLVMExecutionEngineRefArray(engines); engines = null;
		if(!success)
			throw new Exception(outerr);
	}
}
