package jllvm;

import jllvm.llvm.ExecutionEngine;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueValue;
import jllvm.llvm.SWIGTYPE_p_LLVMOpaqueExecutionEngine;
import jllvm.llvm.SWIGTYPE_p_p_LLVMOpaqueExecutionEngine;

public class LLVMExecutionEngine {
	protected SWIGTYPE_p_LLVMOpaqueExecutionEngine instance;
  
	//LLVMCreateExecutionEngineForModule
	public LLVMExecutionEngine(LLVMModule mod) {
		SWIGTYPE_p_p_LLVMOpaqueExecutionEngine engines = ExecutionEngine.new_LLVMExecutionEngineRefArray(1);
		SWIGTYPE_p_p_char outerrs = ExecutionEngine.new_StringArray(1);
		int success = ExecutionEngine.LLVMCreateExecutionEngineForModule(engines,mod.getInstance(),outerrs);
		String outerr = ExecutionEngine.StringArray_getitem(outerrs,0);
		ExecutionEngine.delete_StringArray(outerrs); outerrs = null;
		instance = ExecutionEngine.LLVMExecutionEngineRefArray_getitem(engines,0);
		ExecutionEngine.delete_LLVMExecutionEngineRefArray(engines); engines = null;
		if(!success)
			throw new Exception(outerr);
	}
	
	void runStaticConstructors() {
		ExecutionEngine.LLVMRunStaticConstructors(instance);
	}
	
	void runStaticDestructors() {
		ExecutionEngine.LLVMRunStaticDestructors(instance);
	}
	
	int runFunctionAsMain(LLVMFunction f,String[] argv,String[] envp) {
		SWIGTYPE_p_p_char args = ExecutionEngine.new_StringArray(argv.length);
		SWIGTYPE_p_p_char envs = ExecutionEngine.new_StringArray(envp.length);
		for(int i=0;i<argv.length;i++)
			ExecutionEngine.StringArray_setitem(args,i,argv[i]);
		for(int i=0;i<envp.length;i++)
			ExecutionEngine.StringArray_setitem(envs,i,envp[i]);
		int result = ExecutionEngine.LLVMRunFunctionAsMain(instance,f.getInstance(),argv.length,args,envs);
		ExecutionEngine.delete_StringArray(envs);
		ExecutionEngine.delete_StringArray(args);
		return result;
	}
	
	void finalize() {
		LLVMDisposeExecutionEngine(instance);
	}
}
