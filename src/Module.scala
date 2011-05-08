package decac

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import jllvm._
import jllvm.llvm.LLVMLinkage

abstract trait Definition extends Scopeable {
  override def scope: Scope[Definition]
}

class Module(m: Option[Module],n: String) extends Scope[Definition](m) with Definition {
  override val parent: Option[Module] = m
  override val name = n
  val compiledModule: LLVMModule = new LLVMModule(name)
  var compiled = false
  override val scope: Module = m.getOrElse(this) 
  val imports: Map[AnyRef,Definition] = new HashMap[AnyRef,Definition]()
  protected var fPath: Option[String] = None
  def path: String = fPath.get
  def setPath(str: String) = {
    fPath = Some(str)
  }
  
  override def lookup(name: String): Definition = symbols.get(name) match {
    case Some(result) => result
    case None => parent match {
      case Some(parent) => parent.lookup(name)
      case None => throw new UndeclaredIdentifierException(name)
    }
  }
  
  def lookupImport(name: List[String]): Definition = name.tail match {
    case Nil => lookup(name.head)
    case _ => lookup(name.head) match {
      case mod: Module => mod.lookup(name.tail)
      case _ => throw new Exception("Name of non-module " + name.head + " used as module identifier in qualified identifier.")
    }
  }
  
  override def lookup(name: List[String]): Definition = lookupImport(name) match {
    case function: FunctionDefinition => function.signature match {
      case _: FunctionArrow => imports.get(function.specialized.head) match {
        case Some(func) => func
        case None => {
          val result = new ImportedFunction(function.specialized.head,this)
          imports.put(function.specialized.head,result)
          result
        }
      }
      case _ => function
    }
    case mvar: ModuleVariableDefinition => imports.get(mvar) match {
      case Some(modvar) => modvar
      case None => {
        val result = new ImportedVariable(mvar,this)
        imports.put(mvar,result)
        result
      }
    }
  }
  
  def define(obj: Definition) = declare(obj)
  
  def compile: LLVMModule = {
    if(!compiled) {
      for(definition <- symbols.values) definition match {
        case function: FunctionDefinition => function.specialized.foreach(func => func.compile)
        case defin: TypeDefinition => defin.getSpecializations.foreach(gamma => compiledModule.addTypeName(name,gamma.compile))
        case global: ModuleVariableDefinition => global.build 
        //TODO: Add code for constant expressions, and use it to set the initializer on global variables.
        case module: Module => if(module.parent == Some(this)) module.compile
      }
      compiled = true
    }
    compiledModule
  }
  
  def writeBitcode: Unit = (new LLVMBitWriter(compile)).writeBitcodeToFile(path + name + ".bc")
  
  override def scopeType: ScopeType = new GlobalScopeType(Some(this))
}

object GlobalScope extends Module(None,"") {
  override val parent = None
}

class ImportedFunction(spec: SpecializedFunction,importer: Module) extends Definition with SpecializedFunction {
  override val name: String = spec.generic.name
  override val scope: Module = importer
  override val generic = spec.generic
  override val signature: FunctionArrow = spec.signature
  override val compile: LLVMFunction = {
    val f = new LLVMFunction(scope.compiledModule,generic.name + signature.toString,signature.compile)
    f.setLinkage(LLVMLinkage.LLVMExternalLinkage)
    f
  }
}

class ImportedVariable(mvar: ModuleVariableDefinition,importer: Module) extends Definition with SpecializedVariableBinding {
  override val name: String = mvar.name
  override val scope: Module = importer
  val original = mvar
  val global: LLVMGlobalVariable = {
    val result = scope.compiledModule.addGlobal(mvar.variableType.compile,mvar.name)
    result.setThreadLocal(mvar.mutable)
    result.setConstant(!mvar.mutable)
    result.setLinkage(LLVMLinkage.LLVMExternalLinkage)
    result
  }
  override val variableType: GammaType = mvar.variableType
  override def compile(builder: LLVMInstructionBuilder): LLVMValue = global
  override def load(builder: LLVMInstructionBuilder): LLVMValue = new LLVMLoadInstruction(builder,global,"load" + mvar.name)
  override def store(builder: LLVMInstructionBuilder,value: LLVMValue): LLVMValue = new LLVMStoreInstruction(builder,value,global)
}
