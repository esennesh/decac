package org.deca.compiler.expression

import org.jllvm._
import org.jllvm.bindings._

import org.deca.compiler.definition._
import org.deca.compiler.signature._

class ImplicitUpcast(val expression: Expression,upcast: MonoType) extends Expression {
  expType = upcast
  if(!TypeOrdering.lteq(expression.expType,expType))
    throw new Exception(expression.expType.toString + " </: " + expType.toString)
  override def constrain(lui: LatticeUnificationInstance): Unit = Unit
  override def check(lui: LatticeUnificationInstance): Unit = Unit
  
  override def substitute(sub: SignatureSubstitution): Unit = {
    expression.substitute(sub)
    expType = sub.solve(expType).asInstanceOf[MonoType]
  }
  override def specialize(spec: SignatureSubstitution,specScope: Scope): Expression =
    new ImplicitUpcast(expression.specialize(spec,specScope),spec.solve(upcast).asInstanceOf[MonoType])
  override val children: List[Expression] = (expression :: Nil)
  override def compile(builder: LLVMInstructionBuilder,scope: Scope,instantiation: Module): LLVMValue = if(TypeOrdering.lt(expression.expType,expType)) {
    val child = expression.compile(builder,scope,instantiation)
    (expression.expType,expType) match {
      case (unsignedx: UnsignedIntegerType,realy: RealType) => {
        val doubled = new LLVMIntegerToFloatCast(builder,child,DoubleType.compile,"cast",LLVMIntegerToFloatCast.IntCastType.UNSIGNED)
        new LLVMExtendCast(LLVMExtendCast.ExtendType.FLOAT,builder,doubled,realy.compile,"cast")
      }
      
      case (integerx: IntegerType,realy: RealType) => {
        val doubled = new LLVMIntegerToFloatCast(builder,child,DoubleType.compile,"cast",LLVMIntegerToFloatCast.IntCastType.SIGNED)
        new LLVMExtendCast(LLVMExtendCast.ExtendType.FLOAT,builder,doubled,realy.compile,"cast")
      }
      
      case (realx: RealType,realy: RealType) => new LLVMExtendCast(LLVMExtendCast.ExtendType.FLOAT,builder,child,realy.compile,"cast")
      
      //TODO: Fill in the rest of these cases.
      case (sumx: SumType,sumy: SumType) => (sumx.enumeration,sumy.enumeration) match {
        case (true,true) => new LLVMExtendCast(LLVMExtendCast.ExtendType.ZERO,builder,child,sumy.compile,"cast")
        case (true,false) => new LLVMInsertValueInstruction(builder,new LLVMUndefinedValue(sumy.compile),new LLVMExtendCast(LLVMExtendCast.ExtendType.ZERO,builder,child,sumy.tagRepresentation,"cast"),0,"variant")
        case (false,false) => {
          //Insert the tag into the resulting variant.
          val tag = new LLVMExtractValueInstruction(builder,child,0,"extract")
          val variant = new LLVMInsertValueInstruction(builder,new LLVMUndefinedValue(sumy.compile),tag,0,"insert")
          
          //Allocate memory space for the resulting variant into which to stuff the old data.
          val result = new LLVMStackAllocation(builder,sumy.compile,LLVMConstantInteger.constantInteger(Nat.compile,1,false),"nonsense")
          new LLVMStoreInstruction(builder,variant,result)
          
          //Get the pointer to the new variant's contents, fetch the old variant's contents, cast the pointer, and store the old into the new.
          val indices = List(LLVMConstantInteger.constantInteger(Nat.compile,0,false),LLVMConstantInteger.constantInteger(Nat.compile,1,false))
          var pContents: LLVMValue = new LLVMGetElementPointerInstruction(builder,result,indices.toArray,"gep")
          val oldContents = new LLVMExtractValueInstruction(builder,child,1,"extract")
          pContents = new LLVMBitCast(builder,pContents,new LLVMPointerType(oldContents.typeOf,0),"pointer_cast")
          new LLVMStoreInstruction(builder,oldContents,pContents)
          //Return the entire new variant.
          new LLVMLoadInstruction(builder,result,"load")
        }
        case (false,true) => throw new Exception("Cannot implicitly upcast a variant to an enumeration because such a subtyping relation is impossible.  What the fuck happened?")
      }
      
      case (px: PointerType,py: PointerType) => new LLVMBitCast(builder,child,py.compile,"pointer_cast")
      
      case (ux: UnsignedIntegerType,uy: UnsignedIntegerType) => new LLVMExtendCast(LLVMExtendCast.ExtendType.ZERO,builder,child,uy.compile,"cast")
      case (ix: IntegerType,iy: IntegerType) => new LLVMExtendCast(LLVMExtendCast.ExtendType.SIGN,builder,child,iy.compile,"cast")
      
      case (_,UnitType) => child
      
      case _ => throw new Exception("Unknown implicit upcast from " + expression.expType.toString + " to " + expType.toString)
    }
  }
  else
    expression.compile(builder,scope,instantiation)
}
