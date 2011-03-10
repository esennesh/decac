package decac

import jllvm._

class ImplicitUpcast(expr: SpecializedExpression,gamma: GammaType) extends SpecializedExpression(gamma) {
  assert(TauOrdering.lteq(expr.expressionType,expressionType))
  override def children: List[SpecializedExpression] = (expr :: Nil)
  override def compile(builder: LLVMInstructionBuilder,scope: Scope[_]): LLVMValue = if(TauOrdering.lt(children.apply(0).expressionType,expressionType)) {
    val child = children.apply(0).compile(builder,scope)
    (children.apply(0).expressionType,expressionType) match {
      case (unsignedx: UnsignedIntegerGamma,realy: RealGamma) => {
        val doubled = new LLVMIntegerToFloatCast(builder,child,DoubleGamma.compile,"cast",LLVMIntegerToFloatCast.IntCastType.UNSIGNED)
        new LLVMExtendCast(LLVMExtendCast.ExtendType.FLOAT,builder,doubled,realy.compile,"cast")
      }
      
      case (integerx: IntegerGamma,realy: RealGamma) => {
        val doubled = new LLVMIntegerToFloatCast(builder,child,DoubleGamma.compile,"cast",LLVMIntegerToFloatCast.IntCastType.SIGNED)
        new LLVMExtendCast(LLVMExtendCast.ExtendType.FLOAT,builder,doubled,realy.compile,"cast")
      }
      
      case (realx: RealGamma,realy: RealGamma) => new LLVMExtendCast(LLVMExtendCast.ExtendType.FLOAT,builder,child,realy.compile,"cast")
      
      //TODO: Fill in the rest of these cases.
      case (sumx: SumType,sumy: SumType) => (sumx.enumeration,sumy.enumeration) match {
        case (true,true) => new LLVMExtendCast(LLVMExtendCast.ExtendType.ZERO,builder,child,sumy.compile,"cast")
        case (true,false) => new LLVMInsertValueInstruction(builder,new LLVMUndefinedValue(sumy.compile),child,0,"variant")
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
      }
      
      case (px: PointerType,py: PointerType) => new LLVMBitCast(builder,child,py.compile,"pointer_cast")
      
      case (ux: UnsignedIntegerGamma,uy: UnsignedIntegerGamma) => new LLVMExtendCast(LLVMExtendCast.ExtendType.ZERO,builder,child,uy.compile,"cast")
      case (ix: IntegerGamma,iy: IntegerGamma) => new LLVMExtendCast(LLVMExtendCast.ExtendType.SIGN,builder,child,iy.compile,"cast")
      
      case _ => throw new Exception("Unknown implicit upcast from " + children.apply(0).expressionType.toString + " to " + expressionType.toString)
    }
  }
  else
    children.apply(0).compile(builder,scope)
}
