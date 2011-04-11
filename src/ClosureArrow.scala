package decac;

import jllvm.LLVMType

class ClosureArrow(d: List[TauType],r: TauType) extends RhoType with ArrowType {
  override val domain: List[TauType] = d
  override val range = r
  override val signature = new FunctionArrow(domain,range)
  val representation: SumType = {
    val alpha = new TauVariable
    val struct = new RecordProduct(List(RecordMember(None,alpha),RecordMember(None,new FunctionArrow(new PointerType(alpha) :: domain,range))))
    val existential = ExistentialConstants.pack(struct,alpha).asInstanceOf[RecordProduct]
    new SumType(TaggedProduct(DataConstructor(None,None),existential) :: TaggedProduct(DataConstructor(None,None),new RecordProduct(List(RecordMember(None,signature)))) :: Nil)
  }
  
  override def tagged: Boolean = false
  override def map(f: (TauType) => TauType): ClosureArrow = new ClosureArrow(domain.map(f),f(range))
  
  override def contents: List[TauType] = {
    var result: List[TauType] = Nil
    domain.foreach(tau => tau match {
      case rho: RhoType => result = result ++ rho.contents
      case _ => result = tau :: result
    })
    range match {
      case rho: RhoType => result = result ++ rho.contents
      case _ => result = range :: result
    }
    result
  }
  
  def compile: LLVMType = representation.compile
  
  override def mangle: String = "(" + (domain match { case head :: tail => head.toString + tail.foldLeft("")((x: String,y: TauType) => x + "," + y.toString) case Nil => "" }) + ")->" + range.toString
}
