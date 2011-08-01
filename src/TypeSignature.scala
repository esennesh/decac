package decac

import jllvm._
import scala.collection.immutable.HashSet

object TopType extends MonoType {
  override def compile: LLVMType = throw new TypeException("Top type indicates a type-inference error.")
  override def variables: Set[SignatureVariable[MonoType]] = HashSet.empty
  override def toString: String = "top"
}

object BottomType extends MonoType {
  override def compile: LLVMType = throw new TypeException("Bottom type indicates a type-inference error.")
  override def variables: Set[SignatureVariable[MonoType]] = HashSet.empty
  override def toString: String = "bottom"
}

object UnitType extends MonoType {
  override def compile: LLVMType = new LLVMVoidType
  override def variables: Set[SignatureVariable[MonoType]] = HashSet.empty
  override def toString: String = "unit"
}

case class TypeConstructorCall(constructor: TypeConstructor,params: List[MonoType]) extends MonoType {
  override def compile: LLVMType = constructor.compile(params)
  override def tagged: Boolean = constructor.represent(params).tagged
  override def toString: String = constructor.name + "<" + params.tail.foldLeft(params.head.toString)((s,p) => s + "," + p.toString) + ">"
  override def map(f: (MonoType) => MonoType): MonoType = f(new TypeConstructorCall(constructor,params.map(tau => tau.map(f))))
  override def map(f: (MonoEffect) => MonoEffect): TypeConstructorCall = new TypeConstructorCall(constructor,params.map(tau => tau.map(f)))
  override def map(f: (MonoRegion) => MonoRegion): TypeConstructorCall = new TypeConstructorCall(constructor,params.map(tau => tau.map(f)))
  override def variables: Set[SignatureVariable[MonoType]] = params.foldLeft(HashSet.empty)((result,param) => result ++ param.variables)
}

case class TypeException(error: String) extends Exception("Type error: " + error)

class OpaqueType extends MonoType {
  protected val compiled: LLVMOpaqueType = new LLVMOpaqueType
  override def compile: LLVMType = compiled
  override def toString: String = getClass().getName() + '@' + Integer.toHexString(hashCode())
  override def map(f: (MonoType) => MonoType): MonoType = f(this)
  override def map(f: (MonoEffect) => MonoEffect): MonoType = this
  override def map(f: (MonoRegion) => MonoRegion): MonoType = this
  override def variables: Set[SignatureVariable[MonoType]] = HashSet.empty
}

class RecursiveType(tau: MonoType,loopNode: Option[MonoType]) extends MonoType {
  val innards: MonoType = loopNode match {
    case None => tau
    case Some(alpha) => tau.replace(alpha,this)
  }
  
  def unfold: Tuple2[OpaqueType,MonoType] = {
    val loop = new OpaqueType
    (loop,innards.map(tau => if(tau == this) loop else tau))
  }
  
  override def tagged: Boolean = innards.tagged
  
  override def map(f: (MonoSignature) => MonoSignature): MonoType = f(new RecursiveType(innards.map(tau => if(tau == this) tau else f(tau)),Some(this)))
  override def map(f: (MonoRegion) => MonoRegion): MonoType = new RecursiveType(innards.map(tau => if(tau == this) tau else tau.map(f),Some(this)))
  override def map(f: (MonoEffect) => MonoEffect): MonoType = new RecursiveType(innards.map(tau => if(tau == this) tau else tau.map(f),Some(this)))
  
  override def variables: Set[SignatureVariable[MonoType]] = unfold.variables
  
  override def compile: LLVMType = {
    val opaque = new OpaqueGamma
    val bodyType = innards.map(tau => if(tau == this) opaque else tau).compile
    val bodyHandle = new LLVMTypeHandle(bodyType)
    LLVMTypeHandle.refineType(opaque.compile,bodyType)
    bodyHandle.resolve
  }
  
  override def toString: String = {
    val alpha = new TypeVariable
    "mu " + alpha.toString + "." + innards.map(tau => if(tau == this) alpha else tau).toString
  }
}

case class RecordMember(name: Option[String],tau: MonoType,isPublic: Boolean = true)

class RecordType(f: List[RecordMember]) extends MonoType {
  val fields: List[RecordMember] = f
  val length: Int = fields.length
  
  override def map(f: (MonoType) => MonoType): MonoType = {
    f(new RecordType(fields.map(field => RecordMember(field.name,field.tau.map(f),field.isPublic))))
  }
  override def map(f: (MonoRegion) => MonoRegion): RecordType = {
    new RecordType(fields.map(field => RecordMember(field.name,field.tau.map(f),field.isPublic)))
  }
  override def map(f: (MonoEffect) => MonoEffect): RecordType = {
    new RecordType(fields.map(field => RecordMember(field.name,field.tau.map(f),field.isPublic)))
  }
  override def variables: Set[SignatureVariable[MonoType]] = fields.foldLeft(HashSet.empty)((result,field) => result ++ field.tau.variables)

  override def compile: LLVMStructType = new LLVMStructType(fields.map(field => field.compile).toArray,true)

  override def toString: String = {
    val fieldStrings = fields.map(field => field.name + ": " field.tau.toString)
    "{" + fieldStrings.tail.foldLeft(fieldStrings.head)((rest: String,next: String) => rest + "," + next) + "}"
  }
}

object EmptyRecord extends RecordProduct(Nil)

class FunctionPointer(d: List[MonoType],r: MonoType,e: MonoEffect) extends MonoType {
  override val domain = d
  override val range = r
  val effect = e

  override def map(f: (MonoType) => MonoType): MonoType = {
    f(new FunctionPointer(domain.map(d => d.map(f)),range.map(f)))
  }
  override def map(f: (MonoEffect) => MonoEffect): FunctionPointer = new FunctionPointer(domain.map(d => d.map(f)),range.map(f),effect.map(f)))
  override def map(f: (MonoRegion) => MonoRegion): FunctionPointer = new FunctionPointer(domain.map(d => d.map(f)),range.map(f),effect.map(f))
  override def variables: Set[SignatureVariable[MonoType]] = domain.foldLeft(HashSet.empty)((result,tau) => result ++ tau.variables) ++ range.variables

  override def compile: LLVMFunctionType = {
    val compiledRange = range.compile
    val compiledDomain = domain.map(tau => tau.compile)
    new LLVMFunctionType(compiledRange,compiledDomain.toArray,false)
  }

  override def toString: String = "(" + domain.tail.foldLeft(domain.head.toString)((x: String,y: MonoType) => x + "," + y.toString) + ") @->" + range.toString
}

case class TaggedRecord(name: String,tag: Int,record: RecordType) {
  override def compileTag: LLVMConstantInteger = {
    val tagSize = math.floor(math.log(tag) / math.log(2)).toInt + 1
    assert(tagSize > 0)
    val tagType = new LLVMIntegerType(tagSize)
    LLVMConstantInteger.constantInteger(tagType,tag,false)
  }
}

class SumType(trs: List[Tuple2[String,RecordType]]) extends MonoType {
  val cases = trs.zipWithIndex.map(pair => TaggedRecord(pair._1._1,pair._2,pair._1._2))
  override def map(f: (MonoType) => MonoType): MonoType = f(new SumType(cases.map(tr => (tr.name,tr.record.map(f)))))
  override def map(f: (MonoEffect) => MonoEffect): SumType = new SumType(cases.map(tr => (tr.name,tr.record.map(f))))
  override def map(f: (MonoRegion) => MonoRegion): SumType = new SumType(cases.map(tr => (tr.name,tr.record.map(f))))
  override def variables: Set[SignatureVariable[MonoType]] = cases.foldLeft(HashSet.empty)((result,tr) => result ++ tr.record.variables)
  override def tagged: Boolean = !enumeration
  def enumeration: Boolean = addends.forall(trec => trec.record.length == 0)
  
  def tagRepresentation: LLVMIntegerType = {
    val tagSize = math.floor(math.log(cases.length-1) / math.log(2)).toInt + 1
    assert(tagSize > 0)
    new LLVMIntegerType(tagSize)
  }
  
  def minimalRecord: RecordType = {
    val smallest = cases.sortWith((x,y) => x.record.length <= y.record.length).head
    if(cases.forall(c => TypeOrdering.lteq(c.record,smallest)))
      smallest
    else
      EmptyRecord
  }
  
  def caseRepresentation(which: Int): LLVMType = {
    assert(which < cases.length)
    if(enumeration)
      tagRepresentation
    else
      new LLVMStructType(List(tagRepresentation,cases.apply(which).record.compile).toArray,true)
  }
  
  override def compile: LLVMType = {
    if(enumeration)
      tagRepresentation
    else {
      val maxRecord = cases.zipWithIndex.sortWith((x,y) => x._1.sizeOf >= y._1.sizeOf).head
      caseRepresentation(maxRecord._2)
    }
  }

  override def toString: String = {
    val sum = cases.tail.map(x => x.toString).foldLeft(cases.head.toString)((x: String,y: String) => x + " + " + y)
    "<" + sum + ">"
  }
}

case class ExistentialMethod(name: Option[String],domain: List[MonoType],range: MonoType)

class ExistentialObject(ms: List[ExistentialMethod],w: Option[MonoType]=None) extends MonoType {
  val methods = ms
  protected val witness = w
  val skolem = {
    val shape = new RecordType(RecordMember(None,TopType) :: methods.map(method => RecordMember(method.name,FunctionPointer(TopType :: method.domain,method.range))))
    val shapeVariables = shape.variables
    if(shapeVariables.forall(tvar => tvar.universal)) {
      val constructor = SkolemConstructors.get(shape)
      witness match {
        case Some(w) => {
          assert(w.variables.forall(tvar => shapeVariables.contains(tvar)))
          constructor.witness(w)
        }
        case None => { }
      }
      new TypeConstructorCall(constructor,shapeVariables)
    }
    else
      TopType
  }
  val shape = new RecordType(RecordMember(None,skolem) :: methods.map(method => RecordMember(method.name,FunctionPointer(skolem :: method.domain,method.range))))
  override def tagged: Boolean = false
  override def map(f: (MonoType) => MonoType): MonoType = f(new ExistentialObject(methods.map(method => ExistentialMethod(method.name,method.domain.map(tau => tau.map(f)),range.map(f))),witness.map(tau => tau.map(f))))
  override def map(f: (MonoRegion) => MonoRegion): MonoType = new ExistentialObject(methods.map(method => ExistentialMethod(method.name,method.domain.map(tau => tau.map(f)),range.map(f))),witness.map(tau => tau.map(f)))
    override def map(f: (MonoEffect) => MonoEffect): MonoType = new ExistentialObject(methods.map(method => ExistentialMethod(method.name,method.domain.map(tau => tau.map(f)),range.map(f))),witness.map(tau => tau.map(f)))f
  override def compile: LLVMType = shape.compile
}

class TypeVariable(univ: Boolean,n: Option[String] = None) extends SignatureVariable[MonoType] {
  override val universal = univ
  val name = n
  
  override def tagged: Boolean = false
  override def compile: LLVMType = throw new TypeException("Cannot compile type variable " + name.toString + ".")
  override def sizeOf: Int = 1
  override def toString: String = name match {
    case Some(str) => str
    case None => getClass().getName() + '@' + Integer.toHexString(hashCode())
  }
}

class BoundedTypeVariable(tau: MonoType,bnd: SignatureBound,univ: Boolean) extends BoundsVariable[MonoType](tau,bnd,univ) {
  override def tagged: Boolean = signature.tagged
  override def compile: LLVMType = signature.compile
  override def sizeOf: Int = signature.sizeOf
  override def toString: String = signature.toString
}

implicit object TypeBounds extends BoundsVariables[MonoType] {
  override def join(bound: BoundsVariable[MonoType],above: MonoType): Tuple2[InferenceConstraint[MonoType],BoundsVariable[MonoType]] = {
    val constraint = bound.bound match {
      case JoinBound => SubsumptionConstraint(bound.signature,above)
      case MeetBound => SubsumptionConstraint(above,bound.signature)
    }
    (constraint,new BoundedTypeVariable(above,JoinBound,bound.universal)
  }
  override def meet(bound: BoundsVariable[MonoType],below: MonoType): Tuple2[InferenceConstraint[MonoType],BoundsVariable[MonoType]] = bound.bound match {
    case JoinBound => (SubsumptionConstraint(bound.signature,below),this)
    case MeetBound => (SubsumptionConstraint(below,bound.signature),new BoundedTypeVariable(below,MeetBound,bound.universal))
  }
}

object TypeRelation extends InferenceOrdering[MonoType] {
  protected val assumptions = new Stack[InferenceConstraint[MonoType]]()

  override def lt(x: MonoType,y: MonoType,physical: Boolean = false): Option[Set[InferenceConstraint[MonoType]]] = (x,y) match {
    case (_,TopType) => Some(HashSet.empty)
    case (BottomType,_) => Some(HashSet.empty)
    case (UnitType,UnitType) => Some(HashSet.empty)
    case (TypeConstructorCall(cx,px),TypeConstructorCall(cy,py)) => lt(cx.represent(px),cy.represent(py),physical)
    case (TypeConstructorCall(cx,px),_) => lt(cx.represent(px),y,physical)
    case (_,TypeConstructorCall(cy,py)) => lt(x,cy.represent(py),physical)
    case (_,ry: RecursiveType) => lt(x,ry.innards,physical)
    case (rx: RecursiveType,_) => lt(rx.innards,y,physical)
    case (rx: RecursiveType,ry: RecursiveType) => {
      val unfoldx = rx.unfold
      val unfoldy = ry.unfold
      assumptions.push(SubsumptionConstraint(unfoldx._1,unfoldy._1))
      val result = lt(unfoldx._2,unfoldy._2,physical)
      assumptions.pop
      result
    }
    case (rx: RecordType,ry: RecordType) => {
      val width = if(rx.length >= ry.length) Some(HashSet.empty) else None
      val depths = rx.fields.zip(ry.fields).map(taus => if(physical) equiv(taus._1,taus._2) else lt(taus._1,taus._2,physical))
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (fx: FunctionPointer,fy: FunctionPointer) => {
      val width = if(fx.domain.length == fy.domain.length) Some(HashSet.empty) else None
      val depths = lt(fx.range,fy.range) :: fx.domain.zip(fy.domain).map(taus => lt(taus._2,taus._1,physical))
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (sx: SumType,sy: SumType) => {
      val width = if(sx.cases.length <= sy.cases.length) Some(HashSet.empty) else None
      val depths = sx.cases.zip(sy.cases).map(cs => if(cs._1.name == cs._2.name) { if(physical) equiv(cs._1.record,cs._2.record) else lt(cs._1.record,cs._2.record,physical) } else None)
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (ex: ExistentialObject,ey: ExistentialObject) => lt(ex.shape,ey.shape,physical)
    case (BoundsVariable(tx,_),BoundsVariable(ty,_)) => lt(tx,ty,physical)
    case (vx: TypeVariable,vy: TypeVariable) => {
      val empty = HashSet.empty
      val constraint = if(physical) PhysicalSubtypingConstraint(vx,vy) else SubsumptionConstraint(vx,vy)
      if(vx == vy || vx.name == vy.name && vy.universal || assumptions.contains(constraint) || assumptions.contains(EqualityConstraint(vx,vy)))
        Some(empty)
      else
        Some(empty.add(constraint))
    }
    case (vx: TypeVariable,_) => {
      val empty = HashSet.empty
      Some(empty.add(if(physical) PhysicalSubtypingConstraint(vx,vy) else SubsumptionConstraint(vx,vy)))
    }
    case (_,vy: TypeVariable) => {
      val empty = HashSet.empty
      if(vy.universal)
        Some(empty)
      else
        Some(if(physical) PhysicalSubtypingConstraint(vx,vy) else SubsumptionConstraint(vx,vy))
    }
    case (_,_) => {
      val constraint = if(physical) PhysicalSubtypingConstraint(vx,vy) else SubsumptionConstraint(vx,vy)
      assumptions.contains(constraint) || assumptions.contains(EqualityConstraint((x,y)))
    }
  }
  
  override def equiv(x: MonoType,y: MonoType): Option[Set[InferenceConstraint[MonoType]]] = (x,y) match {
    case (TopType,TopType) => Some(HashSet.empty)
    case (BottomType,BottomType) => Some(HashSet.empty)
    case (UnitType,UnitType) => Some(HashSet.empty)
    case (TypeConstructorCall(cx,px),TypeConstructorCall(cy,py)) => equiv(cx.represent(px),cy.represent(py))
    case (TypeConstructorCall(cx,px),_) => equiv(cx.represent(px),y)
    case (_,TypeConstructorCall(cy,py)) => equiv(x,cy.represent(py))
    case (_,ry: RecursiveType) => equiv(x,ry.innards)
    case (rx: RecursiveType,_) => equiv(rx.innards,y)
    case (rx: RecursiveType,ry: RecursiveType) => {
      val unfoldx = rx.unfold
      val unfoldy = ry.unfold
      assumptions.push(EqualityConstraint(unfoldx._1,unfoldy._1))
      val result = equiv(unfoldx._2,unfoldy._2)
      assumptions.pop
      result
    }
    case (rx: RecordType,ry: RecordType) => {
      val width = if(rx.length == ry.length) Some(HashSet.empty) else None
      val depths = rx.fields.zip(ry.fields).map(taus => equiv(taus._1,taus._2))
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (fx: FunctionPointer,fy: FunctionPointer) => {
      val width = if(fx.domain.length == fy.domain.length) Some(HashSet.empty) else None
      val depths = equiv(fx.range,fy.range) :: fx.domain.zip(fy.domain).map(taus => equiv(taus._1,taus._2))
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (sx: SumType,sy: SumType) => {
      val width = if(sx.cases.length == sy.cases.length) Some(HashSet.empty) else None
      val depths = sx.cases.zip(sy.cases).map(cs => if(cs._1.name == cs._2.name) equiv(cs._1.record,cs._2.record) else None)
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (ex: ExistentialObject,ey: ExistentialObject) => equiv(ex.shape,ey.shape)
    case (BoundsVariable(tx,_),BoundsVariable(ty,_)) => equiv(tx,ty)
    case (vx: TypeVariable,vy: TypeVariable) => {
      val empty = HashSet.empty
      if(vx == vy || vx.name == vy.name && vy.universal || assumptions.contains(EqualityConstraint((vx,vy))))
        Some(empty)
      else
        Some(empty.add(EqualityConstraint(vx,vy)))
    }
    case (vx: TypeVariable,_) => {
      val empty = HashSet.empty
      Some(empty.add(EqualityConstraint(vx,y)))
    }
    case (_,vy: TypeVariable) => {
      val empty = HashSet.empty
      if(vy.universal)
        Some(empty)
      else
        Some(empty.add(EqualityConstraint(x,vy)))
    }
    case (_,_) => assumptions.contains(EqualityConstraint((x,y)))
  }
}

object TypeOrdering extends PartialOrdering[MonoType] {
  override def lt(x: MonoType,y: MonoType): Boolean = TypeRelation.lt(x,y) match {
    case Some(constraints) => constraints.isEmpty
    case None => false
  }
  override def equiv(x: MonoType,y: MonoType): Boolean = TypeRelation.equiv(x,y) match {
    case Some(constraints) => constraints.isEmpty
    case None => false
  }
  override def gt(x: MonoType,y: MonoType): Boolean = lt(y,x)
  override def lteq(x: MonoType,y: MonoType): Boolean = equiv(x,y) || lt(x,y)
  override def gteq(x: MonoType,y: MonoType): Boolean = equiv(x,y) || gt(x,y)
  override def tryCompare(x: MonoType,y: MonoType): Option[Int] = {
    if(gt(x,y))
      Some(1)
    else if(lt(x,y))
      Some(-1)
    else if(equiv(x,y))
      Some(0)
    else
      None
  }
}
