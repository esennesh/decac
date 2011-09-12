package decac

import jllvm._
import scala.collection.mutable.Stack
import scala.collection.immutable.HashSet
import scala.collection.immutable.HashMap
import scala.collection.mutable.LatticeOrdering
import scala.collection.mutable.GraphLattice

object TopType extends MonoType {
  override def compile: LLVMType = throw TypeException("Top type indicates a type-inference error.")
  override def variables: Set[SignatureVariable] = HashSet.empty
  override def toString: String = "top"
}

object BottomType extends MonoType {
  override def compile: LLVMType = throw TypeException("Bottom type indicates a type-inference error.")
  override def variables: Set[SignatureVariable] = HashSet.empty
  override def toString: String = "bottom"
}

object UnitType extends MonoType {
  override def compile: LLVMType = new LLVMVoidType
  override def variables: Set[SignatureVariable] = HashSet.empty
  override def toString: String = "unit"
}

case class TypeConstructorCall(constructor: TypeConstructor,params: List[MonoType]) extends MonoType {
  override def compile: LLVMType = constructor.compile(params)
  override def toString: String = constructor.name + "<" + params.tail.foldLeft(params.head.toString)((s,p) => s + "," + p.toString) + ">"
  override def mapT(f: (MonoType) => MonoType): MonoType = f(new TypeConstructorCall(constructor,params.map(tau => tau.mapT(f))))
  override def mapE(f: (MonoEffect) => MonoEffect): TypeConstructorCall = new TypeConstructorCall(constructor,params.map(tau => tau.mapE(f)))
  override def mapR(f: (MonoRegion) => MonoRegion): TypeConstructorCall = new TypeConstructorCall(constructor,params.map(tau => tau.mapR(f)))
  override def variables: Set[SignatureVariable] = {
    params.foldLeft(HashSet.empty[SignatureVariable])((result,param) => result ++ param.variables)
  }
}

case class TypeException(error: String) extends Exception("Type error: " + error)

class OpaqueType extends MonoType {
  protected val compiled: LLVMOpaqueType = new LLVMOpaqueType
  override def compile: LLVMType = compiled
  override def toString: String = getClass().getName() + '@' + Integer.toHexString(hashCode())
  override def mapT(f: (MonoType) => MonoType): MonoType = f(this)
  override def mapE(f: (MonoEffect) => MonoEffect): MonoType = this
  override def mapR(f: (MonoRegion) => MonoRegion): MonoType = this
  override def variables: Set[SignatureVariable] = HashSet.empty
}

class RecursiveType(tau: MonoType,loopNode: Option[MonoType]) extends MonoType {
  val innards: MonoType = loopNode match {
    case None => tau
    case Some(alpha) => tau.mapT((sig: MonoType) => if(sig == alpha) this else sig)
  }
  
  def unfold: Tuple2[OpaqueType,MonoType] = {
    val loop = new OpaqueType
    (loop,innards.mapT((tau: MonoType) => if(tau == this) loop else tau))
  }
  
  override def mapT(f: (MonoType) => MonoType): MonoType = f(new RecursiveType(innards.mapT((tau: MonoType) => if(tau == this) tau else f(tau)),Some(this)))
  override def mapR(f: (MonoRegion) => MonoRegion): RecursiveType = {
    val unfolded = unfold
    new RecursiveType(unfolded._2.mapR(f),Some(unfolded._1))
  }
  override def mapE(f: (MonoEffect) => MonoEffect): RecursiveType = {
    val unfolded = unfold
    new RecursiveType(unfolded._2.mapE(f),Some(unfolded._1))
  }
  
  override def variables: Set[SignatureVariable] = unfold._2.variables
  
  override def compile: LLVMType = {
    val opaque = new OpaqueType
    val bodyType = innards.mapT((tau: MonoType) => if(tau == this) opaque else tau).compile
    val bodyHandle = new LLVMTypeHandle(bodyType)
    LLVMTypeHandle.refineType(opaque.compile,bodyType)
    bodyHandle.resolve
  }
  
  override def toString: String = {
    val alpha = new TypeVariable(false,None)
    "mu " + alpha.toString + "." + innards.mapT((tau: MonoType) => if(tau == this) alpha else tau).toString
  }
}

case class RecordMember(name: Option[String],tau: MonoType)

class RecordType(f: List[RecordMember]) extends MonoType {
  val fields: List[RecordMember] = f
  val length: Int = fields.length
  
  override def mapT(f: (MonoType) => MonoType): MonoType = {
    f(new RecordType(fields.map(field => RecordMember(field.name,field.tau.mapT(f)))))
  }
  override def mapR(f: (MonoRegion) => MonoRegion): RecordType = {
    new RecordType(fields.map(field => RecordMember(field.name,field.tau.mapR(f))))
  }
  override def mapE(f: (MonoEffect) => MonoEffect): RecordType = {
    new RecordType(fields.map(field => RecordMember(field.name,field.tau.mapE(f))))
  }
  override def variables: Set[SignatureVariable] = fields.foldLeft(HashSet.empty[SignatureVariable])((result,field) => result ++ field.tau.variables)

  override def compile: LLVMStructType = new LLVMStructType(fields.map(field => field.tau.compile).toArray,true)

  override def toString: String = {
    val fieldStrings = fields.map(field => field.name + ": " + field.tau.toString)
    "{" + fieldStrings.tail.foldLeft(fieldStrings.head)((rest: String,next: String) => rest + "," + next) + "}"
  }
}

object EmptyRecord extends RecordType(Nil)

class FunctionPointer(d: List[MonoType],r: MonoType,e: MonoEffect) extends MonoType {
  val domain = d
  val range = r
  val effect = e

  override def mapT(f: (MonoType) => MonoType): MonoType = {
    f(new FunctionPointer(domain.map(d => d.mapT(f)),range.mapT(f),effect))
  }
  override def mapE(f: (MonoEffect) => MonoEffect): FunctionPointer = new FunctionPointer(domain.map(d => d.mapE(f)),range.mapE(f),effect.mapE(f))
  override def mapR(f: (MonoRegion) => MonoRegion): FunctionPointer = new FunctionPointer(domain.map(d => d.mapR(f)),range.mapR(f),effect.mapR(f))
  override def variables: Set[SignatureVariable] = domain.foldLeft(HashSet.empty[SignatureVariable])((result,tau) => result ++ tau.variables) ++ range.variables ++ effect.variables

  override def compile: LLVMFunctionType = {
    val compiledRange = range.compile
    val compiledDomain = domain.map(tau => tau.compile)
    new LLVMFunctionType(compiledRange,compiledDomain.toArray,false)
  }

  override def toString: String = "(" + domain.tail.foldLeft(domain.head.toString)((x: String,y: MonoType) => x + "," + y.toString) + ") @->" + range.toString
}

case class TaggedRecord(name: String,tag: Int,record: RecordType) {
  def compileTag: LLVMConstantInteger = {
    val tagSize = math.floor(math.log(tag) / math.log(2)).toInt + 1
    assert(tagSize > 0)
    val tagType = new LLVMIntegerType(tagSize)
    LLVMConstantInteger.constantInteger(tagType,tag,false)
  }
}

class ZippableMap[A,B](val m: Map[A,B]) {
  def zipElements[C](that: Map[A,C]): Map[A,Tuple2[B,C]] = {
    val xys = m.map(xs => (xs._1,xs._2,that.get(xs._1)))
    val intersection = xys.foldRight(Nil : List[(A,(B,C))])((head: (A,B,Option[C]),rest: List[(A,(B,C))]) => head._3 match {
      case Some(c) => (head._1,(head._2,c)) :: rest
      case None => rest
    })
    HashMap.empty[A,Tuple2[B,C]] ++ intersection
  }
}

class SumType(trs: List[Tuple2[String,RecordType]]) extends MonoType {
  val cases: Map[String,TaggedRecord] = HashMap.empty[String,TaggedRecord] ++ trs.zipWithIndex.map(pair => (pair._1._1,TaggedRecord(pair._1._1,pair._2,pair._1._2)))
  implicit def zippingMap[A,B](m: Map[A,B]): ZippableMap[A,B] = new ZippableMap[A,B](m)
  
  override def mapT(f: (MonoType) => MonoType): MonoType = f(new SumType(cases.toList.map(tr => (tr._1,tr._2.record.mapT(f).asInstanceOf[RecordType]))))
  override def mapE(f: (MonoEffect) => MonoEffect): SumType = new SumType(cases.toList.map(tr => (tr._1,tr._2.record.mapE(f))))
  override def mapR(f: (MonoRegion) => MonoRegion): SumType = new SumType(cases.toList.map(tr => (tr._1,tr._2.record.mapR(f))))
  override def variables: Set[SignatureVariable] = {
    val empty: Set[SignatureVariable] = HashSet.empty
    cases.foldLeft(empty)((result,tr) => result ++ tr._2.record.variables)
  }
  def enumeration: Boolean = cases.forall(trec => trec._2.record.length == 0)
  
  def tagRepresentation: LLVMIntegerType = {
    val tagSize = math.floor(math.log(cases.size-1) / math.log(2)).toInt + 1
    assert(tagSize > 0)
    new LLVMIntegerType(tagSize)
  }
  
  def minimalRecord: RecordType = {
    val smallest = cases.toList.sortWith((x,y) => x._2.record.length <= y._2.record.length).head
    if(cases.forall(c => TypeOrdering.lteq(c._2.record,smallest._2.record)))
      smallest._2.record
    else
      EmptyRecord
  }
  
  def caseRepresentation(which: Int): LLVMType = {
    assert(which < cases.size)
    if(enumeration)
      tagRepresentation
    else
      new LLVMStructType(List(tagRepresentation,cases.toList.apply(which)._2.record.compile).toArray,true)
  }
  
  override def compile: LLVMType = {
    if(enumeration)
      tagRepresentation
    else {
      val maxRecord = cases.toList.zipWithIndex.sortWith((x,y) => x._1._2.record.sizeOf >= y._1._2.record.sizeOf).head
      caseRepresentation(maxRecord._2)
    }
  }

  override def toString: String = {
    val sum = cases.tail.map(x => x.toString).foldLeft(cases.head.toString)((x: String,y: String) => x + " + " + y)
    "<" + sum + ">"
  }
}

case class ExistentialMethod(name: Option[String],domain: List[MonoType],range: MonoType,effect: MonoEffect) {
  def ==(em: ExistentialMethod): Boolean = {
    name == em.name && (domain zip em.domain).forall(ds => TypeOrdering.equiv(ds._1,ds._2)) && TypeOrdering.equiv(range,em.range) && EffectOrdering.equiv(effect,em.effect)
  }
}

class ExistentialObject(ms: List[ExistentialMethod],w: Option[MonoType]=None) extends MonoType {
  val methods = ms
  protected val witness = w
  val skolem = {
    val shape = new RecordType(RecordMember(None,TopType) :: methods.map(method => RecordMember(method.name,new FunctionPointer(TopType :: method.domain,method.range,method.effect))))
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
      new TypeConstructorCall(constructor,shapeVariables.map(v => v.asInstanceOf[MonoType]).toList)
    }
    else
      TopType
  }
  val shape = new RecordType(RecordMember(None,skolem) :: methods.map(method => RecordMember(method.name,new FunctionPointer(skolem :: method.domain,method.range,method.effect))))
  override def mapT(f: (MonoType) => MonoType): MonoType = {
    val mappedMethods = methods.map(method => ExistentialMethod(method.name,method.domain.map(tau => tau.mapT(f)),method.range.mapT(f),method.effect.mapT(f)))
    f(new ExistentialObject(mappedMethods,witness.map(tau => tau.mapT(f))))
  }
  override def mapR(f: (MonoRegion) => MonoRegion): MonoType = {
    val mappedMethods = methods.map(method => ExistentialMethod(method.name,method.domain.map(tau => tau.mapR(f)),method.range.mapR(f),method.effect.mapR(f)))
    new ExistentialObject(mappedMethods,witness.map(tau => tau.mapR(f)))
  }
  override def mapE(f: (MonoEffect) => MonoEffect): MonoType = {
    val mappedMethods = methods.map(method => ExistentialMethod(method.name,method.domain.map(tau => tau.mapE(f)),method.range.mapE(f),method.effect.mapE(f)))
    new ExistentialObject(mappedMethods,witness.map(tau => tau.mapE(f)))
  }
  override def compile: LLVMType = shape.compile
  override def variables: Set[SignatureVariable] = shape.variables
}

class TypeVariable(univ: Boolean,n: Option[String] = None) extends MonoType with SignatureVariable {
  override val universal = univ
  val name = n
  
  override def compile: LLVMType = throw new TypeException("Cannot compile type variable " + name.toString + ".")
  override def sizeOf: Int = 1
  override def toString: String = name match {
    case Some(str) => str
    case None => getClass().getName() + '@' + Integer.toHexString(hashCode())
  }
}

class BoundedTypeVariable(tau: MonoType,bnd: SignatureBound,univ: Boolean) extends MonoType with BoundsVariable[MonoType] {
  override val signature = tau
  override val bound = bnd
  override val universal = univ
  override def compile: LLVMType = signature.compile
  override def sizeOf: Int = signature.sizeOf
  override def toString: String = signature.toString
  
  def join(above: MonoType): Tuple2[InferenceConstraint,BoundsVariable[MonoType]] = {
    val constraint = bound match {
      case JoinBound => SubsumptionConstraint(signature,above)
      case MeetBound => SubsumptionConstraint(above,signature)
    }
    (constraint,new BoundedTypeVariable(above,JoinBound,universal))
  }
  def meet(below: MonoType): Tuple2[InferenceConstraint,BoundsVariable[MonoType]] = bound match {
    case JoinBound => (SubsumptionConstraint(signature,below),this)
    case MeetBound => (SubsumptionConstraint(below,signature),new BoundedTypeVariable(below,MeetBound,universal))
  }
}

object TypeRelation extends InferenceOrdering[MonoType] {
  protected val assumptions = new Stack[InferenceConstraint]()
  
  override protected val lattice = new GraphLattice(BottomType,TopType)(TypeOrdering)

  override def lt(x: MonoType,y: MonoType): Option[Set[InferenceConstraint]] = (x,y) match {
    case (_,TopType) => Some(HashSet.empty)
    case (BottomType,_) => Some(HashSet.empty)
    case (UnitType,UnitType) => Some(HashSet.empty)
    case (TypeConstructorCall(cx,px),TypeConstructorCall(cy,py)) => lt(cx.represent(px),cy.represent(py))
    case (TypeConstructorCall(cx,px),_) => lt(cx.represent(px),y)
    case (_,TypeConstructorCall(cy,py)) => lt(x,cy.represent(py))
    case (rx: RecursiveType,ry: RecursiveType) => {
      val unfoldx = rx.unfold
      val unfoldy = ry.unfold
      assumptions.push(SubsumptionConstraint(unfoldx._1,unfoldy._1))
      val result = lt(unfoldx._2,unfoldy._2)
      assumptions.pop
      result
    }
    case (_,ry: RecursiveType) => lt(x,ry.innards)
    case (rx: RecursiveType,_) => lt(rx.innards,y)
    case (rx: RecordType,ry: RecordType) => {
      val width = if(rx.length >= ry.length) Some(HashSet.empty[InferenceConstraint]) else None
      val depths = rx.fields.zip(ry.fields).map(taus => lt(taus._1.tau,taus._2.tau))
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (fx: FunctionPointer,fy: FunctionPointer) => {
      val width = if(fx.domain.length == fy.domain.length) Some(HashSet.empty[InferenceConstraint]) else None
      val depths = lt(fx.range,fy.range) :: fx.domain.zip(fy.domain).map(taus => lt(taus._2,taus._1))
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (sx: SumType,sy: SumType) => {
      val width = if(sx.cases.size <= sy.cases.size) Some(HashSet.empty[InferenceConstraint]) else None
      val depths = sx.cases.zip(sy.cases).map(cs => if(cs._1._1 == cs._2._1) lt(cs._1._2.record,cs._2._2.record) else None)
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (ex: ExistentialObject,ey: ExistentialObject) => lt(ex.shape,ey.shape)
    case (bx: BoundedTypeVariable,by: BoundedTypeVariable) => lt(bx.signature,by.signature)
    case (vx: TypeVariable,vy: TypeVariable) => {
      val empty = HashSet.empty[InferenceConstraint]
      val constraint: InferenceConstraint = SubsumptionConstraint(vx,vy)
      if(vx == vy || vx.name == vy.name && vy.universal || assumptions.contains(constraint) || assumptions.contains(EqualityConstraint(vx,vy)))
        Some(empty)
      else
        Some(empty + constraint)
    }
    case (vx: TypeVariable,_) => {
      val empty = HashSet.empty[InferenceConstraint]
      Some(empty + SubsumptionConstraint(vx,y))
    }
    case (_,vy: TypeVariable) => {
      val empty = HashSet.empty[InferenceConstraint]
      if(vy.universal)
        Some(empty)
      else
        Some(empty + SubsumptionConstraint(x,vy))
    }
    case (_,_) => {
      val constraint = SubsumptionConstraint(x,y)
      val empty = HashSet.empty[InferenceConstraint]
      if(assumptions.contains(constraint) || assumptions.contains(EqualityConstraint(x,y)))
        Some(empty)
      else
        Some(HashSet(constraint))
    }
  }
  override def equiv(x: MonoType,y: MonoType): Option[Set[InferenceConstraint]] = (x,y) match {
    case (TopType,TopType) => Some(HashSet.empty)
    case (BottomType,BottomType) => Some(HashSet.empty)
    case (UnitType,UnitType) => Some(HashSet.empty)
    case (TypeConstructorCall(cx,px),TypeConstructorCall(cy,py)) => equiv(cx.represent(px),cy.represent(py))
    case (TypeConstructorCall(cx,px),_) => equiv(cx.represent(px),y)
    case (_,TypeConstructorCall(cy,py)) => equiv(x,cy.represent(py))
    case (rx: RecursiveType,ry: RecursiveType) => {
      val unfoldx = rx.unfold
      val unfoldy = ry.unfold
      assumptions.push(EqualityConstraint(unfoldx._1,unfoldy._1))
      val result = equiv(unfoldx._2,unfoldy._2)
      assumptions.pop
      result
    }
    case (_,ry: RecursiveType) => equiv(x,ry.innards)
    case (rx: RecursiveType,_) => equiv(rx.innards,y)
    case (rx: RecordType,ry: RecordType) => {
      val width = if(rx.length == ry.length) Some(HashSet.empty[InferenceConstraint]) else None
      val depths = rx.fields.zip(ry.fields).map(taus => equiv(taus._1.tau,taus._2.tau))
      depths.foldLeft(width)((res,depth: Option[Set[InferenceConstraint]]) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (fx: FunctionPointer,fy: FunctionPointer) => {
      val width = if(fx.domain.length == fy.domain.length) Some(HashSet.empty[InferenceConstraint]) else None
      val depths = equiv(fx.range,fy.range) :: fx.domain.zip(fy.domain).map(taus => equiv(taus._1,taus._2))
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (sx: SumType,sy: SumType) => {
      val width = if(sx.cases.size == sy.cases.size) Some(HashSet.empty[InferenceConstraint]) else None
      val depths = sx.cases.zip(sy.cases).map(cs => if(cs._1._1 == cs._2._1) equiv(cs._1._2.record,cs._2._2.record) else None)
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (ex: ExistentialObject,ey: ExistentialObject) => equiv(ex.shape,ey.shape)
    case (bx: BoundedTypeVariable,by: BoundedTypeVariable) => equiv(bx.signature,bx.signature)
    case (vx: TypeVariable,vy: TypeVariable) => {
      val empty = HashSet.empty[InferenceConstraint]
      if(vx == vy || vx.name == vy.name && vy.universal || assumptions.contains(EqualityConstraint(vx,vy)))
        Some(empty)
      else
        Some(empty + EqualityConstraint(vx,vy))
    }
    case (vx: TypeVariable,_) => {
      val empty = HashSet.empty[InferenceConstraint]
      Some(empty + EqualityConstraint(vx,y))
    }
    case (_,vy: TypeVariable) => {
      val empty = HashSet.empty[InferenceConstraint]
      if(vy.universal)
        Some(empty)
      else
        Some(empty + EqualityConstraint(x,vy))
    }
    case (_,_) => {
      val empty = HashSet.empty[InferenceConstraint]
      if(assumptions.contains(EqualityConstraint(x,y)))
        Some(empty)
      else
        None
    }
  }
}

object PhysicalTypeRelation extends InferenceOrdering[MonoType] {
  protected val assumptions = new Stack[InferenceConstraint]()
  
  override protected val lattice = new GraphLattice(BottomType,TopType)(PhysicalTypeOrdering)

  override def lt(x: MonoType,y: MonoType): Option[Set[InferenceConstraint]] = (x,y) match {
    case (_,TopType) => Some(HashSet.empty)
    case (BottomType,_) => Some(HashSet.empty)
    case (UnitType,UnitType) => Some(HashSet.empty)
    case (TypeConstructorCall(cx,px),TypeConstructorCall(cy,py)) => lt(cx.represent(px),cy.represent(py))
    case (TypeConstructorCall(cx,px),_) => lt(cx.represent(px),y)
    case (_,TypeConstructorCall(cy,py)) => lt(x,cy.represent(py))
    case (rx: RecursiveType,ry: RecursiveType) => {
      val unfoldx = rx.unfold
      val unfoldy = ry.unfold
      assumptions.push(SubsumptionConstraint(unfoldx._1,unfoldy._1))
      val result = lt(unfoldx._2,unfoldy._2)
      assumptions.pop
      result
    }
    case (_,ry: RecursiveType) => lt(x,ry.innards)
    case (rx: RecursiveType,_) => lt(rx.innards,y)
    case (rx: RecordType,ry: RecordType) => {
      val width = if(rx.length >= ry.length) Some(HashSet.empty[InferenceConstraint]) else None
      val depths = rx.fields.zip(ry.fields).map(taus => equiv(taus._1.tau,taus._2.tau))
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (fx: FunctionPointer,fy: FunctionPointer) => {
      val width = if(fx.domain.length == fy.domain.length) Some(HashSet.empty[InferenceConstraint]) else None
      val depths = lt(fx.range,fy.range) :: fx.domain.zip(fy.domain).map(taus => lt(taus._2,taus._1))
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (sx: SumType,sy: SumType) => {
      val width = if(sx.cases.size <= sy.cases.size) Some(HashSet.empty[InferenceConstraint]) else None
      val depths = sx.cases.zip(sy.cases).map(cs => if(cs._1._1 == cs._2._1) equiv(cs._1._2.record,cs._2._2.record) else None)
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (ex: ExistentialObject,ey: ExistentialObject) => lt(ex.shape,ey.shape)
    case (bx: BoundedTypeVariable,by: BoundedTypeVariable) => lt(bx.signature,by.signature)
    case (vx: TypeVariable,vy: TypeVariable) => {
      val empty = HashSet.empty[InferenceConstraint]
      val constraint: InferenceConstraint = PhysicalSubtypingConstraint(vx,vy)
      if(vx == vy || vx.name == vy.name && vy.universal || assumptions.contains(constraint) || assumptions.contains(EqualityConstraint(vx,vy)))
        Some(empty)
      else
        Some(empty + constraint)
    }
    case (vx: TypeVariable,_) => {
      val empty = HashSet.empty[InferenceConstraint]
      Some(empty + PhysicalSubtypingConstraint(vx,y))
    }
    case (_,vy: TypeVariable) => {
      val empty = HashSet.empty[InferenceConstraint]
      if(vy.universal)
        Some(empty)
      else
        Some(empty + PhysicalSubtypingConstraint(x,vy))
    }
    case (_,_) => {
      val constraint = PhysicalSubtypingConstraint(x,y)
      val empty = HashSet.empty[InferenceConstraint]
      if(assumptions.contains(constraint) || assumptions.contains(EqualityConstraint(x,y)))
        Some(empty)
      else
        Some(HashSet(constraint))
    }
  }
  override def equiv(x: MonoType,y: MonoType): Option[Set[InferenceConstraint]] = TypeRelation.equiv(x,y)
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

object PhysicalTypeOrdering extends PartialOrdering[MonoType] {
  override def lt(x: MonoType,y: MonoType): Boolean = PhysicalTypeRelation.lt(x,y) match {
    case Some(constraints) => constraints.isEmpty
    case None => false
  }
  override def equiv(x: MonoType,y: MonoType): Boolean = PhysicalTypeRelation.equiv(x,y) match {
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
