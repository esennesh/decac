package org.deca.compiler.signature

import org.jllvm._
import scala.collection.mutable.Stack
import scala.collection.immutable.{Set,Map,HashSet,HashMap}
import scala.collection.mutable.LatticeOrdering
import scala.collection.mutable.GraphLattice

import org.deca.compiler.definition._

object TopType extends MonoType {
  override def compile: LLVMType = throw TypeException("Top type indicates a type-inference error.")
  override def variables: Set[SignatureVariable] = HashSet.empty
  override def toString: String = "top"
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = HashSet.empty
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = HashSet.empty
}

object BottomType extends MonoType {
  override def compile: LLVMType = throw TypeException("Bottom type indicates a type-inference error.")
  override def variables: Set[SignatureVariable] = HashSet.empty
  override def toString: String = "bottom"
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = HashSet.empty
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = HashSet.empty
}

object UnitType extends MonoType {
  override def compile: LLVMType = new LLVMVoidType
  override def variables: Set[SignatureVariable] = HashSet.empty
  override def toString: String = "unit"
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = HashSet.empty
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = HashSet.empty
}

case class TypeException(error: String) extends Exception("Type error: " + error)

class OpaqueType(context: LLVMContext = LLVMContext.getGlobalContext) extends MonoType {
  override def toString: String = getClass().getName() + '@' + Integer.toHexString(hashCode())
  override def mapT(f: (MonoType) => MonoType): MonoType = f(this)
  override def mapE(f: (MonoEffect) => MonoEffect): MonoType = this
  override def mapR(f: (MonoRegion) => MonoRegion): MonoType = this
  override def variables: Set[SignatureVariable] = HashSet.empty
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = HashSet.empty
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = HashSet.empty
  
  override val compile: LLVMIdentifiedStructType = new LLVMIdentifiedStructType(context)
  def resolve(body: List[MonoType]): LLVMIdentifiedStructType = {
    compile.setBody(body.map(_.compile).toArray,true)
    compile
  }
}

case class RecordMember(name: Option[String],mutable: MonoMutability,tau: MonoType)

class RecordType(val fields: List[RecordMember]) extends MonoType {
  val length: Int = fields.length
  
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = {
    fields.map(field => field.tau.filterT(pred)).foldLeft(HashSet.empty[MonoType].asInstanceOf[Set[MonoType]])((res: Set[MonoType],ts: Set[MonoType]) => res ++ ts) ++ (if(pred(this)) Set.empty + this else Set.empty)
  }
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = {
    fields.map(field => field.tau.filterR(pred)).foldLeft(HashSet.empty[MonoRegion].asInstanceOf[Set[MonoRegion]])((res: Set[MonoRegion],ts: Set[MonoRegion]) => res ++ ts)
  }
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = {
    fields.map(field => field.tau.filterE(pred)).foldLeft(HashSet.empty[MonoEffect].asInstanceOf[Set[MonoEffect]])((res: Set[MonoEffect],ts: Set[MonoEffect]) => res ++ ts)
  }
  
  override def mapT(f: (MonoType) => MonoType): MonoType = {
    f(new RecordType(fields.map(field => RecordMember(field.name,field.mutable,field.tau.mapT(f)))))
  }
  override def mapR(f: (MonoRegion) => MonoRegion): RecordType = {
    new RecordType(fields.map(field => RecordMember(field.name,field.mutable,field.tau.mapR(f))))
  }
  override def mapE(f: (MonoEffect) => MonoEffect): RecordType = {
    new RecordType(fields.map(field => RecordMember(field.name,field.mutable,field.tau.mapE(f))))
  }
  override def variables: Set[SignatureVariable] = fields.foldLeft(Set.empty[SignatureVariable])((result,field) => result ++ field.tau.variables)

  override def compile: LLVMStructType = new LLVMStructType(fields.map(field => field.tau.compile).toArray,true)

  override def toString: String = {
    val fieldStrings = fields.map(field => field.name + ": " + field.tau.toString)
    "{" + fieldStrings.foldRight("")((field: String,result: String) => field + (if(result != "") "," else "") + result) + "}"
  }
}

object EmptyRecord extends RecordType(Nil)

class FunctionPointer(val domain: List[MonoType],
                      val range: MonoType,
                      val positive: MonoEffect,
                      val negative: MonoEffect) extends MonoType {
  def pure: Boolean = !EffectOrdering.equiv(positive,PureEffect) || !EffectOrdering.equiv(negative,PureEffect)
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = {
    val domains = domain.map(d => d.filterT(pred)).foldLeft(Set.empty[MonoType])((res,typ) => res ++ typ)
    domains ++ range.filterT(pred) ++ positive.filterT(pred) ++ negative.filterT(pred) ++ (if(pred(this)) Set.empty + this else Set.empty)
  }
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = {
    val domains = domain.map(d => d.filterR(pred)).foldLeft(Set.empty[MonoRegion])((res,reg) => res ++ reg)
    domains ++ range.filterR(pred) ++ positive.filterR(pred) ++ negative.filterR(pred)
  }
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = {
    val domains = domain.map(d => d.filterE(pred)).foldLeft(Set.empty[MonoEffect])((res,eff) => res ++ eff)
    domains ++ range.filterE(pred) ++ positive.filterE(pred) ++ negative.filterE(pred)
  }
  override def mapT(f: (MonoType) => MonoType): MonoType =
    f(new FunctionPointer(domain.map(d => d.mapT(f)),range.mapT(f),positive,negative))
  override def mapE(f: (MonoEffect) => MonoEffect): FunctionPointer =
    new FunctionPointer(domain.map(d => d.mapE(f)),range.mapE(f),positive.mapE(f),negative.mapE(f))
  override def mapR(f: (MonoRegion) => MonoRegion): FunctionPointer =
    new FunctionPointer(domain.map(d => d.mapR(f)),range.mapR(f),negative.mapR(f),negative.mapR(f))
  override def variables: Set[SignatureVariable] =
    domain.foldLeft[Set[SignatureVariable]](Set.empty)(_ ++ _.variables) ++ range.variables ++ positive.variables ++ negative.variables

  override def compile: LLVMFunctionType = {
    val compiledRange = range.compile
    val compiledDomain = domain.map(tau => tau.compile)
    new LLVMFunctionType(compiledRange,compiledDomain.toArray,false)
  }

  override def toString: String = {
    val parameters = "(" + domain.foldLeft("")((x: String,y: MonoType) => x + (if(x != "") "," else "") + y.toString) + ") @->"
    val effect = if(pure)
      "!{+(" + positive.toString + "),-(" + negative.toString + ")}"
    else
      ""
    parameters + effect + " " + range.toString
  }
}

object MapZipping {
  def zipMaps[A,B,C](x: Map[A,B],y: Map[A,C]): Map[A,(B,C)] = {
    val xys = x.map(xs => (xs._1,xs._2,y.get(xs._1)))
    val intersection = xys.foldRight(Nil : List[(A,(B,C))])((head: (A,B,Option[C]),rest: List[(A,(B,C))]) => head._3 match {
      case Some(c) => (head._1,(head._2,c)) :: rest
      case None => rest
    })
    HashMap.empty[A,(B,C)] ++ intersection
  }
}

class ClassBrand(val name: String,r: RecordType,ms: Map[String,FunctionPointer],val parent: Option[ClassBrand],loopNode: Option[MonoType] = None) {
  val record = loopNode match {
    case None => r
    case Some(mu) => r.mapT(tau => if(tau == mu) new BrandType(this,EmptyRecord) else tau).asInstanceOf[RecordType]
  }
  val methods: Map[String,FunctionPointer] = loopNode match {
    case None => ms
    case Some(mu) => ms.map(m => (m._1,m._2.mapT(tau => if(tau == mu) new BrandType(this,EmptyRecord) else tau).asInstanceOf[FunctionPointer]))
  }
  parent match {
    case Some(parentBrand) => parentBrand.extend(this)
  }
  protected var subBrands: Map[String,ClassBrand] = Map(name -> this)
  protected var sealingTags: Option[Map[ClassBrand,Int]] = None
  def extend(child: ClassBrand): Unit = {
    assert(!isSealed)
    val methodsSafelyExtend = methods.forall(m => child.methods.get(m._1) match {
      case Some(fp) => TypeOrdering.equiv(fp,m._2)
      case None => false
    })
    assert(TypeOrdering.lteq(child.record,record) && methodsSafelyExtend)
    subBrands = subBrands + ((child.name,child))
    parent.map(_.extend(child))
  }
  def containedBrands: Map[String,ClassBrand] = subBrands
  
  def seal(tags: Map[String,Int]): Unit =
    sealingTags = Some(tags.map(pair => (containedBrands(pair._1),pair._2)))
  def isSealed: Boolean = sealingTags != None
  
  def enumeration: Boolean = isSealed && subBrands.forall(brand => brand._2.record.fields == Nil)
  
  def tagRepresentation: MonoType =
    if(enumeration) {
      val tagSize = math.floor(math.log(subBrands.size) / math.log(2)).toInt + 1
      assert(tagSize > 0 && tagSize <= 32)
      if(tagSize <= 8)
        Byte
      else if(tagSize <= 16)
        SNat
      else if(tagSize <= 32)
        Nat
      else
        LongNat
    }
    else
      new PointerType(new BrandType(this,EmptyRecord),GlobalRegion,ReadOnlyMutability)
  def sizeOf: Int = tagRepresentation.sizeOf + record.sizeOf
  
  def mapT(f: (MonoType) => MonoType): ClassBrand = new ClassBrand(name,record.mapT(f).asInstanceOf[RecordType],methods.map(m => (m._1,m._2.mapT(f).asInstanceOf[FunctionPointer])),parent.map(_.mapT(f)),Some(new BrandType(this,EmptyRecord)))
  def mapE(f: (MonoEffect) => MonoEffect): ClassBrand = new ClassBrand(name,record.mapE(f),methods.map(m => (m._1,m._2.mapE(f))),parent.map(_.mapE(f)),Some(new BrandType(this,EmptyRecord)))
  def mapR(f: (MonoRegion) => MonoRegion): ClassBrand = new ClassBrand(name,record.mapR(f),methods.map(m => (m._1,m._2.mapR(f))),parent.map(_.mapR(f)),Some(new BrandType(this,EmptyRecord)))
}

object ExceptionBrand extends ClassBrand("Exception",EmptyRecord,Map.empty,None) {
  new TypeDefinition(new TypeExpressionConstructor(Nil,new BrandType(this,EmptyRecord)),"Exception",StandardLibrary)
}

class BrandType(val brand: ClassBrand,val extension: RecordType) extends MonoType {
  def enumeration: Boolean = brand.enumeration && extension.fields == Nil
  def represent: List[MonoType] = {
    val tag: MonoType = brand.tagRepresentation
    if(enumeration)
      List(tag)
    else {
      val largestChild: ClassBrand = brand.containedBrands.values.toList.sort((x,y) => x.sizeOf >= y.sizeOf).head
      tag :: largestChild.record.fields.map(_.tau) ++ extension.fields.map(_.tau)
    }
  }
  def resolve: Unit = compile.setBody(represent.map(_.compile).toArray,true)
  val compile: LLVMIdentifiedStructType = (new OpaqueType).compile
  override def sizeOf: Int = represent.foldLeft(0)((total: Int,tau: MonoType) => total + tau.sizeOf)
  override def toString: String = "class " + brand.name + extension.toString
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = brand.record.filterR(pred) ++ extension.filterR(pred)
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = brand.record.filterE(pred) ++ extension.filterE(pred)
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = brand.record.filterT(pred) ++ extension.filterT(pred)
  override def mapT(f: (MonoType) => MonoType): MonoType = f(new BrandType(brand.mapT(f),extension.mapT(f).asInstanceOf[RecordType]))
  override def mapE(f: (MonoEffect) => MonoEffect): BrandType = new BrandType(brand.mapE(f),extension.mapE(f))
  override def mapR(f: (MonoRegion) => MonoRegion): BrandType = new BrandType(brand.mapR(f),extension.mapR(f))
}

class TypeVariable(override val universal: Boolean,override val name: Option[String] = None) extends MonoType with SignatureVariable {
  override def compile: LLVMType = throw new TypeException("Cannot compile type variable " + name.toString + ".")
  override def sizeOf: Int = 1
  override def toString: String = name.getOrElse(getClass().getName()) + ':' + universal.toString + '@' + Integer.toHexString(hashCode)
  
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = Set.empty
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = Set.empty
}

class BoundedTypeVariable(tau: MonoType,bnd: SignatureBound,univ: Boolean,override val name: Option[String] = None) extends BoundsVariable[MonoType](tau,bnd,univ) with MonoType {
  override def clone(sig: MonoType,bnd: SignatureBound,univ: Boolean,nm: Option[String]) = new BoundedTypeVariable(sig,bnd,univ,nm orElse name)
  override def compile: LLVMType = signature.compile
  override def sizeOf: Int = signature.sizeOf
  override def filterT(pred: MonoType => Boolean) = if(pred(this)) signature.filterT(pred) + this else signature.filterT(pred)
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = signature.filterR(pred)
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = signature.filterE(pred)
  
  override def toString: String = name match {
    case Some(str) => {
      val suffix = "(" + str + "," + signature.toString + "," + universal.toString + ")"
      bound match {
        case JoinBound => "Join" + suffix
        case MeetBound => "Meet" + suffix
      }
    }
    case None => super.toString
  }
}

object TypeRelation extends InferenceOrdering[MonoType] {
  protected val assumptions = new Stack[InferenceConstraint]()
  
  override protected val lattice = new GraphLattice(TopType,BottomType)(TypeOrdering)

  override def lt(x: MonoType,y: MonoType): Option[Set[InferenceConstraint]] = (x,y) match {
    case (_,TopType) => Some(HashSet.empty)
    case (BottomType,_) => Some(HashSet.empty)
    case (UnitType,UnitType) => Some(HashSet.empty)
    case (rx: RecordType,ry: RecordType) => {
      val width = if(rx.length >= ry.length) Some(Set.empty[InferenceConstraint]) else None
      val depths = rx.fields.zip(ry.fields).map(taus => {
        if(taus._1.name == taus._2.name)
          lt(taus._1.tau,taus._2.tau).map(s => s + SubsumptionConstraint(taus._1.mutable,taus._2.mutable))
        else
          None
      })
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (fx: FunctionPointer,fy: FunctionPointer) => PhysicalTypeRelation.lt(fx,fy)
    case (bx: BrandType,by: BrandType) =>
      if(by.brand.containedBrands(bx.brand.name) == by.brand)
        (lt(bx.brand.record,by.brand.record),lt(bx.extension,by.extension)) match {
          case (Some(cx),Some(cy)) => Some(cx ++ cy)
          case _ => None
        }
      else
        None
    case (nx: NumericalType,ny: NumericalType) => if((nx == ny) || (nx enclosedIn ny)) Some(Set.empty) else None
    case (vx: TypeVariable,vy: TypeVariable) => {
      val constraint: InferenceConstraint = SubsumptionConstraint(vx,vy)
      if(vx == vy || vx.name == vy.name && vy.universal || assumptions.contains(constraint) || assumptions.contains(EqualityConstraint(vx,vy)))
        Some(Set.empty)
      else
        Some(Set.empty + constraint)
    }
    case (vx: TypeVariable,_) => {
      val empty = HashSet.empty[InferenceConstraint]
      Some(empty + SubsumptionConstraint(vx,y))
    }
    case (_,vy: TypeVariable) => {
      if(vy.universal)
        Some(Set.empty)
      else
        Some(Set.empty + SubsumptionConstraint(x,vy))
    }
    case (_,_) => {
      val constraint = SubsumptionConstraint(x,y)
      if(x == y || assumptions.contains(constraint) || assumptions.contains(EqualityConstraint(x,y)))
        Some(Set.empty[InferenceConstraint])
      else
        Some(HashSet(constraint))
    }
  }
  override def equiv(x: MonoType,y: MonoType): Option[Set[InferenceConstraint]] = (x,y) match {
    case (rx: RecordType,ry: RecordType) => {
      val width = if(rx.length == ry.length) Some(HashSet.empty[InferenceConstraint]) else None
      val depths = rx.fields.zip(ry.fields).map(taus => equiv(taus._1.tau,taus._2.tau))
      depths.foldLeft(width)((res,depth: Option[Set[InferenceConstraint]]) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    //Function pointers have to physiosubtype each other in a systems language, since there can be no implicit upcasts/coercions of arguments or results.
    case (fx: FunctionPointer,fy: FunctionPointer) => PhysicalTypeRelation.equiv(fx,fy)
    case (bx: BrandType,by: BrandType) =>
      if(bx.brand == by.brand)
        equiv(bx.extension,by.extension)
      else
        None
    case (bx: BoundedTypeVariable,by: BoundedTypeVariable) => equiv(bx.signature,bx.signature)
    case (vx: TypeVariable,vy: TypeVariable) => {
      val empty = HashSet.empty[InferenceConstraint]
      if(vx == vy || vx.name == vy.name && vy.universal || assumptions.contains(EqualityConstraint(vx,vy)))
        Some(empty)
      else
        Some(empty + EqualityConstraint(vx,vy))
    }
    case (vx: TypeVariable,_) => Some(Set.empty + EqualityConstraint(vx,y))
    case (_,vy: TypeVariable) => {
      if(vy.universal)
        Some(Set.empty)
      else
        Some(Set.empty + EqualityConstraint(x,vy))
    }
    case (_,_) => {
      if(assumptions.contains(EqualityConstraint(x,y)) || (x == y))
        Some(Set.empty)
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
    case (rx: RecordType,ry: RecordType) => {
      val width = if(rx.length >= ry.length) Some(HashSet.empty[InferenceConstraint]) else None
      val depths = rx.fields.zip(ry.fields).map(taus => {
        if(taus._1.name == taus._2.name)
          equiv(taus._1.tau,taus._2.tau).map(s => s + SubsumptionConstraint(taus._1.mutable,taus._2.mutable))
        else
          None
      })
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (bx: BrandType,by: BrandType) =>
      if(by.brand.containedBrands(bx.brand.name) == by.brand)
        (lt(bx.brand.record,by.brand.record),lt(bx.extension,by.extension)) match {
          case (Some(cx),Some(cy)) => Some(cx ++ cy)
          case _ => None
        }
      else
        None
    //Physiosubtyping allows only width extension, not depth.  To physiosubtype each other, function pointers must equal each other.
    case (fx: FunctionPointer,fy: FunctionPointer) => equiv(fx,fy)
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
      if(x == y || assumptions.contains(constraint) || assumptions.contains(EqualityConstraint(x,y)))
        Some(Set.empty[InferenceConstraint])
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
