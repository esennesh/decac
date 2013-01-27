package org.deca.compiler.signature

import org.jllvm._
import scala.collection.mutable.Stack
import scala.collection.immutable.HashSet
import scala.collection.immutable.HashMap
import scala.collection.mutable.LatticeOrdering
import scala.collection.mutable.GraphLattice

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

case class TypeConstructorCall(constructor: TypeConstructor,params: List[MonoSignature]) extends MonoType {
  override def compile: LLVMType = constructor.compile(params)
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = constructor.represent(params).filterT(pred) ++ (if(pred(this)) Set.empty + this else Set.empty)
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = constructor.represent(params).filterR(pred)
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = constructor.represent(params).filterE(pred)
  override def toString: String = constructor.name + "<" + params.tail.foldLeft(params.head.toString)((s,p) => s + "," + p.toString) + ">"
  override def mapT(f: (MonoType) => MonoType): MonoType = f(new TypeConstructorCall(constructor,params.map(tau => tau.mapT(f))))
  override def mapE(f: (MonoEffect) => MonoEffect): TypeConstructorCall = new TypeConstructorCall(constructor,params.map(tau => tau.mapE(f)))
  override def mapR(f: (MonoRegion) => MonoRegion): TypeConstructorCall = new TypeConstructorCall(constructor,params.map(tau => tau.mapR(f)))
  override def variables: Set[SignatureVariable] = {
    params.foldLeft(HashSet.empty[SignatureVariable])((result,param) => result ++ param.variables)
  }
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

class RecursiveType(tau: MonoType,loopNode: Option[MonoType]) extends MonoType {
  val innards: MonoType = loopNode match {
    case None => tau
    case Some(alpha) => tau.mapT((sig: MonoType) => if(sig == alpha) this else sig)
  }
  
  def unfold: (OpaqueType,MonoType) = {
    val loop = new OpaqueType
    (loop,innards.mapT((tau: MonoType) => if(tau == this) loop else tau))
  }
  
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = unfold._2.filterT(pred) ++ (if(pred(this)) Set.empty + this else Set.empty)
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = unfold._2.filterR(pred)
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = unfold._2.filterE(pred)
  
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
  
  override def compile: LLVMIdentifiedStructType = {
    val opaque = new OpaqueType
    opaque.resolve(List(innards.mapT((tau: MonoType) => if(tau == this) opaque else tau)))
  }
  
  override def toString: String = {
    val alpha = new TypeVariable(false,None)
    "mu " + alpha.toString + "." + innards.mapT((tau: MonoType) => if(tau == this) alpha else tau).toString
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

case class TaggedRecord(name: String,tag: Any,record: RecordType) {
  def tagCode: Int = tag match {
    case i: Int => i
    case ref: AnyRef => tag.hashCode
  }
  def compileTag: LLVMConstantInteger = {
    val tagSize = math.floor(math.log(tagCode) / math.log(2)).toInt + 1
    assert(tagSize > 0)
    val tagType = new LLVMIntegerType(tagSize)
    LLVMConstantInteger.constantInteger(tagType,tagCode,false)
  }
  override def toString: String = {
    val fields = record.fields.foldRight("")((field,result) => (field.name match {
      case Some(str) => str + ": " + field.tau.toString
      case None => field.tau.toString
    }) + (if(result != "") "," else "") + result)
    name + (if(fields != "") "(" + fields + ")" else "")
  }
}

class ZippableMap[A,B](val m: Map[A,B]) {
  def zipElements[C](that: Map[A,C]): Map[A,(B,C)] = {
    val xys = m.map(xs => (xs._1,xs._2,that.get(xs._1)))
    val intersection = xys.foldRight(Nil : List[(A,(B,C))])((head: (A,B,Option[C]),rest: List[(A,(B,C))]) => head._3 match {
      case Some(c) => (head._1,(head._2,c)) :: rest
      case None => rest
    })
    HashMap.empty[A,(B,C)] ++ intersection
  }
}

class SumType(trs: Iterable[TaggedRecord]) extends MonoType {
  assert(!trs.isEmpty)
  val cases: Map[String,TaggedRecord] = HashMap.empty[String,TaggedRecord] ++ trs.map(tr => (tr.name,tr))
  implicit def zippingMap[A,B](m: Map[A,B]): ZippableMap[A,B] = new ZippableMap[A,B](m)
  
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = {
    val cs = cases.values.map(tr => tr.record.filterT(pred))
    cs.foldLeft(Set.empty[MonoType])((res,c) => res ++ c) ++ (if(pred(this)) Set.empty + this else Set.empty)
  }
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = {
    val cs = cases.values.map(tr => tr.record.filterE(pred))
    cs.foldLeft(Set.empty[MonoEffect])((res,c) => res ++ c)
  }
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = {
    val cs = cases.values.map(tr => tr.record.filterR(pred))
    cs.foldLeft(Set.empty[MonoRegion])((res,c) => res ++ c)
  }
  override def mapT(f: MonoType => MonoType): MonoType = f(new SumType(cases.values.map(tr => TaggedRecord(tr.name,tr.tag,tr.record.mapT(f).asInstanceOf[RecordType]))))
  override def mapE(f: (MonoEffect) => MonoEffect): SumType = new SumType(cases.values.map(tr => TaggedRecord(tr.name,tr.tag,tr.record.mapE(f))))
  override def mapR(f: (MonoRegion) => MonoRegion): SumType = new SumType(cases.values.map(tr => TaggedRecord(tr.name,tr.tag,tr.record.mapR(f))))
  override def variables: Set[SignatureVariable] = {
    val empty: Set[SignatureVariable] = HashSet.empty
    cases.foldLeft(empty)((result,tr) => result ++ tr._2.record.variables)
  }
  def enumeration: Boolean = cases.forall(trec => trec._2.record.length == 0)
  
  def tagRepresentation: LLVMIntegerType = {
    val tagSize = math.floor(math.log(cases.size) / math.log(2)).toInt + 1
    if(tagSize <= 0)
      throw new Exception("Size of sum-type tag in bits is non-positive: " + tagSize.toString)
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
    val sum = cases.values.foldRight("")((c,res: String) => c.toString + (if(res != "") " + " else "") + res)
    "<" + sum + ">"
  }
}

class ExistentialInterface(r: RecordType,abs: MonoType,w: Option[MonoType] = None) extends MonoType {
  protected val witness = w
  val effects: MonoEffect = witness match {
    case Some(wit) => {
      val effs = wit.filterE(eff => true)
      SetEffect(effs)
    }
    case None => PureEffect
  }
  val skolem = {
    val shape = r.replace(abs,TopType).asInstanceOf[RecordType]
    val shapeVariables = shape.variables
    val constructor = SkolemConstructors.get(shape)
    witness match {
      case Some(w) => {
        assert(w.variables.forall(tvar => shapeVariables.contains(tvar)))
        constructor.witness(w)
      }
      case None => Unit
    }
    //Remember: existential skolem-constructors have to reveal their "closed over" region and effect variables, not just their "closed over" type variables.
    new TypeConstructorCall(constructor,shapeVariables.toList)
  }
  val shape = r.replace(abs,skolem).asInstanceOf[RecordType]
  override def filterT(pred: MonoType => Boolean): Set[MonoType] = {
    shape.filterT(pred) ++ (if(pred(this)) (Set.empty[MonoType] + this) else Set.empty[MonoType])
  }
  override def filterR(pred: MonoRegion => Boolean): Set[MonoRegion] = {
    shape.filterR(pred) ++ (witness match {
      case Some(w) => w.filterR(pred)
      case None => Set.empty
    })
  }
  override def filterE(pred: MonoEffect => Boolean): Set[MonoEffect] = {
    shape.filterE(pred) ++ (witness match {
      case Some(w) => w.filterE(pred)
      case None => Set.empty
    })
  }
  override def mapT(f: MonoType => MonoType) = f(new ExistentialInterface(shape.mapT(f).asInstanceOf[RecordType],skolem,witness.map(tau => tau.mapT(f))))
  override def mapR(f: MonoRegion => MonoRegion) = new ExistentialInterface(shape.mapR(f),skolem,witness.map(tau => tau.mapR(f)))
  override def mapE(f: MonoEffect => MonoEffect) = new ExistentialInterface(shape.mapE(f),skolem,witness.map(tau => tau.mapE(f)))
  override def compile: LLVMType = shape.compile
  override def variables: Set[SignatureVariable] = shape.variables
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
    case (fx: FunctionPointer,fy: FunctionPointer) => {
      val width = if(fx.domain.length == fy.domain.length) Some(HashSet.empty[InferenceConstraint]) else None
      val depths = PhysicalTypeRelation.lt(fx.range,fy.range) :: fx.domain.zip(fy.domain).map(taus => PhysicalTypeRelation.lt(taus._2,taus._1))
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (sx: SumType,sy: SumType) => {
      val width = if(sx.cases.size <= sy.cases.size) Some(Set.empty[InferenceConstraint]) else None
      val depths = sx.cases.zip(sy.cases).map(cs =>
        if(cs._1._1 == cs._2._1)
          lt(cs._1._2.record,cs._2._2.record)
        else
          None
      )
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (ex: ExistentialInterface,ey: ExistentialInterface) => lt(ex.shape,ey.shape)
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
    //Function pointers have to physiosubtype each other in a systems language, since there can be no implicit upcasts/coercions of arguments or results.
    case (fx: FunctionPointer,fy: FunctionPointer) => PhysicalTypeRelation.lt(fx,fy)
    case (sx: SumType,sy: SumType) => {
      val width = if(sx.cases.size == sy.cases.size) Some(HashSet.empty[InferenceConstraint]) else None
      val depths = sx.cases.zip(sy.cases).map(cs => if(cs._1._1 == cs._2._1) equiv(cs._1._2.record,cs._2._2.record) else None)
      depths.foldLeft(width)((res,depth) => (res,depth) match {
        case (Some(resset),Some(set)) => Some(resset union set)
        case _ => None
      })
    }
    case (ex: ExistentialInterface,ey: ExistentialInterface) => equiv(ex.shape,ey.shape)
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
    //Physiosubtyping allows only width extension, not depth.  To physiosubtype each other, function pointers must equal each other.
    case (fx: FunctionPointer,fy: FunctionPointer) => {
      val width = if(fx.domain.length == fy.domain.length) Some(HashSet.empty[InferenceConstraint]) else None
      val depths = equiv(fx.range,fy.range) :: fx.domain.zip(fy.domain).map(taus => equiv(taus._2,taus._1))
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
    case (ex: ExistentialInterface,ey: ExistentialInterface) => lt(ex.shape,ey.shape)
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
