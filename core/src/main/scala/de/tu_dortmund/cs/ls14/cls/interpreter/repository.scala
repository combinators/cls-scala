package de.tu_dortmund.cs.ls14.cls.interpreter

import de.tu_dortmund.cs.ls14.cls.inhabitation.{BoundedCombinatoryLogic, InhabitationAlgorithm, Tree, TreeGrammar, TreeGrammarEnumeration}
import de.tu_dortmund.cs.ls14.cls.types.{Type, _}

import scala.annotation.StaticAnnotation
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.reflect.NameTransformer

class combinator extends StaticAnnotation

sealed trait CombinatorInfo {
  val name: String
  val parameters: Option[Seq[universe.Type]]
  val result: universe.Type
  val semanticType: Option[Type]
  def =:=(other: CombinatorInfo): Boolean =
    name == other.name &&
      semanticType == other.semanticType &&
      result =:= other.result &&
      ((parameters, other.parameters) match {
        case (None, None) => true
        case (Some(ps), Some(otherPs)) if ps.size == otherPs.size =>
          ps.zip(otherPs).forall(p => p._1 =:= p._2)
      })
}
case class StaticCombinatorInfo(name: String,
  parameters: Option[Seq[universe.Type]],
  result: universe.Type,
  semanticType: Option[Type]) extends CombinatorInfo
case class DynamicCombinatorInfo[A](name: String,
  parameters: Option[Seq[universe.Type]],
  result: universe.Type,
  semanticType: Option[Type],
  instance: A,
  combinatorTypeTag: WeakTypeTag[A]) extends CombinatorInfo

case class InhabitationResult[T](grammar: TreeGrammar, target: Type, resultInterpreter: Tree => T) {
  val terms = TreeGrammarEnumeration(grammar, target)
  val interpretedTerms = terms.map(resultInterpreter)

  def isInfinite: Boolean = {
    def visit(seen: Set[Type], start: Type): Boolean = {
      if (seen.contains(start)) true
      else {
        grammar(start).exists {
          case (_, types) =>
            types.exists(ty => visit(seen + start, ty))
        }
      }
    }
    visit(Set.empty, target)
  }
}

trait ReflectedRepository[A] { self =>

  import ReflectedRepository._
  import scala.tools.reflect.ToolBox

  val typeTag: WeakTypeTag[A]
  val instance: A
  val semanticTaxonomy: Taxonomy
  val kinding: Kinding
  val algorithm: InhabitationAlgorithm

  protected def tb = universe.runtimeMirror(this.getClass.getClassLoader()).mkToolBox()

  protected def findCombinatorComponents: Map[String, CombinatorInfo] = {
    typeTag.tpe.members.flatMap (member =>
        member.annotations.foldLeft[Seq[CombinatorInfo]](Seq()) {
          case (Seq(), c) if c.tree.tpe =:= universe.typeOf[combinator] =>
            Seq(staticCombinatorInfoFor(member.name.toString, member.typeSignature))
          case (s, _) => s
        }
    ).map(cInfo => cInfo.name -> cInfo).toMap
  }
  lazy val combinatorComponents: Map[String, CombinatorInfo] = findCombinatorComponents

  def applyMethodInfoFor(typeSignature: universe.Type): (Option[Seq[universe.Type]], universe.Type) = {
    val applyMember = typeSignature.member(TermName("apply"))
    if (!applyMember.isMethod)
      throw new RuntimeException("Combinators need to have an apply method")
    val applyMethod = applyMember.asMethod
    if (applyMethod.typeParams.nonEmpty)
      throw new RuntimeException("Combinator methods cannot have type parameters")
    applyMethod.typeSignatureIn(typeSignature) match {
        case NullaryMethodType(result) => (None, result.dealias)
        case MethodType(params, result) =>
          val paramTys =
            Some(params.map(p => {
              val byName = definitions.ByNameParamClass
              val paramTy = p.info match {
                case TypeRef(_, sym, pTy :: Nil) if sym == byName => pTy // lazyness => T
                case pTy => pTy
              }
              paramTy.dealias
            }))
          (paramTys, result.dealias)
      }
  }

  def staticCombinatorInfoFor(combinatorName: String, typeSignature: universe.Type): StaticCombinatorInfo = {
    val (applyMethodParameters, applyMethodResult) = applyMethodInfoFor(typeSignature)
    val tb = this.tb
    val semanticType =
      typeSignature
        .members
        .find(m => m.name.toString == "semanticType")
        .map(semType =>
          tb.eval(
            q"""import de.tu_dortmund.cs.ls14.cls.types.Type;
                import de.tu_dortmund.cs.ls14.cls.types.syntax._;
                identity[Type]({
                  ${reify(instance).in(tb.mirror)}
                    .asInstanceOf[${typeTag.in(tb.mirror).tpe}]
                    .${TermName(NameTransformer.encode(combinatorName))}
                    .semanticType
                  })"""
          ).asInstanceOf[Type])
    StaticCombinatorInfo(combinatorName, applyMethodParameters, applyMethodResult, semanticType)
  }
  def dynamicCombinatorInfoFor[C](combinatorName: String, combinatorInstance: C)
    (implicit combinatorTypeTag: WeakTypeTag[C]): DynamicCombinatorInfo[C] = {
    val (applyMethodParameters, applyMethodResult) = applyMethodInfoFor(combinatorTypeTag.tpe)
    val tb = this.tb
    val semanticType =
      combinatorTypeTag
        .tpe
        .members
        .find(m => m.name.toString == "semanticType")
        .map(semType =>
          tb.eval(
            q"""${reify(combinatorInstance).in(tb.mirror)}
                  .asInstanceOf[${combinatorTypeTag.in(tb.mirror).tpe}]
                  .semanticType"""
          ).asInstanceOf[Type])
    DynamicCombinatorInfo(combinatorName, applyMethodParameters, applyMethodResult, semanticType, combinatorInstance, combinatorTypeTag)
  }


  lazy val scalaTypes: Set[universe.Type] =
    combinatorComponents.values.flatMap(combinatorInfo =>
      combinatorInfo.parameters.map(_.toSet).getOrElse(Set.empty) + combinatorInfo.result
    ).toSet

  lazy val nativeTypes: Set[Constructor] =
    scalaTypes.map((ty: universe.Type) => nativeTypeOf(ty))

  lazy val combinators: Map[String, Type] =
    combinatorComponents
      .mapValues(combinatorInfo =>
        combinatorInfo.semanticType match {
          case None => nativeTypeOf (combinatorInfo)
          case Some(semTy) => Intersection(nativeTypeOf(combinatorInfo), semTy)
        })

  lazy val nativeTypeTaxonomy: NativeTaxonomyBuilder =
    new NativeTaxonomyBuilder(scalaTypes)

  def evalInhabitant[A](inhabitant: Tree): A = {
    val tb = this.tb
    def toTermName(name: String) =
      TermName(NameTransformer.encode(name))
    def toCombinatorInstanceTree(info: CombinatorInfo): universe.Tree =
      info match {
        case StaticCombinatorInfo(name, _, _, _) =>
          q"${reify(this.instance).in(tb.mirror)}.asInstanceOf[${typeTag.in(tb.mirror).tpe}].${toTermName(name)}"
        case DynamicCombinatorInfo(_, _, _, _, combinatorInstance, combinatorTypeTag) =>
          q"${reify(combinatorInstance).in(tb.mirror)}.asInstanceOf[${combinatorTypeTag.in(tb.mirror).tpe}]"
      }
    def constructTerm(inhabitant: Tree): universe.Tree =
      inhabitant match {
        case Tree(name)
          if combinatorComponents(name).parameters.isEmpty =>
          q"${toCombinatorInstanceTree(combinatorComponents(name))}.apply"
        case Tree(name) =>
          q"${toCombinatorInstanceTree(combinatorComponents(name))}()"
        case Tree(name, arguments@_*) =>
          q"${toCombinatorInstanceTree(combinatorComponents(name))}(..${arguments.map(constructTerm)})"
      }
    tb.eval(constructTerm(inhabitant)).asInstanceOf[A]
  }


  def inhabit[T](semanticTypes: Type*)(implicit targetTag: WeakTypeTag[T]): InhabitationResult[T] = {
    val fullTaxonomy = nativeTypeTaxonomy.addNativeType[T].taxonomy.merge(semanticTaxonomy)
    val targetTypes = nativeTypeOf[T] +: semanticTypes
    val targetType = targetTypes.init.foldRight(targetTypes.last){ case (ty, tgt) => Intersection(ty, tgt) }
    val result = algorithm(kinding, SubtypeEnvironment(fullTaxonomy), combinators)(targetType)
    InhabitationResult(result, targetType, evalInhabitant[T])
  }

  def addCombinator[C](name: String, combinator: C)(implicit combinatorTag: WeakTypeTag[C]): ReflectedRepository[A] = {
    new ReflectedRepository[A] {
      lazy val typeTag = self.typeTag
      lazy val instance = self.instance
      lazy val semanticTaxonomy = self.semanticTaxonomy
      lazy val kinding = self.kinding
      lazy val algorithm = self.algorithm
      override lazy val combinatorComponents: Map[String, CombinatorInfo] =
        findCombinatorComponents + (name -> dynamicCombinatorInfoFor(name, combinator))
    }
  }
}

object ReflectedRepository {
  def nativeTypeOf[A](implicit aTag: WeakTypeTag[A]): Constructor =
    nativeTypeOf(aTag.tpe)

  def nativeTypeOf(ty: universe.Type): Constructor =
    Constructor(show(ty.dealias))

  def nativeTypeOf(combinatorInfo: CombinatorInfo): Type =
    combinatorInfo.parameters
      .getOrElse(Seq.empty)
      .map((x: universe.Type) => nativeTypeOf(x))
      .foldRight[Type](nativeTypeOf(combinatorInfo.result)) {
        case (parameter, result) => Arrow(parameter, result)
      }

  def apply[R](inst: R,
    semanticTaxonomy: Taxonomy = Taxonomy.empty,
    kinding: Kinding = Kinding.empty,
    algorithm : InhabitationAlgorithm = BoundedCombinatoryLogic.algorithm
  )(implicit tag: WeakTypeTag[R]): ReflectedRepository[R] = {
    val algo = algorithm
    val semTax = semanticTaxonomy
    val knd = kinding
    new ReflectedRepository[R] {
      lazy val typeTag = tag
      lazy val instance = inst
      lazy val semanticTaxonomy = semTax
      lazy val kinding = knd
      lazy val algorithm = algo
    }
  }
}



