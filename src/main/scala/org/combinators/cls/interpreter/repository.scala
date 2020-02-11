/*
 * Copyright 2017 Jan Bessai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.combinators.cls.interpreter

import java.util.UUID

import org.combinators.cls.inhabitation.{BoundedCombinatoryLogic, InhabitationAlgorithm, Tree, Rule, TreeGrammarEnumeration, Failed, Apply}
import org.combinators.cls.types.{Type, _}
import shapeless.feat

import scala.annotation.StaticAnnotation
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.reflect.NameTransformer

/** Marker annotation to find combinators via reflection. */
class combinator extends StaticAnnotation

/** Combinator information collected via reflection.
  * Used to turn scala objects into combinators for BoundedCombinatoryLogic and to interpret inhabitatoin results
  * back into call of their apply methods.
  */
sealed trait CombinatorInfo {
  /** The name of the combinator */
  val name: String
  /** The parameter types of the combinator's apply method.
    * None, for def `apply: A` and Some(Seq()) for `apply(): A`.
    */
  val parameters: Option[Seq[universe.Type]]
  /** The result type of the combinator's apply method. */
  val result: universe.Type
  /** The optional semantic type annotation (given in the field `semanticType: Type`). */
  val semanticType: Option[Type]
  /** Computes if two combinator information objects are equal up to Scala type equality of their parameters and results.
    * This is necessary if the same combinator is looked at form different scopes, where in one scope reflection
    * information contains fully qualified packages, while in the other it doesn't.
    */
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
/** Combinator information collected for a compile time static singleton object.
  *
  * @param fullSignature the full type signature of the reflected object (for debugging information).
  */
case class StaticCombinatorInfo(name: String,
  parameters: Option[Seq[universe.Type]],
  result: universe.Type,
  semanticType: Option[Type],
  fullSignature: universe.Type) extends CombinatorInfo

/** Combinator information for a runtime generated object.
  *
  * @param instance the runtime instance of the reflected object.
  * @param combinatorTypeTag reflected type information of the `instance` object.
  * @param position a stack trace, indicating where the object was added to the repository.
  * @param uniqueNameTag a unique name tag, which will be part of the name of the resulting combinator.
  */
case class DynamicCombinatorInfo[A](name: String,
  parameters: Option[Seq[universe.Type]],
  result: universe.Type,
  semanticType: Option[Type],
  instance: A,
  combinatorTypeTag: WeakTypeTag[A],
  position: Array[StackTraceElement],
  uniqueNameTag: String = UUID.randomUUID().toString) extends CombinatorInfo

/** Encapsulates an inhabitation result.
  *
  * @param rules a tree grammar rule representation of all inhabitants.
  * @param target the inhabitation request target type.
  * @param resultInterpreter an interpreter to turn inhabitants into scala objects.
  * @tparam T the native scala type of the requested inhabitants.
  */
case class InhabitationResult[T](rules: Set[Rule], target: Type, resultInterpreter: Tree => T) {
  /** The rules grouped by their target. */
  lazy val groupedRules: Map[Type, Set[Rule]] = rules.groupBy(_.target)
  /** The (possibly infinite) enumeration of all inhabitants . */
  val terms: feat.Enumeration[Tree] = TreeGrammarEnumeration(rules, target)
  /** The (possibly infinite) enumeration of all (lazyly) interpreted inhabitants . */
  val interpretedTerms: feat.Enumeration[T] = terms.map(resultInterpreter)

  /** Indicates if this result is empty. */
  def isEmpty: Boolean = rules.exists(_ == Failed(target)) || rules.forall(_.target != target)

  /** Indicates if this result contains infinitely many inhabitants. */
  def isInfinite: Boolean = {
    def visit(seen: Set[Type], start: Type): Boolean = {
      if (seen.contains(start)) true
      else groupedRules(start).exists {
        case Apply(_, lhs, rhs) => visit(seen + start, lhs) || visit(seen + start, rhs)
        case _ => false
      }
    }
    !isEmpty && visit(Set.empty, target)
  }

  /** Computes the number of results, or None if this result is infinite. */
  def size: Option[BigInt] = {
    Option(!isInfinite).collect {
      case true if isEmpty => 0
      case true => terms.values.foldLeft(0){case (s, (_, xs)) => s + xs.size }
    }
  }
}

/** A Bounded Combinatory Logic repository constructed from Scala code via reflection.
  *
  * Use [ReflectedRepository.apply] of the companion object to obtain a new instance.
  *
  * @tparam R the Scala type of the repository.
  */
trait ReflectedRepository[R] { self =>

  import ReflectedRepository._
  import scala.tools.reflect.ToolBox

  /** Reflection information for the Scala repository. */
  val typeTag: WeakTypeTag[R]
  /** Instance of the Scala repository. */
  val instance: R
  /** Taxonomy for semantic type information. */
  val semanticTaxonomy: Taxonomy
  /** Finite restriction on well-formed substitutions. */
  val substitutionSpace: FiniteSubstitutionSpace
  /** The algorithm to use. */
  val algorithm: InhabitationAlgorithm
  /** The class loader used for interpreting inhabitation results. */
  val classLoader: ClassLoader

  /** The compiler toolbox used to runtime compile interpreted results. */
  protected def tb: ToolBox[universe.type] = universe.runtimeMirror(classLoader).mkToolBox()

  /** Performs reflection to find all objects in the Scala repository, which are annotated by [[combinator]].
    * Overload this to add additional combinators.
    *
    * @return A map from combinator names to their reflected information.
    */
  protected def findCombinatorComponents: Map[String, CombinatorInfo] = {
    typeTag.tpe.members.flatMap (member =>
        member.annotations.foldLeft[Seq[CombinatorInfo]](Seq()) {
          case (Seq(), c) if c.tree.tpe =:= universe.typeOf[combinator] =>
            Seq(staticCombinatorInfoFor(member.name.toString, member.typeSignature))
          case (s, _) => s
        }
    ).map(cInfo => cInfo.name -> cInfo).toMap
  }

  /** A map from combinator names to reflected combinator information.
    * Obtained by findCombinatorComponents.
    */
  lazy val combinatorComponents: Map[String, CombinatorInfo] = findCombinatorComponents

  /** Extracts the type information of an apply method inside of the type described by `typeSignature`.
    * The apply method must be unique, declared via def, and cannot have type parameters.
    *
    * @return a pair of the parameter types and the result type, where the first component is None if there are no
    *         parameters (`def apply: A`), or Some(Seq.empty), if there are empty parameters (`def apply(): A`).
    */
  def applyMethodInfoFor(combinatorName: String, typeSignature: universe.Type): (Option[Seq[universe.Type]], universe.Type) = {
    val applyMember = typeSignature.member(TermName("apply"))
    if (!applyMember.isMethod)
      throw new RuntimeException(s"$combinatorName: Combinators need to have an apply method")
    val applyMethod = applyMember.asMethod
    if (applyMethod.typeParams.nonEmpty)
      throw new RuntimeException(s"$combinatorName: Combinator methods cannot have type parameters")
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

  /** Obtains the [[StaticCombinatorInfo]] for a single [[combinator]]-annotated object inside of the repository.
    * The object must have an apply-Method found by [[applyMethodInfoFor]].
    * It can include a field `val semanticType: Type`, which will be interpreted as the semantic type of the combinator.
    * The latter interpretation internally performs locking, because the Scala runtime compiler used to dynamically
    * obtain the field content is not thread safe.
    *
    * @param combinatorName the name of the combinator to add.
    * @param typeSignature reflected type information of the combinator object.
    * @return the [[StaticCombinatorInfo]] for `typeSignature`.
    */
  def staticCombinatorInfoFor(combinatorName: String, typeSignature: universe.Type): StaticCombinatorInfo =
    ReflectedRepository.synchronized {
      val (applyMethodParameters, applyMethodResult) = applyMethodInfoFor(combinatorName, typeSignature)
      val tb = this.tb
      val semanticType =
        typeSignature
          .members
          .collectFirst {
            case m if m.name.toString == "semanticType" =>
              tb.eval(
                q"""import org.combinators.cls.types.Type;
                    import org.combinators.ls14.cls.types.syntax._;
                    identity[Type]({
                      ${reify(instance).in(tb.mirror)}
                        .asInstanceOf[${typeTag.in(tb.mirror).tpe}]
                        .${TermName(NameTransformer.encode(combinatorName))}
                        .semanticType
                      })"""
              ).asInstanceOf[Type]
          }
      StaticCombinatorInfo(combinatorName, applyMethodParameters, applyMethodResult, semanticType, typeSignature.dealias)
    }

  /** Obtains the [[DynamicCombinatorInfo]] for a single object programmatically added to the repository.
    * The object must have an apply-Method found by [[applyMethodInfoFor]].
    * It can include a field `val semanticType: Type`, which will be interpreted as the semantic type of the combinator.
    * The latter interpretation internally performs  locking, because the Scala runtime compiler used to dynamically
    * obtain the field content is not thread safe.
    *
    * @param combinatorInstance the object to obtain information for.
    * @param combinatorName the name of the combinator to add; it will be augmented by a random generated UUID
    *                       avoiding name clashes.
    * @param position a stack trace of the code position where the combinator is added -- required for debugging.
    * @return the [[DynamicCombinatorInfo]] for `combinatorInstance`.
    */
  def dynamicCombinatorInfoFor[C](combinatorName: String, combinatorInstance: C, position: Array[StackTraceElement])
    (implicit combinatorTypeTag: WeakTypeTag[C]): DynamicCombinatorInfo[C] =
    ReflectedRepository.synchronized {
      val (applyMethodParameters, applyMethodResult) = applyMethodInfoFor(combinatorName, combinatorTypeTag.tpe)
      val tb = this.tb
      val semanticType =
        combinatorTypeTag
          .tpe
          .members
          .collectFirst {
            case m if m.name.toString == "semanticType" =>
              tb.eval(
                q"""${reify(combinatorInstance).in(tb.mirror)}
                      .asInstanceOf[${combinatorTypeTag.in(tb.mirror).tpe}]
                      .semanticType"""
              ).asInstanceOf[Type]
          }
      DynamicCombinatorInfo(combinatorName,
        applyMethodParameters,
        applyMethodResult,
        semanticType,
        combinatorInstance,
        combinatorTypeTag,
        position)
    }

  /** Build a new reflected repository, which additionaly includes `combinator`.
    * The combinator to add must have a single monomorphic apply method and can additionally include a field
    * `val semanticType: Type`.
    * Its name will be based on the class name of `C`.
    *
    * @param combinator the new combinator to include.
    * @param position the stack trace of the position where this method was called -- used to provide debugging information.
    * @param combinatorTag reflection information for `combinator`.
    * @tparam C the type of the combinator object to add.
    * @return a new reflected repository augmented by `combinator`.
    */
  def addCombinator[C](combinator: C,
    position: Array[StackTraceElement] = Thread.currentThread().getStackTrace)
    (implicit combinatorTag: WeakTypeTag[C]): ReflectedRepository[R] = {
    new ReflectedRepository[R] {
      lazy val typeTag = self.typeTag
      lazy val instance = self.instance
      lazy val semanticTaxonomy = self.semanticTaxonomy
      lazy val substitutionSpace = self.substitutionSpace
      lazy val algorithm = self.algorithm
      lazy val classLoader: ClassLoader = self.classLoader
      override lazy val combinatorComponents: Map[String, CombinatorInfo] = {
        val dynamicCombinatorInfo = dynamicCombinatorInfoFor(combinatorTag.tpe.toString, combinator, position)
        val mapKey = s"DynamicCombinator(${dynamicCombinatorInfo.name}, ${dynamicCombinatorInfo.uniqueNameTag})"
        self.combinatorComponents + (mapKey -> dynamicCombinatorInfo)
      }
    }
  }

  /** The set of all scala types present in this reflected repository. */
  lazy val scalaTypes: Set[universe.Type] =
    combinatorComponents.values.flatMap(combinatorInfo =>
      combinatorInfo.parameters.map(_.toSet).getOrElse(Set.empty) + combinatorInfo.result
    ).toSet

  /** The set of all intersection types representing Scala types present in this reflected repository. */
  lazy val nativeTypes: Set[Constructor] =
    scalaTypes.map((ty: universe.Type) => nativeTypeOf(ty))

  /** Maps all combinator names in this repository to their full (native Scala and semantic) intersection type. */
  lazy val combinators: Map[String, Type] =
    combinatorComponents.transform((_, ty) => fullTypeOf(ty)).toMap

  /** A taxonomy representing the subtype relationship of all Scala types in this repository. */
  lazy val nativeTypeTaxonomy: NativeTaxonomyBuilder =
    new NativeTaxonomyBuilder(scalaTypes)

  /** Inteprets `inhabitant` as an instance of type `T`.
    * Performs runtime compilation and maps each combinator application to a call to the apply method of the
    * combinator's scala object.
    * The caller is responsible for proving a correct type `T`, otherwise runtime errors will occur.
    * Reconstructs the shape (empty of no argument list) of the apply method from `combinatorComponents`.
    * Example:
    * {{{
    *   evalInhabitant[Int](Tree("Foo", List(Tree("Bar"), Tree("Baz"))) = Foo.apply(Bar.apply, Baz.apply())
    * }}}
    * Uses the scala runtime compiler toolbox, which is not threadsafe.
    *
    * @param inhabitant the inhabitant to interpret.
    * @return the interpreted inhabitant.
    */
  def evalInhabitant[T](inhabitant: Tree): T = {
    val tb = this.tb
    def toTermName(name: String) =
      TermName(NameTransformer.encode(name))
    def toCombinatorInstanceTree(info: CombinatorInfo): universe.Tree =
      info match {
        case StaticCombinatorInfo(name, _, _, _, _) =>
          q"${reify(this.instance).in(tb.mirror)}.asInstanceOf[${typeTag.in(tb.mirror).tpe}].${toTermName(name)}"
        case DynamicCombinatorInfo(_, _, _, _, combinatorInstance, combinatorTypeTag, _, _) =>
          q"${reify(combinatorInstance).in(tb.mirror)}.asInstanceOf[${combinatorTypeTag.in(tb.mirror).tpe}]"
      }
    def constructTerm(inhabitant: Tree): universe.Tree =
      inhabitant match {
        case Tree(name, _)
          if combinatorComponents(name).parameters.isEmpty =>
          q"${toCombinatorInstanceTree(combinatorComponents(name))}.apply"
        case Tree(name, _) =>
          q"${toCombinatorInstanceTree(combinatorComponents(name))}()"
        case Tree(name, _, arguments@_*) =>
          q"${toCombinatorInstanceTree(combinatorComponents(name))}(..${arguments.map(constructTerm)})"
      }
    tb.eval(constructTerm(inhabitant)).asInstanceOf[T]
  }

  /** Uses type inhabitation relative to this reflected repository to obtain a result of Scala type `T` and
    * the intersection of all `semanticTypes`.
    *
    * @param semanticTypes the semantic types to inhabit.
    * @param targetTag reflection information for the target type.
    * @tparam T the native Scala type to inhabit.
    */
  def inhabit[T](semanticTypes: Type*)(implicit targetTag: WeakTypeTag[T]): InhabitationResult[T] =
    ReflectedRepository.synchronized {
      val fullTaxonomy = nativeTypeTaxonomy.addNativeType[T].taxonomy.merge(semanticTaxonomy)
      val targetTypes = nativeTypeOf[T] +: semanticTypes
      val targetType = targetTypes.init.foldRight(targetTypes.last){ case (ty, tgt) => Intersection(ty, tgt) }
      val result = algorithm(substitutionSpace, SubtypeEnvironment(fullTaxonomy.underlyingMap), combinators)(Seq(targetType))
      InhabitationResult(result, targetType, evalInhabitant[T])
    }

  /** Combines multiple inhabitation requests into one more efficient batch job.
    * Reuses shared intermediate results.
    * Create new batch jobs using [InhabitationBatchJob.apply[R](Type*)] and [InhabitationBatchJob.addJob[R](Type*)].
    */
  sealed trait InhabitationBatchJob { self =>
    /** The native Scala type of the last request. */
    type RequestType
    /** All semantic types of the last request.
      * These are be joined in a big intersection as in [[inhabit]].
      */
    val semanticTypes: Seq[Type]
    /** Reflected type information about the native Scala request type. */
    val typeTag: WeakTypeTag[RequestType]

    /** A combination of all native Scala inhabitation result types of this batch job. */
    type ResultType

    /** Collects subtype information about `RequestType`. */
    def enrichTaxonomyWithTargets(taxonomy: NativeTaxonomyBuilder): NativeTaxonomyBuilder =
      taxonomy.addNativeType[RequestType](typeTag)

    /** Computes a sequence of all requested target types of this batch job. */
    def targets: Seq[Type] = {
      val targetTypes = nativeTypeOf[RequestType](typeTag) +: semanticTypes
      Seq(targetTypes.init.foldRight(targetTypes.last) { case (ty, tgt) => Intersection(ty, tgt) })
    }

    /** Interpretes the tree grammar rules returned by the algorithm as the computed `ResultType`, that is
      * a combination of all requested native Scala types in this job. */
    def toResult(resultRules: Set[Rule]): ResultType

    /** Creates a new batch job, adding the request specified via `R` and `semanticTypes` to the requests in this job.
      * The result type is composed by creating a tuple of the current result type and the newly requested type `R`,
      * where `ResultType` is the first component and `R` is the second component.
      */
    def addJob[R](semanticTypes: Type*)(implicit tag: WeakTypeTag[R]): InhabitationBatchJob.AuxWithPrior[R, ResultType] = {
      val sts: Seq[Type] = semanticTypes
      new InhabitationBatchJob with HasPriorJob[ResultType] {
        type RequestType = R
        val typeTag: universe.WeakTypeTag[R] = tag
        val semanticTypes: Seq[Type] = sts
        lazy val priorJob: self.type = self
      }
    }

    /** Runs this batch job, returning the current native scala `ResultType`. */
    def run(): ResultType = {
      val fullTaxonomy = enrichTaxonomyWithTargets(nativeTypeTaxonomy).taxonomy.merge(semanticTaxonomy)
      val resultGrammar = algorithm(substitutionSpace, SubtypeEnvironment(fullTaxonomy.underlyingMap), combinators)(targets)
      toResult(resultGrammar)
    }
  }

  /** An [[InhabitationBatchJob]] with more than one recorded job.
    * @tparam P the combined native Scala type of prior requests.
    */
  sealed trait HasPriorJob[P] extends InhabitationBatchJob {
    override type ResultType = (P, InhabitationResult[RequestType])
    /** The rest of the batch job, for requests made before the request for type `P`. */
    val priorJob: InhabitationBatchJob.AuxWithResult[P]
    abstract override def enrichTaxonomyWithTargets(taxonomy: NativeTaxonomyBuilder): NativeTaxonomyBuilder =
      super.enrichTaxonomyWithTargets(priorJob.enrichTaxonomyWithTargets(taxonomy))

    abstract override def targets: Seq[Type] =
      super.targets ++ priorJob.targets

    override def toResult(resultRules: Set[Rule]): ResultType = {
      (priorJob.toResult(resultRules), // IntelliJ complains here for no reason, the code typechecks and compiles
        InhabitationResult(resultRules, super.targets.head, evalInhabitant[RequestType]))
    }
  }

  /** Helper object to create new batch jobs of inhabitation requests. */
  object InhabitationBatchJob {
    /** The type of batch jobs where the most recently requested native Scala type is `R`. */
    type Aux[R] = InhabitationBatchJob { type RequestType = R; type ResultType = InhabitationResult[R] }
    /** The type of batch jobs where the combination of all requested native Scala type is `R`. */
    type AuxWithResult[R] = InhabitationBatchJob { type ResultType = R }
    /** The type of batch jobs where the most recently requested native scala type is `R` and the
      * combination of all requested native Scala type is `(P, R)`.
      */
    type AuxWithPrior[R, P] = InhabitationBatchJob with HasPriorJob[P] {
      type RequestType = R
      type ResultType = (P, InhabitationResult[R])
    }
    /** Creates a new batch job, starting with a request for native Scala type `R` and the intersection of all
      * given `semanticTypes`.
      * Use [InhabitationBatchJob.addJob[R](Type*)] to add more requests.
      */
    def apply[R](semanticTypes: Type*)(implicit tag: WeakTypeTag[R]): Aux[R] = {
      val sts = semanticTypes
      new InhabitationBatchJob {
        type RequestType = R
        type ResultType = InhabitationResult[R]
        val typeTag: universe.WeakTypeTag[R] = tag
        val semanticTypes: Seq[Type] = sts
        override def toResult(resultRules: Set[Rule]): InhabitationResult[R] =
          InhabitationResult(resultRules, super.targets.head, evalInhabitant[R])
      }
    }
  }
}

/** Helper object to construct a [[ReflectedRepository]]. */
object ReflectedRepository {
  /** Translates native Scala type `A` into an intersection type constant. */
  def nativeTypeOf[A](implicit aTag: WeakTypeTag[A]): Constructor =
    nativeTypeOf(aTag.tpe)

  /** Translates the reflected native Scala type `ty` into an intersection type constant. */
  def nativeTypeOf(ty: universe.Type): Constructor =
    Constructor(show(ty.dealias), Omega)

  /** Constructs the native combinator type out of a [[CombinatorInfo]] object.
    * For this, `apply(x: A, y: B): C` becomes `A =>: B =>: C`.
    */
  def nativeTypeOf(combinatorInfo: CombinatorInfo): Type =
    combinatorInfo.parameters
      .getOrElse(Seq.empty)
      .map((x: universe.Type) => nativeTypeOf(x))
      .foldRight[Type](nativeTypeOf(combinatorInfo.result)) {
        case (parameter, result) => Arrow(parameter, result)
      }

  /** Constructs the full type of a combinator out of a [[CombinatorInfo]] object.
    * The full type is [[nativeTypeOf(CombinatorInfo)]] intersected with the semantic type.
    */
  def fullTypeOf(combinatorInfo: CombinatorInfo): Type =
    combinatorInfo.semanticType match {
      case None => nativeTypeOf(combinatorInfo)
      case Some(semTy) => Intersection(nativeTypeOf(combinatorInfo), semTy)
    }

  /** Obtains a [[ReflectedRepository]] from an instance of any type `R`.
    * Use [[ReflectedRepository.inhabit]] or [[ReflectedRepository.InhabitationBatchJob]] to perform type inhabitation
    * with the constructed repository.
    * If desired, pass in another inhabitation algorithm implementation, e.g. one creating debug information.
    *
    * @param inst the instance to obtain reflection information from.
    * @param semanticTaxonomy a subtype relation on semantic type constructors used in `inst`.
    * @param substitutionSpace a finite restriction on the space of well-formed substitutions used during inhabitation.
    * @param algorithm the inhabitation algorithm to use.
    * @param classLoader the class loader to resolve combinator implementations when interpreting inhabitation results.
    * @param tag reflection information to inspect `R`.
    * @tparam R the Scala type of the repository.
    * @return a repository that contains every combinator in `inst` and has methods to perform inhabitation.
    */
  def apply[R](inst: R,
    semanticTaxonomy: Taxonomy = Taxonomy.empty,
    substitutionSpace: FiniteSubstitutionSpace = FiniteSubstitutionSpace.empty,
    algorithm: InhabitationAlgorithm = BoundedCombinatoryLogic.algorithm,
    classLoader: ClassLoader = getClass.getClassLoader
  )(implicit tag: WeakTypeTag[R]): ReflectedRepository[R] = {
    val algo = algorithm
    val semTax = semanticTaxonomy
    val substSpace = substitutionSpace
    val loader = classLoader
    new ReflectedRepository[R] {
      lazy val typeTag = tag
      lazy val instance = inst
      lazy val semanticTaxonomy = semTax
      lazy val substitutionSpace = substSpace
      lazy val algorithm = algo
      lazy val classLoader = loader
    }
  }
}



