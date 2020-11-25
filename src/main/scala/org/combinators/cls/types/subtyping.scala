/*
 * Copyright 2018-2020 Jan Bessai
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

package org.combinators.cls.types

/** A path `p` conforms to the syntax:
  * <code>
  *   p ::= 'C(p) | C(Omega) | (Omega, Omega) | (Omega, p) | (p, Omega) | sigma_1 =>: ... =>: sigma_k =>: p
  * </code>
  * where each `sigma_i` is an arbitrary intersection type without variables.
  */
sealed trait Path extends Organized { self: Type =>
  final val paths: Seq[Type with Path] = Seq(this)
}

/** Type class to compute a minimal set wrt. some metric adhering to some constraint. */
trait Minimizable {
  type T

  /** Computes a minimal set of elements. */
  def minimize: Seq[T]
}

/** A type is organized iff it is syntactically identical to an intersection of paths. */
trait Organized { self: Type =>
  val paths: Seq[Type with Path]
}

/** Helper methods to (de-)construct paths from (/into) arguments and targets of arrows. */
object Path {

  /** Constructs a path ending in `target` and taking `args` as arrow parameters. */
  final def apply(
      args: Seq[Type] = Seq.empty,
      target: Type with Path
  ): Type with Path =
    args.foldRight[Type with Organized with Path](target) {
      case (arg, result) => new Arrow(arg, result) with Path
    }

  /** If possible, deconstructs a path `t` into a constructor or product, which is a path, and arrow parameters.
    * <code>
    *   unapply('A =>: 'B :&: 'C =>: 'D) = Some(Seq('A, 'B :&: 'C), 'D)
    *   unapply('A =>: 'B :&: 'C) = None
    * </code>
    */
  final def unapply(t: Type): Option[(Seq[Type], Type with Path)] =
    t match {
      case c: Constructor with Path => Some((Seq.empty, c))
      case Constructor(name, Omega) =>
        Some((Seq.empty, new Constructor(name, Omega) with Path))
      case Constructor(name, arg @ Path(_, _)) =>
        Some((Seq.empty, new Constructor(name, arg) with Path))
      case p: Product with Path => Some((Seq.empty, p))
      case Product(Omega, Omega) =>
        Some((Seq.empty, new Product(Omega, Omega) with Path))
      case Product(p @ Path(_, _), Omega) =>
        Some((Seq.empty, new Product(p, Omega) with Path))
      case Product(Omega, p @ Path(_, _)) =>
        Some((Seq.empty, new Product(Omega, p) with Path))
      case Arrow(src, Path(srcs, tgt)) => Some((src +: srcs, tgt))
      case _                           => None
    }
}

/** Helper methods to organize types. */
object Organized {

  /** Appends to sequences of paths. */
  final def addPaths(
      xs: Seq[Type with Path],
      ys: Seq[Type with Path]
  ): Seq[Type with Path] =
    xs ++ ys

  /** Organizes any type into an intersection of paths. */
  final def apply(t: Type): Type with Organized =
    t match {
      case ot: Organized => ot
      case Constructor(name, arg) if arg.isOmega =>
        new Constructor(name, Omega) with Path
      case Constructor(name, arg) =>
        intersect(
          Organized(arg).paths.map(p => new Constructor(name, p) with Path)
        )
      case Product(Omega, Omega) =>
        new Product(Omega, Omega) with Path
      case Product(sigma, tau) =>
        intersect(
          Organized(sigma).paths.map(p => new Product(p, Omega) with Path),
          Organized(tau).paths.map(p => new Product(Omega, p) with Path)
        )
      case Arrow(src, tgt) =>
        intersect(
          Organized(tgt).paths.map(tgt => new Arrow(src, tgt) with Path)
        )
      case Intersection(sigma, tau) =>
        intersect(Organized(sigma).paths, Organized(tau).paths)
    }

  /** Builds an organized type out of an intersection of paths. */
  final def intersect(
      pathss: Seq[Type with Organized with Path]*
  ): Type with Organized = {
    val allPaths = pathss.view.flatten
    if (allPaths.isEmpty) Omega
    else {
      allPaths.tail.foldLeft[Type with Organized](allPaths.head)(
        (result, path) =>
          new Intersection(path, result) with Organized {
            val paths = path +: result.paths
          }
      )
    }
  }

  /** Computes the piecewise intersection of the arguments in the given sequences.
    * Assumes the sequences have the same length.
    * Example:
    * <code>
    *   intersectPiecewise(Seq('A, 'B), Seq('C, 'D)) = Seq('A :&: 'C, 'B :&: 'D)
    * </code>
    */
  def intersectPiecewise(
      xs: Seq[Type with Organized],
      ys: Seq[Type with Organized]
  ): Seq[Type with Organized] =
    xs.zip(ys).map {
      case (t1, t2) => intersect(t1.paths, t2.paths)
    }
}

/** Subtyping based on a taxonomy of type constructors.
  * @param taxonomicSubtypesOf the taxonomy, where each constructor name is mapped to its directly smaller successors.
  */
case class SubtypeEnvironment(taxonomicSubtypesOf: Map[String, Set[String]]) {

  /** Computes a transitive closure step of a taxonomy.
    * For `x, y, z` with `y` in `state(x)` and `z` in `state(y)` we have `z` in `transitiveClosureStep(state)._2(x)`.
    * @return the new taxonomy and a boolean indicating if any new entries had to be added.
    */
  final private def transitiveClosureStep(
      state: Map[String, Set[String]]
  ): (Boolean, Map[String, Set[String]]) = {
    state.foldLeft((false, state)) {
      case ((hasChanged, newState), (sigma, currentSubtypes)) =>
        val recursiveSubtypes =
          currentSubtypes.flatMap(state.getOrElse(_, Set.empty))
        val newSubtypes = currentSubtypes.union(recursiveSubtypes)
        val changedNow = currentSubtypes.size != newSubtypes.size
        if (changedNow) (true, newState + (sigma -> newSubtypes))
        else (hasChanged, newState)
    }
  }

  /** Computes the reflexive closure of a taxonomy.
    * For any `x`, `x` is in `reflexiveClosure(state)(x)`.
    */
  final private def reflexiveClosure(
      state: Map[String, Set[String]]
  ): Map[String, Set[String]] =
    state
      .map {
        case (sigma, taus) => (sigma, taus + sigma)
      }
      .withDefault(x => Set(x))

  /** The reflexive transitive closure of the taxonomy passed in the constructor. */
  lazy private val closedEnvironment: Map[String, Set[String]] = {
    var updatedState = (true, taxonomicSubtypesOf)
    while (updatedState._1) {
      updatedState = transitiveClosureStep(updatedState._2)
    }
    reflexiveClosure(updatedState._2)
  }

  /** Functional representation of the taxonomy under reflexive transitive closure. */
  lazy val transitiveReflexiveTaxonomicSubtypesOf: String => Set[String] =
    closedEnvironment.apply

  /** Type class to make types (subtype-)comparable. */
  sealed trait TypeRelationOf {
    def isSupertypeOf(tau: Type): Boolean
    def isSubtypeOf(tau: Type): Boolean
  }

  /** Extract immediate children in the syntax tree of types and filters them by relevance for subtype comparison. */
  object cast {
    def apply(to: Omega.type, from: Type): Seq[Type] = Seq(Omega)
    def apply(to: Arrow, from: Type): Seq[(Type, Type)] =
      if (to.target.isOmega) Seq((Omega, Omega))
      else {
        def castRec(from: Type, ctxt: Seq[(Type, Type)]): Seq[(Type, Type)] =
          from match {
            case Arrow(src, tgt)          => (src, tgt) +: ctxt
            case Intersection(sigma, tau) => castRec(sigma, castRec(tau, ctxt))
            case _                        => ctxt
          }
        castRec(from, Seq.empty)
      }
    def apply(to: Constructor, from: Type): Seq[Type] = {
      def castRec(from: Type, ctxt: Seq[Type]): Seq[Type] =
        from match {
          case Constructor(name, arg)
              if transitiveReflexiveTaxonomicSubtypesOf(to.name)(name) =>
            arg +: ctxt
          case Intersection(sigma, tau) => castRec(sigma, castRec(tau, ctxt))
          case _                        => ctxt
        }
      castRec(from, Seq.empty)
    }
    def apply(to: Product, from: Type): Seq[(Type, Type)] = {
      def castRec(from: Type, ctxt: Seq[(Type, Type)]): Seq[(Type, Type)] =
        from match {
          case Product(sigma, tau)      => (sigma, tau) +: ctxt
          case Intersection(sigma, tau) => castRec(sigma, castRec(tau, ctxt))
          case _                        => ctxt
        }
      castRec(from, Seq.empty)
    }
  }

  private final def checkSubtypes(subType: Type, superType: Type): Boolean = {
    def tgtForSrcs(gte: Type, in: Seq[(Type, Type)]): Seq[Type] =
      in.collect { case (src, tgt) if checkSubtypes(gte, src) => tgt }

    superType match {
      case Omega => true
      case ctor @ Constructor(_, arg) =>
        val casted = cast(ctor, subType)
        casted.nonEmpty && checkSubtypes(Type.intersect(casted), arg)
      case arr @ Arrow(src, tgt) =>
        tgt.isOmega || checkSubtypes(
          Type.intersect(tgtForSrcs(src, cast(arr, subType))),
          tgt
        )
      case p @ Product(tau1, tau2) =>
        val (sigmas1, sigmas2) = cast(p, subType).unzip
        sigmas1.nonEmpty && checkSubtypes(Type.intersect(sigmas1), tau1) && checkSubtypes(
          Type.intersect(sigmas2),
          tau2
        )
      case Intersection(tau1, tau2) =>
        checkSubtypes(subType, tau1) && checkSubtypes(subType, tau2)
      case _ => false
    }
  }

  /** Instance of the subtype relation type class which operates on casted types */
  implicit class toTypeRelationOf(sigma: Type) extends TypeRelationOf {
    override def isSubtypeOf(tau: Type): Boolean = checkSubtypes(sigma, tau)
    override def isSupertypeOf(tau: Type): Boolean = checkSubtypes(tau, sigma)
  }

  object Minimizable {
    type Aux[Ty <: Type] = Minimizable { type T = Ty }
  }

  /** Typeclass instance to minimize a type collection wrt. to its path cardinality under the constraint
    * that the intersected initial type collection is subtype equal to the intersected result.
    * Example:
    * <code>
    *   Seq('A :&: 'B =>: 'C :&: 'D, 'A =>: 'C) = Set('A =>: 'C, 'A :&: 'B =>: 'D)
    * </code>
    */
  implicit class MinimalPathSet(tys: Seq[Type]) extends Minimizable {
    type T = Type with Path
    final def minimize: Seq[T] =
      tys.view.flatMap(Organized(_).paths).foldLeft(Seq.empty[T]) {
        case (result, path) if result.exists(_.isSubtypeOf(path)) => result
        case (result, path)                                       => path +: result.filterNot(_.isSupertypeOf(path))
      }
  }
}

/** Taxonomies are relations between constructor names.
  * For a taxonomy t we have `'C <= 'D` if `"D"` is in `t("C")`.
  * Subtyping will arange the transitive reflexive closure of taxonomies.
  */
sealed trait Taxonomy extends (String => Set[String]) {

  /** The finite map representation of this taxonomy */
  val underlyingMap: Map[String, Set[String]]

  /** Merges this taxonomy with `other` and return a new taxonomy, containing the entries of both. */
  def merge(other: Taxonomy): Taxonomy

  /** Merges this taxonomy with `other` and return a new taxonomy, containing the entries of both. */
  def merge(other: NonEmptyTaxonomy): NonEmptyTaxonomy

  /** Looks up a constructor name in this taxonomy, returning all directly related constructor names. */
  def apply(s: String): Set[String] = underlyingMap.getOrElse(s, Set.empty)
}

/** A non empty taxonomy with a marked root node */
sealed trait NonEmptyTaxonomy extends Taxonomy { self =>

  /** The marked root node */
  protected val head: String

  /** Adds a constructor name to the relation for the marked root node. */
  def addSubtype(entry: String): NonEmptyTaxonomy =
    new NonEmptyTaxonomy {
      val underlyingMap =
        self.underlyingMap.updated(self.head, self(self.head) + entry)
      val head: String = self.head
    }

  /** Adds multiple constructor names to the relation for the marked root node. */
  def addSubtypes(entries: NonEmptyTaxonomy): NonEmptyTaxonomy =
    new NonEmptyTaxonomy {
      val underlyingMap =
        self
          .merge(entries)
          .underlyingMap
          .updated(self.head, self(self.head) + entries.head)
      val head: String = self.head
    }

  /** Merges this taxonomy with `other` and return a new taxonomy, containing the entries of both.
    * Keeps the current root node.
    */
  override def merge(entries: Taxonomy): NonEmptyTaxonomy =
    new NonEmptyTaxonomy {
      val underlyingMap =
        entries.underlyingMap.foldLeft(self.underlyingMap) {
          case (m, (k, v)) => m.updated(k, m.getOrElse(k, Set.empty) ++ v)
        }
      val head: String = self.head
    }

  /** Merges this taxonomy with `other` and return a new taxonomy, containing the entries of both.
    * Keeps the current root node.
    */
  override def merge(entries: NonEmptyTaxonomy): NonEmptyTaxonomy =
    merge(entries.asInstanceOf[Taxonomy])
}

/** Helper to construct new taxonomies. */
object Taxonomy {

  /** Starts a new non-empty taxonomy for the constructor name `superType`. */
  def apply(superType: String): NonEmptyTaxonomy =
    new NonEmptyTaxonomy {
      val underlyingMap: Map[String, Set[String]] = Map.empty
      val head: String = superType
    }

  /** Returns a new empty taxonomy. */
  def empty: Taxonomy =
    new Taxonomy {
      val underlyingMap: Map[String, Set[String]] = Map.empty

      override def merge(other: Taxonomy) = other
      override def merge(other: NonEmptyTaxonomy) = other
    }
}
