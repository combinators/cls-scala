package de.tu_dortmund.cs.ls14.cls.types

/** A path `p` conforms to the syntax:
  * <code>
  *   p ::= 'C() | 'C((Omega, )* p (, Omega)*) | sigma_1 =>: ... =>: sigma_k =>: p
  * </code>
  * where each `sigma_i` is an arbitrary intersection type without variables.
  */
sealed trait Path extends Organized { self: Type =>
  final val paths: Stream[Type with Path] = this #:: Stream.empty[Type with Path]
}

/** A type is organized iff it is syntactically identical to an intersection of paths. */
trait Organized { self: Type =>
  val paths: Stream[Type with Path]
}

/** Helper methods to (de-)construct paths from (/into) arguments and targets of arrows. */
object Path {
  /** If possible, deconstructs a path `t` into a constructor, which is a path, and arrow parameters.
    * <code>
    *   unapply('A =>: 'B :&: 'C =>: 'D) = Some(Seq('A, 'B :&: 'C), 'D)
    *   unapply('A =>: 'B :&: 'C) = None
    * </code>
    */
  def unapply(t: Type): Option[(Seq[Type], Constructor with Path)] =
    t match {
      case c : Constructor with Path => Some((Seq.empty, c))
      case Constructor(name) => Some((Seq.empty, new Constructor(name) with Path))
      case Constructor(name, args @ _*) =>
        args.dropWhile(_ == Omega) match {
          case Path(_, _) +: rest =>
            rest.dropWhile(_ == Omega) match {
              case Seq() => Some((Seq(), new Constructor(name, args: _*) with Path))
              case _ => None
            }
          case Seq() => Some((Seq(), new Constructor(name, args: _*) with Path))
        }
      case Arrow(src, Path(srcs, tgt)) => Some((src +: srcs, tgt))
      case _ => None
    }

  /** Constructs a path ending in `target` and taking `args` as arrow parameters. */
  def apply(args: Seq[Type] = Seq.empty, target: Constructor with Path): Type with Path =
    args.foldRight[Type with Organized with Path](target) {
      case (arg, result) => new Arrow(arg, result) with Path
    }
}

/** Helper methods to organize types. */
object Organized {
  /** Checks, if a type is (subtype-)equal to Omega */
  private def isOmega(ty: Type): Boolean =
    ty match {
      case Omega => true
      case Arrow(_, tgt) => isOmega(tgt)
      case Intersection(l, r) => isOmega(l) && isOmega(r)
      case _ => false
    }

  /** Appends to sequences of paths. */
  final def addPaths(xs: Seq[Type with Path], ys: Seq[Type with Path]): Seq[Type with Path] =
    xs.toStream.append(ys.toStream)

  /** Organizes any type into an intersection of paths. */
  final def apply(t: Type): Type with Organized =
    t match {
      case ot : Organized => ot
      case Constructor(name) => new Constructor(name) with Path
      case Constructor(name, args @ _ *) if args.forall(isOmega) =>
        new Constructor(name, Seq.fill(args.size)(Omega): _*) with Path
      case Constructor(name, args @ _*) =>
        intersect(args.map(Organized(_)).zipWithIndex.flatMap {
            case (orgArg, argNo) =>
              orgArg.paths.map (orgArg => {
                val argVect = Stream.tabulate(args.size) {
                  case n if n == argNo => orgArg
                  case _ => Omega
                }
                new Constructor(name, argVect: _*) with Path
              })
          })
      case Arrow(src, tgt) =>
        intersect(Organized(tgt).paths map (tgt => new Arrow(src, tgt) with Path))
      case Intersection(sigma, tau) =>
        intersect(Organized(sigma).paths.append(Organized(tau).paths))
    }

    /** Builds an organized type out of an intersection of paths. */
    final def intersect(paths: Seq[Type with Organized with Path]): Type with Organized =
      paths match {
        case Seq() => Omega
        case _ =>
          paths.toStream.reduce[Type with Organized] {
            case (p1, p2) => new Intersection(p1, p2) with Organized {
              val paths = p1.paths.append(p2.paths)
            }
          }
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
  final private def transitiveClosureStep(state: Map[String, Set[String]]): (Boolean, Map[String, Set[String]]) = {
    state.foldLeft((false, state)) {
      case ((hasChanged, newState), (sigma, currentSubtypes)) =>
        val recursiveSubtypes = currentSubtypes.flatMap(state.getOrElse(_, Set.empty))
        val newSubtypes = currentSubtypes.union(recursiveSubtypes)
        val changedNow = currentSubtypes.size != newSubtypes.size
        if (changedNow) (true, newState + (sigma -> newSubtypes))
        else (hasChanged, newState)
    }
  }

  /** Computes the reflexive closure of a taxonomy.
    * For any `x`, `x` is in `reflexiveClosure(state)(x)`.
    */
  final private def reflexiveClosure(state: Map[String, Set[String]]): Map[String, Set[String]] =
    state.map {
      case (sigma, taus) => (sigma, taus + sigma)
    }.withDefault(x => Set(x))

  /** The reflexive transtivie closure of the taxonomy passed in the constructor. */
  lazy private val closedEnvironment: Map[String, Set[String]] =
    reflexiveClosure(
      Stream.iterate[(Boolean, Map[String, Set[String]])]((true, taxonomicSubtypesOf))(x => transitiveClosureStep(x._2))
        .dropWhile(_._1)
        .head
        ._2)


  /** Functional representation of the taxonomy under reflexive transitive closure. */
  lazy val transitiveReflexiveTaxonomicSubtypesOf: String => Set[String] = closedEnvironment.apply

  /** Type class to make types (subtype-)comparable */
  sealed trait TypeRelationOf {
    def isSupertypeOf(tau: Type): Boolean
    def isSubtypeOf(tau: Type): Boolean
  }

  /** Type comparison for decompsed paths */
  sealed private class SupertypesOfPath(pathArgs: Seq[Type], tgt: Constructor) {
    /** All constructor names subtype-related of the target constructor */
    private lazy val tgtSubs = transitiveReflexiveTaxonomicSubtypesOf(tgt.name)

    /** Checks, if another decomposed path `p'` can possibly be subtype-related to the path `p` given to the constructor:
      * is it possible, that `p' <= p`?
      * For this, parameter counts must be equal, `p` and `p'` must end in a subtype-related constructors and
      * all parameters of `p'` must be greater or equal to those of `p`.
      */
    private def relevant(subArgs: Seq[Type], subTgt: Constructor): Boolean =
      (subArgs.size == pathArgs.size) &&
        tgtSubs(subTgt.name) &&
        subArgs.par.zip(pathArgs.par).forall {
          case (subArg, pathArg) => subArg.isSupertypeOf(pathArg)
        }

    /** Test, if the path given to the constructor is greater or equal to the intersection of `taus`. */
    def isSuperTypeOf(taus: Seq[(Seq[Type], Constructor)]): Boolean = {
      taus
        .toStream
        .filter { case (argsTau, tgtTau) => relevant(argsTau, tgtTau) } match {
          case Seq() => false
          case tauPaths =>
            tauPaths.map(_._2.arguments) match {
              case Seq() => true
              case argss@(_ +: _) =>
                argss.reduce[Seq[Type]] {
                    case (args1, args2) =>
                      args1.zip(args2).map { case (arg1, arg2) => Intersection(arg1, arg2) }
                  }
                .zip(tgt.arguments)
                .forall { case (arg, tgtArg) => tgtArg.isSupertypeOf(arg) }
            }
        }

    }
  }

  /** Instance of the subtype relation type class. */
  implicit class toTypeRelationOf(sigma: Type) extends TypeRelationOf {
    def isSupertypeOf(tau: Type): Boolean = {
      val organizedTau =
        Organized(tau).paths.map {
          case Path(args, tgt) => (args, tgt)
        }

      Organized(sigma).paths match {
        case paths@(_ #:: _) =>
          paths.forall {
            case Path(srcs, tgt) => new SupertypesOfPath(srcs, tgt).isSuperTypeOf(organizedTau)
          }
        case _ => true
      }
    }
    def isSubtypeOf(tau: Type): Boolean =
      toTypeRelationOf(tau).isSupertypeOf(sigma)
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
      val underlyingMap = self.underlyingMap.updated(self.head, self(self.head) + entry)
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