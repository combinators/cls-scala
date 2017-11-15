package de.tu_dortmund.cs.ls14.cls.inhabitation

import de.tu_dortmund.cs.ls14.cls.types._
import shapeless.feat.Finite

/** Type inhabitation for bounded combinatory logic (BCL). */
class BoundedCombinatoryLogic(substitutionSpace: FiniteSubstitutionSpace, subtypes: SubtypeEnvironment, Gamma: Repository) {

  import subtypes._

  /** All allowed substitutions. */
  lazy val substitutions: Finite[Variable => Type] = substitutionSpace.allowedSubstitutions

  /** Applies a substitution to a type. */
  private def applySubst(s: => Variable => Type)(sigma: Type): Type = {
    def subst(sigma: => Type): Type =
      sigma match {
        case Omega => Omega
        case Constructor(c, arguments@_*) =>
          Constructor(c, arguments.map(subst(_)): _*)
        case Arrow(src, tgt) =>
          Arrow(subst(src), subst(tgt))
        case Intersection(tau, rho) =>
          Intersection(subst(tau), subst(rho))
        case v@Variable(_) => s(v)
      }
    subst(sigma)
  }

  /** Applies all substitutions of `kinding` to type `sigma`. */
  private def blowUp(sigma: => Type): Finite[Stream[Type with Path]] =
    if (substitutions.values.isEmpty) Finite.singleton(Organized(sigma).paths)
    else substitutions.map { s => Organized(applySubst(s)(sigma)).paths }

  /** Applies all substitutions of `kinding` to every combinator type in `Gamma`. */
  private def blowUp(Gamma: => Repository): Repository = Gamma.mapValues { ty =>
    val paths =
      blowUp(ty).values.foldLeft[Stream[Type with Path]](Stream.empty) {
        case (s, ps) =>
          ps.foldLeft(s) {
            case (newPaths, p) if !newPaths.exists(_.isSubtypeOf(p)) => newPaths :+ p
            case (newPaths, _) => newPaths
          }
      }
    Organized.intersect(paths)
  }

  /** The repository expanded by every substitution in `kinding`. */
  lazy val repository: Repository = blowUp(Gamma)
  /** The algorithm used for inhabitation with the expanded repository. */
  lazy val algorithm: FiniteCombinatoryLogic = new FiniteCombinatoryLogic(subtypes, repository)

  /** Performs inhabitation of every type in targets */
  def inhabit(targets: Type*): TreeGrammar =
    algorithm.inhabit(targets: _*)
}

/** Provides a type inhabitation algorithm for bounded combinatory logic (BCL). */
object BoundedCombinatoryLogic {
  def algorithm: InhabitationAlgorithm = {
    case (substitutionSpace, subtypes, repository) =>
      targets => new BoundedCombinatoryLogic(substitutionSpace, subtypes, repository).inhabit(targets: _*)
  }
}