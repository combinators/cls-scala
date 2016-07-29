package de.tu_dortmund.cs.ls14.cls.inhabitation

import de.tu_dortmund.cs.ls14.cls.types._
import shapeless.feat.Enumeration

class BoundedCombinatoryLogic(kinding: Kinding, subtypes: SubtypeEnvironment, Gamma: Repository) {
  import subtypes._

  private lazy val substitutions: Enumeration[Variable => Type] = {
    lazy val varMappings = kinding.underlyingMap.toStream.map {
      case (v, e) => e.map((v -> _))
    }
    if (varMappings.isEmpty) Enumeration.empty
    else {
      varMappings.tail.foldLeft(varMappings.headOption.getOrElse(Enumeration.empty).map(Map(_))) {
        case (substs, e) => substs.product(e).map {
          case (subst, vt) => subst + vt
        }
      }
    }
  }

  private def applySubst(s: => Variable => Type)(sigma: Type): Type = {
    def subst(sigma: => Type): Type =
      sigma match {
        case Omega => Omega
        case Constructor(c, arguments@ _*) =>
          Constructor(c, arguments.map(subst(_)) : _*)
        case Arrow(src, tgt) =>
          Arrow(subst(src), subst(tgt))
        case Intersection(sigma, tau) =>
          Intersection(subst(sigma), subst(tau))
        case v@Variable(_) => s(v)
      }
    subst(sigma)
  }

  private def blowUp(sigma: => Type): Enumeration[Stream[Type with Path]] =
    if (substitutions.values.isEmpty) Enumeration.singleton(sigma match { case Organized(ps) => ps.toStream })
    else substitutions.map { s => applySubst(s)(sigma) match { case Organized(ps) => ps.toStream } }

  private def blowUp(Gamma: => Repository): Repository = {
    Gamma.mapValues { case ty =>
      val paths =
        blowUp(ty).values.foldLeft[Stream[Type with Path]](Stream.empty) {
          case (s, (_, ps)) =>
            ps.flatten.foldLeft(s) {
              case (newPaths, p) if !newPaths.exists(_.isSubtype(p)) => newPaths :+ p
              case (newPaths, _) => newPaths
            }
        }
      Organized.intersect(paths)
    }
  }

  lazy val repository = blowUp(Gamma)
  lazy val algorithm = new FiniteCombinatoryLogic(subtypes, repository)

  def inhabit(target: Type): TreeGrammar =
    algorithm.inhabit(target)
}


object BoundedCombinatoryLogic {
  def algorithm: InhabitationAlgorithm = {
    case (kinding, subtypes, repository) =>
      target => new BoundedCombinatoryLogic(kinding, subtypes, repository).inhabit(target)
  }
}