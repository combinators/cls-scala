package de.tu_dortmund.cs.ls14.cls.inhabitation

import de.tu_dortmund.cs.ls14.cls.types._
import shapeless.feat.Enumeration

class BoundedCombinatoryLogic(kinding: Kinding, subtypes: SubtypeEnvironment, Gamma: Repository) {
  private lazy val substitutions: Enumeration[Variable => Type] = {
    lazy val varMappings = kinding.underlyingMap.map {
      case (v, e) => e.map((v -> _))
    }
    varMappings.tail.foldLeft(varMappings.headOption.getOrElse(Enumeration.empty).map(Map(_))) {
      case (substs, e) => substs.product(e).map {
        case (subst, vt) => subst + vt
      }
    }.pay
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

  private def blowUp(sigma: => Type): Enumeration[Type] =
    substitutions.map { s => applySubst(s)(sigma) }

  private def blowUp(Gamma: => Repository): Repository =
    Gamma.mapValues(ty => blowUp(ty).values.foldLeft[Type](Omega){
      case (s, (_, tys)) => tys.foldLeft(s){
        case (s, ty) => Intersection(ty, s)
      }
    })

  lazy val repository = blowUp(Gamma)
  lazy val algorithm = new FiniteCombinatoryLogic(subtypes, repository)

  def inhabit(target: Type): TreeGrammar =
    algorithm.inhabit(target)
}
