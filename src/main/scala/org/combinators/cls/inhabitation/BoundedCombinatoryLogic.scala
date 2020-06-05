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

package org.combinators.cls.inhabitation

import org.combinators.cls.types._
import shapeless.feat.Finite

import scala.collection.SeqView

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
        case Constructor(c, argument) =>
          Constructor(c, subst(argument))
        case Arrow(src, tgt) =>
          Arrow(subst(src), subst(tgt))
        case Intersection(tau, rho) =>
          Intersection(subst(tau), subst(rho))
        case Product(tau, rho) =>
          Product(subst(tau), subst(rho))
        case v@Variable(_) => s(v)
      }
    subst(sigma)
  }

  /** Returns a set of all variables occuring in type `sigma`. */
  private def variablesInType(sigma: Type): Set[Variable] = {
    sigma match {
      case Omega => Set.empty
      case Constructor(c, argument) => variablesInType(argument)
      case Arrow(src, tgt) => variablesInType(src).union(variablesInType(tgt))
      case Intersection(sigma, tau) => variablesInType(sigma).union(variablesInType(tau))
      case Product(tau, rho) => variablesInType(tau).union(variablesInType(rho))
      case v@Variable(_) => Set(v)
    }
  }

  /** Computes a table of all valid substitutions of variables in `sigma`.
    * This avoids applying unneccessary duplicate substitutions to types, e.g.:
    * Gamma = { c: alpha -> x }
    * substitutions = {
    *   { alpha -> a, beta -> b },
    *   { alpha -> a, beta -> c },
    *   { alpha -> b, beta -> d }
    * }
    * Would result in
    * Gamma' = { c: (a -> x) & (a -> x) & (b -> x) }
    * without this function.
    * The generated substitution table is restricted in its domain:
    * substitutionTable(alpha -> x) = {
    *   { alpha -> a, alpha -> b}
    * }
    * which avoids the duplicate (a -> x) type in Gamma'.
    */
  private def substitutionTable(sigma: Type): Set[Map[Variable, Type]] = {
    val domain = variablesInType(sigma)
    substitutions.values.foldLeft(Set.empty[Map[Variable, Type]]) {
      case (res, s) => res + (domain.map(v => (v, s(v))).toMap)
    }
  }


  /** Applies all substitutions to every combinator type in `Gamma`. */
  private def blowUp(Gamma: => Repository): Repository = {
    if (substitutions.values.isEmpty) Gamma else {
      Gamma.transform((_, ty) =>
        if (ty.isClosed) ty
        else {
          val table = substitutionTable(ty)
          table.tail.foldLeft(applySubst(table.head)(ty)) {
            case (res, s) => Intersection(applySubst(s)(ty), res)
          }
        })
    }
  }

  /** The repository expanded by every allowed substitution. */
  lazy val repository: Repository = blowUp(Gamma)
  /** The algorithm used for inhabitation with the expanded repository. */
  lazy val algorithm: FiniteCombinatoryLogic = new FiniteCombinatoryLogic(subtypes, repository)

  /** Performs inhabitation of every type in targets */
  def inhabit(targets: Type*): Set[Rule] =
    algorithm.inhabit(targets: _*)
}

/** Provides a type inhabitation algorithm for bounded combinatory logic (BCL). */
object BoundedCombinatoryLogic {
  def algorithm: InhabitationAlgorithm = {
    case (substitutionSpace, subtypes, repository) =>
      targets => new BoundedCombinatoryLogic(substitutionSpace, subtypes, repository).inhabit(targets: _*)
  }
}
