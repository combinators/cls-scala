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

  /** Applies all substitutions to every combinator type in `Gamma`. */
  private def blowUp(Gamma: => Repository): Repository = {
    if (substitutions.values.isEmpty) Gamma else {
      Gamma.mapValues(ty =>
        if (ty.isClosed) ty
        else  substitutions.values.tail.foldLeft(applySubst(substitutions.values.head)(ty)) {
            case (res, s) => Intersection(applySubst(s)(ty), res)
          })
    }
  }

  /** The repository expanded by every allowed substitution. */
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