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

package org.combinators.cls

import org.combinators.cls.types._

package object inhabitation {
  /** Repositories map combinator names to types. */
  type Repository = Map[String, Type]

  /** Rules describe how to form inhabitants from combinators. */
  sealed trait Rule {
    /** Returns the inhabitation target of this rule. */
    val target: Type
  }
  /** Represents uninhabited types. */
  final case class Failed(target: Type) extends Rule
  /** Represents types inhabited by a combinator without arguments. */
  final case class Combinator(target: Type, combinator: String) extends Rule
  /** Represents the application of a term of type `functionType` to a term of type `argumentType` to obtain
    * type `target`. */
  final case class Apply(target: Type, functionType: Type, argumentType: Type) extends Rule


  /** Returns a prettified String representation of the given rule set */
  def prettyPrintRuleSet(rules: Set[Rule]): String = {
    rules.groupBy(_.target).map {
      case (target, entries) =>
        val prettyEntries = entries.map {
          case Failed(_) => "Uninhabited"
          case Combinator(_, combinator) => combinator
          case Apply(_, functionType, argumentType) => s"@($functionType, $argumentType)"
        }
        s"$target --> ${prettyEntries.mkString(" | ")}"
    }.mkString("{", "; ", "}")
  }

  /** Returns a prettified String representation of the given (ordered) list of rules */
  def prettyPrintRuleList(rules: Seq[Rule]): String = {
    rules.map {
      case Failed(target) => s"$target --> Uninhabited"
      case Combinator(target, combinator) => s"$target --> $combinator"
      case Apply(target, functionType, argumentType) => s"$target --> @($functionType, $argumentType)"
    }.mkString("[", "; ", "]")
  }

  /** For a finitite substitution space, a subtype environment and a repository `Gamma`, an
    * inhabitation algorithm takes a sequence of types and returns a set of rules for a tree grammar, describing all
    * inhabitants of these types.
    * We have for all `i &leq; k`:
    * `Gamma |- M : tau_i` iff M is a word of the language of the grammar returned by the algorithm applied to
    * `Seq(tau_1, tau_2, ..., tau_k)`.
    */
  type InhabitationAlgorithm =
    (FiniteSubstitutionSpace, SubtypeEnvironment, Repository) => Seq[Type] => Set[Rule]
}
