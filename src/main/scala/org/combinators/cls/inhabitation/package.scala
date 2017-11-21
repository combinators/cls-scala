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
  /** Repositories map combinator names to types */
  type Repository = Map[String, Type]

  /** Tree Grammars map types to sets of combinators and recursive targets, such that the combinator, when applied to
    * arguments of the recursive target types, returns a value of the type on the left hand side
    */
  type TreeGrammar = Map[Type, Set[(String, Seq[Type])]]

  /** Returns a prettified String representation of the given tree grammar */
  def prettyPrintTreeGrammar(grammar: TreeGrammar): String = {
    grammar.map {
      case (ty, entries) =>
        val prettyEntries = entries.map {
          case (c, tgts) => s"$c${tgts.mkString("(", ",", ")")}"
        }
        s"$ty -> ${prettyEntries.mkString(" | ")}"
    }.mkString("{", "; ", "}")
  }

  /** For a finitite substitution space, a subtype environment and a repository `Gamma`, an
    * inhabitation algorithm takes a sequence of types and returns a tree grammar, describing all inhabitants of these
    * types.
    * We have for all `i &leq; k`:
    * `Gamma |- M : tau_i` iff M is a word of the language of the grammar returned by the algorithm applied to
    * `Seq(tau_1, tau_2, ..., tau_k)`.
    */
  type InhabitationAlgorithm =
    (FiniteSubstitutionSpace, SubtypeEnvironment, Repository) => Seq[Type] => TreeGrammar
}
