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
import shapeless.feat.Enumeration
import scala.collection.mutable
import org.combinators.cls.compat.ParallelCollectionConverters._

/** A combinator expression.
  *
  * @param name the name of the combinator in head position.
  * @param arguments arguments of the combinator.
  */
case class Tree(name: String, target: Type, arguments: Tree*) {
  def equalsWithSubtypeEqualityIn(environment: SubtypeEnvironment, other: Tree): Boolean = {
    import environment._
    return name == other.name &&
      target.isSubtypeOf(other.target) && other.target.isSubtypeOf(target) &&
      arguments.corresponds(other.arguments)((t1, t2) => t1.equalsWithSubtypeEqualityIn(environment, t2))
  }

  def apply(newTarget: Type, argument: Tree): Tree =
    Tree(name, newTarget, arguments :+ argument :_*)
}

/** Enumeration of all combinator expressions derivable from a tree grammar.
  * Use [[TreeGrammarEnumeration.apply]] to obtain an enumeration for any given tree grammar.
  */
trait TreeGrammarEnumeration {
  /** The tree grammar rules to derive expressions from. */
  val fromRules: Set[Rule]

  /** Returns the rules grouped by their result type. */
  private lazy val groupedRules: Map[Type, Set[Rule]] = fromRules.groupBy(_.target)

  /** Updates an existing enumeration to include terms derivable by `rule`. */
  private final def addToEnumeration(enum: Enumeration[Tree], rule: Rule): Enumeration[Tree] =
    rule match {
      case Failed(_) => enum
      case Combinator(target, combinator) => enum.union(Enumeration.singleton(Tree(combinator, target)))
      case Apply(target, functionType, argumentType) =>
        lazy val functionTerms = enumerationMap(functionType)
        lazy val argumentTerms = enumerationMap(argumentType)
        functionTerms.product(argumentTerms).map {
          case (functionTerm, argument) => functionTerm(target, argument)
        }.pay
    }

  /** Enumerates all expressions for a type acting as the start symbol. */
  lazy val enumerationMap: Type => Enumeration[Tree] = {
    val cache = collection.concurrent.TrieMap[Type, Enumeration[Tree]]()
    entry =>
      cache
        .getOrElseUpdate(entry,
          groupedRules
            .getOrElse(entry, Set.empty)
            .par
            .aggregate[Enumeration[Tree]](Enumeration.empty)(addToEnumeration, (e1, e2) => e1.union(e2))
        )
  }
}

/** Helper object to obtain enumerations. */
object TreeGrammarEnumeration {
  /** Enumerates all combinator expressions for the tree grammar `grammar` and start symbol `root`.
    * Assumes that the grammar does not contain unproductive non-terminals:
    * {{{
    *   S --> @(X -> Y, X) | @(S -> S, S); S -> S --> f; X -> Y --> g; X --> A; Y --> B; Z --> C; is ok
    *   S --> @(X -> Y, X) | @(S -> S, S); S -> S --> f; X -> Y --> g; X --> A; Z --> C; is not ok, because Y is missing
    *   S --> @(S -> S, S); S -> S --> f; X -> Y --> g; X --> A; Y --> B; Z --> C; is not ok, because S is unproductive
    * }}}
    */
  def apply(rules: Set[Rule], root: Type): Enumeration[Tree] = {
    if (rules.par.forall(_.target != root)) Enumeration.empty
    else new TreeGrammarEnumeration { lazy val fromRules = rules }.enumerationMap(root)
  }
}
