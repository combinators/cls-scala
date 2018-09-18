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
import shapeless.feat.Enumeration
import scala.collection.mutable

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
}

/** Enumeration of all combinatory expressions derivable from a [[TreeGrammar]].
  * Use [[TreeGrammarEnumeration.apply]] to obtain an enumeration for any given `TreeGrammar`.
  */
trait TreeGrammarEnumeration {
  /** The tree grammar to derive expressions from. */
  val fromGrammar: TreeGrammar

  /** Enumerates all expressions for a type acting as the start symbol. */
  lazy val enumerationMap: Type => Enumeration[Tree] =
    new mutable.HashMap[Type, Enumeration[Tree]] {
      override def apply(entry: Type): Enumeration[Tree] =
        getOrElseUpdate(entry, {
          fromGrammar.getOrElse(entry, Seq.empty).foldLeft[Enumeration[Tree]](Enumeration.empty) { case (enum, (combinator, args)) =>
            val argEnum =
              args.foldRight[Enumeration[Seq[Tree]]](Enumeration.singleton(Seq.empty)) { (arg, enum) =>
                val recArgs = enumerationMap(arg).pay
                enum.product(recArgs).map { case (args, arg) => arg +: args }
              }
            val terms = argEnum.map((args: Seq[Tree]) => Tree(combinator, entry, args: _*)).pay
            enum.union(terms)
          } }
        )
    }
}

/** Helper object to obtain enumerations. */
object TreeGrammarEnumeration {
  /** Enumerates all combinator expressions for the tree grammar `grammar` and start symbol `root`.
    * Assumes that the grammar does not contain unproductive non-terminals:
    * {{{
    *   S -> f(X, Y) | f(S); X -> A(); Y -> B(); Z -> C(); is ok
    *   S -> f(X, Y) | f(S); X -> A(); Z -> C(); is not ok, because Y is missing
    *   S -> f(S); X -> A(); Y -> B(); Z -> C(); is not ok, because S is unproductive
    * }}}
    */
  def apply(grammar: TreeGrammar, root: Type): Enumeration[Tree] = {
    if (!grammar.contains(root)) Enumeration.empty
    else new TreeGrammarEnumeration { lazy val fromGrammar = grammar }.enumerationMap(root)
  }
}
