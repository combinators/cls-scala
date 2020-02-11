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

import org.scalatest._
import org.combinators.cls.types._
import org.combinators.cls.inhabitation.{Failed, Combinator, Apply}
import org.combinators.cls.TestCompat._

class PruneTest extends FunSpec {

  val garbageCombinators =
    Map(
      "f" ->
        Arrow(
          Constructor("Int"),
          Constructor("Goal")
        ),
      "x" -> Constructor("Int"),
      "garbage1" ->
        Arrow(
          Constructor("Garbage1"),
          Intersection(Constructor("Int"), Constructor("Garbage2"))
        ),
      "garbage2" ->
        Arrow(
          Constructor("Garbage2"),
          Constructor("Garbage1")
        )
    )

  val taxonomy: Taxonomy =
    Taxonomy("Int")
      .merge(Taxonomy("Garbage1"))
      .merge(Taxonomy("Garbage2"))
      .merge(Taxonomy("Goal"))

  val Gamma = new FiniteCombinatoryLogic(SubtypeEnvironment(taxonomy.underlyingMap), garbageCombinators)

  describe(Gamma.toString) {
    describe("|- ? : Goal") {
      val tgt = Constructor("Goal")
      val results = Gamma.inhabit(tgt)
      it("should contain only f(x)") {
        assert(results.forall(rule =>
            (rule.target != Constructor("Goal") || 
              rule == Apply(Constructor("Goal"), Arrow(Constructor("Int"), Constructor("Goal")), Constructor("Int"))) &&
            (rule.target != Constructor("Int") ||
              rule == Combinator(Constructor("Int"), "x"))
            ))
      }
      it("should only unroll to Tree(f, Tree(x))") {
        assert(
          TreeGrammarEnumeration(results, tgt).values ==
            (BigInt(0), LazyList.empty[Tree]) #::
              (BigInt(1), Tree("f", Constructor("Goal"), Tree("x", Constructor("Int"))) #:: LazyList.empty[Tree]) #::
              LazyList.empty[(BigInt, LazyList[Tree])]
        )
      }
    }
    describe("|- ? : Garbage1") {
      val tgt = Constructor("Garbage1")
      val results = Gamma.inhabit(tgt)
      it("should be empty") {
        assert(results.forall(rule => rule.target != tgt || rule == Failed(tgt)))
      }
      it("should unroll to an empty enumeration") {
        assert(TreeGrammarEnumeration(results, tgt).values.isEmpty)
      }
    }
  }

}
