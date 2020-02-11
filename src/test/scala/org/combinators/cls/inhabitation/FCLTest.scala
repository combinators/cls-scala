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

class FCLTest extends FunSpec {

  val naturalNumbers =
    Map(
      "Succ" -> Intersection(
        Arrow(
          Intersection(Constructor("Int"), Constructor("Even")),
          Intersection(Constructor("Int"), Constructor("Odd"))
        ),
        Arrow(
          Intersection(Constructor("Int"), Constructor("Odd")),
          Intersection(Constructor("Int"), Constructor("Even"))
        )),
      "Zero" -> Intersection(Constructor("Int"), Constructor("Even"))
    )

  val taxonomy: Taxonomy =
    Taxonomy("Char")
    .addSubtype("Int")

  val Gamma = new FiniteCombinatoryLogic(SubtypeEnvironment(taxonomy.underlyingMap), naturalNumbers)

  describe(Gamma.toString) {
    describe("|- ? : Int") {
      import Gamma.subtypes._
      val tgt = Constructor("Int")
      val results = Gamma.inhabit(tgt)
      it("should not be empty") {
        assert(results.nonEmpty)
      }
      it("should contain Zero()") {
        assert(results.filter(_.target == tgt).contains(Combinator(tgt, "Zero")))
      }
      it("should contain Succ(Int & Even)") {
        assert(results.contains(Apply(tgt, Arrow(Intersection(tgt, Constructor("Even")), tgt), Intersection(tgt, Constructor("Even")))))
        assert(results.contains(Combinator(Arrow(Intersection(tgt, Constructor("Even")), tgt), "Succ")))
      }
      it("should unroll to Tree(Zero) at index 0") {
        assert(
          TreeGrammarEnumeration(results, Constructor("Int")).index(0)
            .equalsWithSubtypeEqualityIn(SubtypeEnvironment(taxonomy.underlyingMap),
              Tree("Zero", Constructor("Int")))
        )
      }
      it("should unroll to Tree(Succ, Tree(Succ, Tree(Succ, Tree(Succ, Tree(Zero))))) at index 4") {
        assert(
          TreeGrammarEnumeration(results, Constructor("Int")).index(4)
            .equalsWithSubtypeEqualityIn(
              SubtypeEnvironment(taxonomy.underlyingMap),
              Tree("Succ", Constructor("Int"), Tree("Succ", Intersection(Constructor("Odd"),Constructor("Int")) , Tree("Succ", Intersection(Constructor("Even"),Constructor("Int")), Tree("Succ", Intersection(Constructor("Odd"),Constructor("Int")), Tree("Zero", Intersection(Constructor("Even"),Constructor("Int"))))))))
        )
      }
    }

    describe("|- ? : List(Int)") {
      val tgt = Constructor("List", Constructor("Int"))
      val results = Gamma.inhabit(tgt)
      it("should be empty") {
        assert(results.forall(rule => rule.target != tgt || rule == org.combinators.cls.inhabitation.Failed(tgt)))
      }
    }

    describe("|- ? : Char") {
      val tgt = Constructor("Char")
      val tgt2 = Constructor("Int")
      val results = Gamma.inhabit(tgt)
      val results2 = Gamma.inhabit(tgt2)
      it("should equal the results for Int plus an entry for Char") {
        def targetsChar(t: Type): Boolean = 
          t match {
            case Arrow(_, t) =>  targetsChar(t)
            case x => x == tgt
          }
        def replaceArrowTarget(arr: Type): Type = 
          arr match {
            case Arrow(src, target) => Arrow(src, replaceArrowTarget(target))
            case x => tgt2
          }
        assert(
          results.map {
            case org.combinators.cls.inhabitation.Failed(target) if targetsChar(target) => org.combinators.cls.inhabitation.Failed(replaceArrowTarget(target))
            case Combinator(target, c) if targetsChar(target) => Combinator(replaceArrowTarget(target), c)
            case Apply(target, fun, arg) if targetsChar(target) => Apply(replaceArrowTarget(target), replaceArrowTarget(fun), arg)
            case x => x
          } == results2)
      }
    }


  }
}
