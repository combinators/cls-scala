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
import org.combinators.cls.inhabitation._
import syntax._
import org.scalatest.funspec.AnyFunSpec
import shapeless.feat.Enumeration
import scala.language.experimental

class TreeGrammarEnumerationTest extends AnyFunSpec {

  def production(
      from: Type,
      to: (String, Seq[Type])*
  ): (Type, Set[(String, Seq[Type])]) =
    (from -> to.toSet)

  val A = Constructor("A")
  val B = Constructor("B")
  val C = Constructor("C")
  val D = Constructor("D")
  val E = Constructor("E")
  val testGrammar: Set[Rule] =
    Set[Rule](
      Combinator(A, "F"),
      Apply(A, Arrow(A, A), A),
      Apply(Arrow(A, A), Arrow(A, Arrow(A, A)), A),
      Combinator(Arrow(A, Arrow(A, A)), "G"),
      Apply(A, Arrow(B, A), B),
      Apply(Arrow(B, A), Arrow(A, Arrow(B, A)), A),
      Combinator(Arrow(A, Arrow(B, A)), "H"),
      Combinator(B, "I"),
      Apply(B, Arrow(A, B), A),
      Combinator(Arrow(A, B), "J"),
      Combinator(C, "K"),
      Failed(D),
      Combinator(E, "L"),
      Apply(E, Arrow(E, E), E),
      Combinator(Arrow(E, E), "M")
    )

  describe(s"Unrolling {${prettyPrintRuleSet(testGrammar)}}") {
    describe(s"Starting with ${D}") {
      it("should yield an empty enumeration") {
        assert(TreeGrammarEnumeration(testGrammar, D).values.isEmpty)
      }
    }

    describe(s"Starting with  ${C}") {
      it(s"should only yield ${Tree("K", C)}") {
        assert(
          TreeGrammarEnumeration(testGrammar, C).values.flatMap(_._2) ==
            Enumeration.singleton(Tree("K", C)).values.flatMap(_._2)
        )
      }
    }

    describe(s"Starting with ${E}") {
      it(
        s"should yield (${Tree("L", E)}, ${Tree("M", E, Tree("L", E))}, ${Tree("M", E, Tree("M", E, Tree("L", E)))}, ...)"
      ) {
        lazy val expectedEnum: Enumeration[Tree] =
          Enumeration
            .singleton(Tree("L", E))
            .union(
              Enumeration
                .singleton("M", E)
                .product(expectedEnum)
                .map { case (c, arg) => Tree(c._1, c._2, arg) }
                .pay
            )
        val result = TreeGrammarEnumeration(testGrammar, E)
        for (i <- 0 to 25)
          assert(
            result.parts(i).values.toSet == expectedEnum.parts(i).values.toSet
          )
      }
    }

    describe(s"Starting with ${A}") {
      it(s"should yield the same results as manual unrolling") {
        lazy val AB: Enumeration[Tree] =
          Enumeration.singleton(Tree("J", Arrow(A, B)))

        lazy val appABtoA: Enumeration[Tree] =
          AB.product(allA)
            .map {
              case (Tree(f, Arrow(_, tgt), args @ _*), arg2) =>
                Tree(f, tgt, (args :+ arg2): _*)
            }
            .pay

        lazy val allB: Enumeration[Tree] =
          Enumeration.singleton(Tree("I", B)).union(appABtoA)

        lazy val AAA: Enumeration[Tree] =
          Enumeration.singleton(Tree("G", Arrow(A, Arrow(A, A))))

        lazy val appAAAtoA: Enumeration[Tree] =
          AAA
            .product(allA)
            .map {
              case (Tree(f, Arrow(_, tgt), args @ _*), arg2) =>
                Tree(f, tgt, (args :+ arg2): _*)
            }
            .pay

        lazy val appAAtoA: Enumeration[Tree] =
          appAAAtoA
            .product(allA)
            .map {
              case (Tree(f, Arrow(_, tgt), args @ _*), arg2) =>
                Tree(f, tgt, (args :+ arg2): _*)
            }
            .pay

        lazy val ABA: Enumeration[Tree] =
          Enumeration.singleton(Tree("H", Arrow(A, Arrow(B, A))))

        lazy val appABAtoA: Enumeration[Tree] =
          ABA
            .product(allA)
            .map {
              case (Tree(f, Arrow(_, tgt), args @ _*), arg2) =>
                Tree(f, tgt, (args :+ arg2): _*)
            }
            .pay

        lazy val appBAtoB: Enumeration[Tree] =
          appABAtoA
            .product(allB)
            .map {
              case (Tree(f, Arrow(_, tgt), args @ _*), arg2) =>
                Tree(f, tgt, (args :+ arg2): _*)
            }
            .pay

        lazy val allA =
          Enumeration.singleton(Tree("F", A)).union(appBAtoB).union(appAAtoA)

        val result = TreeGrammarEnumeration(testGrammar, A)
        for (i <- 0 to 15)
          assert(result.parts(i).values.toSet == allA.parts(i).values.toSet)
      }
    }

  }

}
