package org.combinators.cls.inhabitation

import org.combinators.cls.types._
import syntax._
import org.scalatest.FunSpec
import shapeless.feat.Enumeration

class TreeGrammarEnumerationTest extends FunSpec {

  def production(from: Type, to: (String, Seq[Type])*): (Type, Set[(String, Seq[Type])]) =
    (from -> to.toSet)

  val testGrammar: TreeGrammar =
    Map[Type, Set[(String, Seq[Type])]](
      production('A, ("F", Seq()), ("G", Seq('A, 'A)), ("H", Seq('A, 'B))),
      production('B, ("I", Seq()), ("J", Seq('A))),
      production('C, ("K", Seq())),
      production('D),
      production('E, ("L", Seq()), ("M", Seq('E)))
    )

  describe (s"Unrolling {${testGrammar.toSeq.map{ case (k, v) => s"$k -> ${v.mkString(" | ")}}" }.mkString("; ")}}") {
    describe(s"Starting with ${toConstructor('D)}") {
      it("should yield an empty enumeration") {
        assert(TreeGrammarEnumeration(testGrammar, 'D).values.isEmpty)
      }
    }

    describe(s"Starting with  ${toConstructor('C)}") {
      it(s"should only yield ${Tree("K", 'C)}") {
        assert(TreeGrammarEnumeration(testGrammar, 'C).values.flatMap(_._2) ==
          Enumeration.singleton(Tree("K", 'C)).values.flatMap(_._2))
      }
    }

    describe(s"Starting with ${toConstructor('E)}") {
      it(s"should yield (${Tree("L", 'E)}, ${Tree("M", 'E, Tree("L", 'E))}, ${Tree("M", 'E, Tree("M", 'E, Tree("L", 'E)))}, ...)") {
        lazy val expectedEnum: Enumeration[Tree] =
          Enumeration.singleton(Tree("L", 'E)).pay
            .union(
              Enumeration.singleton("M", 'E)
                .product(expectedEnum.pay)
                .map { case (c, arg) => Tree(c._1, c._2, arg) }
                .pay
            )
        val result = TreeGrammarEnumeration(testGrammar, 'E)
        for (i <- 0 to 25)
          assert(result.parts(i).values.toSet == expectedEnum.parts(i).values.toSet)
      }
    }

    describe(s"Starting with ${toConstructor('A)}") {
      it(s"should yield the same results as manual unrolling") {
        lazy val expectedEnumB: Enumeration[Tree] = {
          val argA = expectedEnumA.pay
          Enumeration.singleton(Tree("I", 'B)).pay
            .union(
              Enumeration.singleton("J", 'B)
                .product(argA)
                .map { case (j, x) => Tree(j._1, j._2, x) }
                .pay
            )
        }
        lazy val expectedEnumA: Enumeration[Tree] = {
          val argA = expectedEnumA.pay
          val argB = expectedEnumB.pay
          Enumeration.singleton(Tree("F", 'A)).pay
            .union(
              Enumeration.singleton("G", 'A)
                .product(argA.product(argA))
                .map { case (g, (x, y)) => Tree(g._1, g._2, x, y) }
                .pay
            ).union(
              Enumeration.singleton("H", 'A)
                .product(argA.product(argB))
                .map { case (h, (x, y)) => Tree(h._1, h._2, x, y) }
                .pay
            )
        }
        val result = TreeGrammarEnumeration(testGrammar, 'A)
        for (i <- 0 to 25)
          assert(result.parts(i).values.toSet == expectedEnumA.parts(i).values.toSet)
      }
    }

  }

}
