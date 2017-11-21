package de.tu_dortmund.cs.ls14.cls.inhabitation

import de.tu_dortmund.cs.ls14.cls.types._
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
      it(s"should only yield ${Tree("K")}") {
        assert(TreeGrammarEnumeration(testGrammar, 'C).values.flatMap(_._2) ==
          Enumeration.singleton(Tree("K")).values.flatMap(_._2))
      }
    }

    describe(s"Starting with ${toConstructor('E)}") {
      it(s"should yield (${Tree("L")}, ${Tree("M", Tree("L"))}, ${Tree("M", Tree("M", Tree("L")))}, ...)") {
        lazy val expectedEnum: Enumeration[Tree] =
          Enumeration.singleton(Tree("L")).pay
            .union(
              Enumeration.singleton("M")
                .product(expectedEnum.pay)
                .map { case (c, arg) => Tree(c, arg) }
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
          Enumeration.singleton(Tree("I")).pay
            .union(
              Enumeration.singleton("J")
                .product(argA)
                .map { case (j, x) => Tree(j, x) }
                .pay
            )
        }
        lazy val expectedEnumA: Enumeration[Tree] = {
          val argA = expectedEnumA.pay
          val argB = expectedEnumB.pay
          Enumeration.singleton(Tree("F")).pay
            .union(
              Enumeration.singleton("G")
                .product(argA.product(argA))
                .map { case (g, (x, y)) => Tree(g, x, y) }
                .pay
            ).union(
              Enumeration.singleton("H")
                .product(argA.product(argB))
                .map { case (h, (x, y)) => Tree(h, x, y) }
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
