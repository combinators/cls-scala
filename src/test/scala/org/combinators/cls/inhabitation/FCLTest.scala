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
      val tgt = Constructor("Int")
      val results = Gamma.inhabit(tgt)
      it("should not return an map") {
        assert(results.nonEmpty)
      }
      it("should contain Zero()") {
        assert(results(tgt).contains(("Zero", Seq.empty)))
      }
      it("should contain Succ(Int & Even)") {
        assert(results(tgt).contains("Succ", Seq(Intersection(tgt, Constructor("Even")))))
      }
      it("should unroll to Tree(Zero) at index 0") {
        assert(TreeGrammarEnumeration(results, Constructor("Int")).index(0) == Tree("Zero"))
      }
      it("should unroll to Tree(Succ, Tree(Succ, Tree(Succ, Tree(Zero)))) at index 4") {
        assert(
          TreeGrammarEnumeration(results, Constructor("Int")).index(4) ==
          Tree("Succ", Tree("Succ", Tree("Succ", Tree("Succ", Tree("Zero"))))))
      }
    }

    describe("|- ? : List(Int)") {
      val tgt = Constructor("List", Constructor("Int"))
      val results = Gamma.inhabit(tgt)
      it("should be empty") {
        assert(results.isEmpty)
      }
    }

    describe("|- ? : Char") {
      val tgt = Constructor("Char")
      val tgt2 = Constructor("Int")
      val results = Gamma.inhabit(tgt)
      val results2 = Gamma.inhabit(tgt2)
      it("should equal the results for Int plus an entry for Char") {
        assert(results2 - tgt2 + (tgt -> results2(tgt2)) == results)
      }
    }


  }
}
