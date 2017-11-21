package de.tu_dortmund.cs.ls14.cls.inhabitation

import org.scalatest._
import de.tu_dortmund.cs.ls14.cls.types._

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
        assert(results == Map(
            Constructor("Goal") -> Set(("f", Seq(Constructor("Int")))),
            Constructor("Int") -> Set(("x", Seq()))
          ))
      }
      it("should only unroll to Tree(f, Tree(x))") {
        assert(
          TreeGrammarEnumeration(results, tgt).values ==
            (BigInt(0), Stream.empty[Tree]) #::
              (BigInt(0), Stream.empty[Tree]) #::
              (BigInt(0), Stream.empty[Tree]) #::
              (BigInt(1), Tree("f", Tree("x")) #:: Stream.empty[Tree]) #::
              (BigInt(0), Stream.empty[Tree]) #::
              Stream.empty[(Int, Stream[Tree])]
        )
      }
    }
    describe("|- ? : Garbage1") {
      val tgt = Constructor("Garbage1")
      val results = Gamma.inhabit(tgt)
      it("should be empty") {
        assert(results.isEmpty)
      }
      it("should unroll to an empty enumeration") {
        assert(TreeGrammarEnumeration(results, tgt).values.isEmpty)
      }
    }
  }

}
