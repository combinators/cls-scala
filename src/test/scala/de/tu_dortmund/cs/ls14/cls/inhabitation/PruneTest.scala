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

  val taxonomy =
    Taxonomy("Int")
      .merge(Taxonomy("Garbage1"))
      .merge(Taxonomy("Garbage2"))
      .merge(Taxonomy("Goal"))

  val Gamma = new FiniteCombinatoryLogic(SubtypeEnvironment(taxonomy), garbageCombinators)

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
            (0, Stream.empty[Tree]) #::
              (0, Stream.empty[Tree]) #::
              (1, Tree("f", Tree("x")) #:: Stream.empty[Tree]) #::
              (0, Stream.empty[Tree]) #::
              Stream.empty[(Int, Stream[Tree])]
        )
      }
    }
  }

}
