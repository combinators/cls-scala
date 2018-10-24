package org.combinators.cls.inhabitation

import org.scalatest._
import org.combinators.cls.types._


class BCLTest extends FunSpec {

  val mapTest =
    Map(
      "map" ->
        Arrow(
          Arrow(Variable("alpha"), Variable("beta")),
          Arrow(
            Constructor("List", Variable("alpha")),
            Constructor("List", Variable("beta"))
          )
        ),
      "l" -> Constructor("List", Constructor("Int")),
      "f" -> Arrow(Constructor("Char"), Constructor("String"))
    )

  val taxonomy: Taxonomy =
    Taxonomy
      .empty
      .merge(Taxonomy("Char")
              .addSubtype("Int"))
      .merge(Taxonomy("String"))

  def addAll(k: NonEmptyKinding): NonEmptyKinding =
    k.addOption(Constructor("Char"))
     .addOption(Constructor("Int"))
     .addOption(Constructor("String"))

  val kinding: Kinding =
    addAll(Kinding(Variable("alpha"))).merge(addAll(Kinding(Variable("beta"))))

  val Gamma = new BoundedCombinatoryLogic(kinding, SubtypeEnvironment(taxonomy.underlyingMap), mapTest)

  describe(Gamma.toString) {
    describe("|- ? : String") {
      val tgt = Constructor("List", Constructor("String"))
      val results = Gamma.inhabit(tgt)
      it("should not be empty") {
        assert(results.nonEmpty)
      }
      it("should unroll exactly to Tree(map, _, Seq(Tree(f, _, Seq()), Tree(l, _, Seq())))") {
        def isExpectedTree(t: Tree): Boolean =
          t.name == "map" &&
            t.arguments.size == 2 &&
            t.arguments.headOption.exists(arg => arg.name == "f" && arg.arguments.isEmpty) &&
            t.arguments.tail.headOption.exists(arg => arg.name == "l" && arg.arguments.isEmpty)
        assert(TreeGrammarEnumeration(results, tgt).values.forall(_._2.forall(isExpectedTree)))
      }
    }
  }

}
