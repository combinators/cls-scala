package org.combinators.cls.inhabitation

import org.scalatest.funspec.AnyFunSpec
import org.combinators.cls.types._

class EdgeCases extends AnyFunSpec {

  val edgeCaseRepository =
    Map(
      "f" ->
        Arrow(
          Arrow(Variable("alpha"), Variable("beta")),
          Arrow(
            Constructor("X", Variable("alpha")),
            Constructor("Y", Variable("beta"))
          )
        ),
      "g" -> Arrow(Omega, Constructor("X", Variable("alpha"))),
      "x" -> Constructor("X", Constructor("Int")),
      "y" ->
        Arrow(
          Constructor("X", Variable("alpha")),
          Constructor("Y", Variable("alpha"))
        ),
      "z" ->
        Arrow(
          Constructor("X", Constructor("Bottom")),
          Constructor("Y", Constructor("Int"))
        ),
      "i" ->
        Intersection(
          Arrow(
            Arrow(Constructor("Int"), Constructor("Int")),
            Constructor("X", Constructor("Int"))
          ),
          Arrow(
            Arrow(Constructor("Char"), Constructor("Char")),
            Constructor("X", Constructor("String"))
          )
        ),
      "ia" -> Intersection(
        Arrow(Variable("alpha"), Variable("alpha")),
        Variable("gamma")
      ),
      "gt" -> Arrow(
        Intersection(
          Constructor("A"),
          Intersection(Constructor("C"), Constructor("B"))
        ),
        Arrow(
          Intersection(Constructor("B"), Constructor("A")),
          Constructor("Y", Constructor("Int"))
        )
      ),
      "gtArg" -> Intersection(
        Arrow(Constructor("X", Constructor("Int")), Constructor("A")),
        Intersection(
          Arrow(Constructor("Z", Constructor("Int")), Constructor("B")),
          Intersection(
            Arrow(Constructor("Z", Constructor("Int")), Constructor("C")),
            Intersection(
              Arrow(
                Constructor("Z", Constructor("Bottom")),
                Intersection(Constructor("A"), Constructor("B"))
              ),
              Arrow(
                Constructor("Y", Constructor("Int")),
                Arrow(
                  Constructor("OtherBottom"),
                  Arrow(
                    Constructor("Y", Constructor("OtherBottom")),
                    Intersection(Constructor("A"), Constructor("B"))
                  )
                )
              )
            )
          )
        )
      )
    )

  val taxonomy: Taxonomy = Taxonomy("X").addSubtype("Z")
  def addAll(k: NonEmptyKinding): NonEmptyKinding =
    k.addOption(Constructor("Char"))
      .addOption(Constructor("Int"))
      .addOption(Constructor("String"))
      .addOption(Omega)

  val kinding: Kinding =
    addAll(Kinding(Variable("alpha"))).merge(
      Kinding(Variable("gamma"))
        .addOption(
          Arrow(
            Constructor("X", Constructor("Int")),
            Constructor("X", Constructor("Int"))
          )
        )
        .addOption(
          Arrow(
            Constructor("X", Constructor("Int")),
            Constructor("Z", Constructor("Int"))
          )
        )
    )

  val Gamma = new BoundedCombinatoryLogic(
    kinding,
    SubtypeEnvironment(taxonomy.underlyingMap),
    edgeCaseRepository
  )

  describe(Gamma.toString) {
    describe("|- ? : Y(Int), |- ? : X(Bottom), |- ? : Z(Bottom)") {
      val tgt = Constructor("Y", Constructor("Int"))
      val results = Gamma.inhabit(
        tgt,
        Omega,
        Constructor("X", Constructor("Bottom")),
        Constructor("Z", Constructor("Bottom"))
      )
      it("should not be empty") {
        assert(results.nonEmpty)
      }
      it(
        "should include { omega |-> f | g | x | y | z | i | ia | gt | gtArg | @(omega, omega) }"
      ) {
        val omegaRules: Set[Rule] =
          Gamma.repository.keySet.map(c => Combinator(Omega, c))
        val omegaApplications = Set[Rule](
          Apply(Omega, Omega, Omega)
        )
        assert(omegaRules.subsetOf(results))
        assert(omegaApplications.subsetOf(results))
      }
      it(
        "should include { X(Int) |-> x | @(omega -> X(Int), omega), omega -> X(Int) |-> g }"
      ) {
        Set[Rule](
          Combinator(Constructor("X", Constructor("Int")), "x"),
          Combinator(Arrow(Omega, Constructor("X", Constructor("Int"))), "g"),
          Apply(
            Constructor("X", Constructor("Int")),
            Arrow(Omega, Constructor("X", Constructor("Int"))),
            Omega
          )
        ).subsetOf(results)
      }
      it(
        "should not use f with any other type than omega or omega -> omega"
      ) {
        assert(
          results.forall {
            case Combinator(tgt, "f") =>
              tgt == Omega || tgt == Arrow(Omega, Omega)
            case _ => true
          }
        )
      }
      it(
        "should contain failures for the Bottom type"
      ) {
        assert(
          results.forall(r =>
            if (Set[Type](
                  Constructor("Bottom"),
                  Constructor("X", Constructor("Bottom")),
                  Constructor("Z", Constructor("Bottom"))
                ).contains(r.target)) {
              r match {
                case Failed(_) => true
                case _         => false
              }
            } else true
          )
        )
      }
    }
  }
}
