package org.combinators.cls.inhabitation

import org.combinators.cls.types._
import org.scalatest.FunSpec

class EarlyAbortReschedulingTest extends FunSpec {

  val repository =
    Map(
      "mkFail" -> Arrow(
          Constructor("Ok"), // schedule targets Always, Ok
          Arrow(
            Constructor("Fail"), // fail, schedule nothing
            Constructor("PoisonOk") // fail: remove Always from schedule, forget Ok in grammar, or grammar is poisoned
          )
        ),
      "mkPoisonOk" -> Constructor("PoisonOk"),
      "mkDone" -> Arrow(
          Constructor("Ok"), // schedule Always, Ok
          Arrow(
            Constructor("AlsoOk"),
            Constructor("Done")
          )
        ),
      "mkOk" -> Arrow(Constructor("Always"), Constructor("Ok")),
      "mkOkOk" -> Arrow(Constructor("Ok"), Constructor("Ok")),
      "mkAlsoOk" -> Constructor("AlsoOk"),
      "mkAlways" -> Constructor("Always")
    )

  val Gamma = new FiniteCombinatoryLogic(SubtypeEnvironment(Map.empty), repository)

  describe(Gamma.toString) {
    describe("First |- ? : Fail then |- ? : Done") {
      val tgt1 = Constructor("PoisonOk")
      val tgt2 = Constructor("Done")
      val results = Gamma.inhabit(tgt1, tgt2)
      val expectedResults: TreeGrammar =
        Map(
          Constructor("PoisonOk") -> Set(("mkPoisonOk", Seq.empty)),
          Constructor("Always") -> Set(("mkAlways", Seq.empty)),
          Constructor("AlsoOk") -> Set(("mkAlsoOk", Seq.empty)),
          Constructor("Ok") -> Set(
            ("mkOk", Seq(Constructor("Always"))),
            ("mkOkOk", Seq(Constructor("Ok")))
          ),
          Constructor("Done") -> Set(("mkDone", Seq(Constructor("Ok"), Constructor("AlsoOk"))))
        )

      it(s"should exactly produce ${prettyPrintTreeGrammar(expectedResults)}") {
        assert(results == expectedResults)
      }
    }
  }
}
