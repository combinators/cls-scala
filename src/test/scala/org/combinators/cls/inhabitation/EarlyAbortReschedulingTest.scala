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
      val expectedResults: Set[Rule] =
        Set[Rule](
          Combinator(Constructor("PoisonOk"), "mkPoisonOk"),
          Combinator(Constructor("Always"), "mkAlways"),
          Combinator(Constructor("AlsoOk"), "mkAlsoOk"),
          Apply(Constructor("Ok"), Arrow(Constructor("Always"), Constructor("Ok")), Constructor("Always")),
          Apply(Constructor("Ok"), Arrow(Constructor("Ok"), Constructor("Ok")), Constructor("Ok")),
          Combinator(Arrow(Constructor("Always"), Constructor("Ok")), "mkOk"),
          Combinator(Arrow(Constructor("Ok"), Constructor("Ok")), "mkOkOk"),
          Apply(Constructor("Done"), Arrow(Constructor("AlsoOk"), Constructor("Done")), Constructor("AlsoOk")),
          Apply(Arrow(Constructor("AlsoOk"), Constructor("Done")), Arrow(Constructor("Ok"), Arrow(Constructor("AlsoOk"), Constructor("Done"))), Constructor("Ok")),
          Combinator(Arrow(Constructor("Ok"), Arrow(Constructor("AlsoOk"), Constructor("Done"))), "mkDone"),
          Failed(Constructor("Fail")),
          Combinator(Arrow(Constructor("Ok"), Arrow(Constructor("Fail"), Constructor("PoisonOk"))), "mkFail"),
          Apply(Arrow(Constructor("Fail"), Constructor("PoisonOk")), Arrow(Constructor("Ok"), Arrow(Constructor("Fail"), Constructor("PoisonOk"))), Constructor("Ok"))
        )
            
      it(s"should exactly produce ${prettyPrintRuleSet(expectedResults)}") {
        assert(results == expectedResults)
      }
    }
  }
}
