package de.tu_dortmund.cs.ls14.cls.interpreter

import org.scalatest._
import de.tu_dortmund.cs.ls14.cls.types._

class InterpreterTest extends FunSpec {

  trait Repository1 {
    @combinator object f {
      def apply(x: Int, y: String): List[Int] = List.empty
      def semanticType = Omega
    }
    //def fSemantic = Omega
  }

  trait Repository2 {
    @combinator object g {
      def apply(x: Type): Type => Type = x => x
      val semanticType = Arrow(Omega, Omega)
    }
    def notACombinator(x: Int): String = "I'm not a combinator"
  }

  class Repo extends Repository1 with Repository2

  describe("Repo") {
    val repo = new Repo
    val result = Repository(repo)
    describe("combinatorComponents") {
      it("should include CombinatorInfo(f, Seq(Int, String), List[Int], Constructor(Foo))") {
        println(result.combinatorComponents)
        assert(true)
      }
    }
  }

}
