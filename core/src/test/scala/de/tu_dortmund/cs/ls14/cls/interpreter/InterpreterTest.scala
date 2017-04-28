package de.tu_dortmund.cs.ls14.cls.interpreter

import de.tu_dortmund.cs.ls14.cls.inhabitation.{FiniteCombinatoryLogic, Tree, TreeGrammarEnumeration}
import org.scalatest._

import scala.reflect.runtime.universe.{Type => UType, _}
import de.tu_dortmund.cs.ls14.cls.types._
import syntax._

class GenericId[A] {
  def apply(x: A): A = x
}
class IntIdC extends GenericId[Int]

trait GenericTestRepo {
  @combinator object IntId extends IntIdC
  @combinator object X {
    def apply: Int = 42
  }
}

class InterpreterTest extends FunSpec {

  trait Top
  trait Super extends Top
  case class Sub() extends Super

  trait Repository1 {
    @combinator object f {
      def apply(x: => Int, y: String): List[Super] = List.empty
      def semanticType = Omega =>: 'bar =>: 'foo
    }
    object foo
  }

  trait Repository2 {
    @combinator object g1 {
      def apply(x: Type): Type => Type = x => x
      val semanticType = Omega =>: Omega
    }
    @combinator object g2 {
      def apply(x: Int, y: String): List[Sub] = List(Sub())
      def semanticType = Omega =>: 'bar =>: 'foo
    }
    object notACombinator {
      def apply(x: Int): String = "I'm not a combinator"
    }
  }

  trait RepoRepeat {
    @combinator object repeated {
      def apply(x: Double, y: Double): Double = x + y
      def semanticType = 'A =>: 'A =>: 'B
    }
    @combinator object repeatedStart {
      def apply: Double = 42
      val semanticType = 'A
    }
    val repatedTaxonomy = Taxonomy("A").addSubtype("B")
  }

  class Repo extends Repository1 with Repository2 with RepoRepeat {
    @combinator object h1 {
      def apply(): Int = 42
    }
    @combinator object h2 {
      def apply: String = "42"
      def semanticType = 'foo :&: 'bar
    }
    def alsoNotACombinator(): String = "I'm also not a combinator"
    val test: List[Sub] = List.empty
  }

  val repository = new Repo
  val result = ReflectedRepository(repository, semanticTaxonomy = repository.repatedTaxonomy)

  describe("The reflected repository") {
    val intTag: WeakTypeTag[Int] = implicitly
    val doubleTag: WeakTypeTag[Double] = implicitly
    val stringTag: WeakTypeTag[String] = implicitly
    val listSuperTag: WeakTypeTag[List[Super]] = implicitly
    val listSubTag: WeakTypeTag[List[Sub]] = implicitly
    val typeTag: WeakTypeTag[Type] = implicitly
    val typeTypeTag: WeakTypeTag[Type => Type] = implicitly

    val fExpectedInfo = CombinatorInfo("f", Some(List(intTag.tpe, stringTag.tpe)), listSuperTag.tpe, Some(Omega =>: 'bar =>: 'foo))
    val g1ExpectedInfo = CombinatorInfo("g1", Some(List(typeTag.tpe)), typeTypeTag.tpe, Some(Omega =>: Omega))
    val g2ExpectedInfo = CombinatorInfo("g2", Some(List(intTag.tpe, stringTag.tpe)), listSubTag.tpe, Some(Omega =>: 'bar =>: 'foo))
    val h1ExpectedInfo = CombinatorInfo("h1", Some(List()), intTag.tpe, None)
    val h2ExpectedInfo = CombinatorInfo("h2", None, stringTag.tpe, Some('foo :&: 'bar))
    val repeatedExpectedInfo = CombinatorInfo("repeated", Some(List(doubleTag.tpe, doubleTag.tpe)), doubleTag.tpe, Some('A =>: 'A =>: 'B))
    val repeatedStartExpectedInfo = CombinatorInfo("repeatedStart", None, doubleTag.tpe, Some('A))

    it(s"should include $fExpectedInfo") {
      assert(result.combinatorComponents.values.toSet.exists(_ =:= fExpectedInfo))
    }
    it(s"should include $g1ExpectedInfo") {
      assert(result.combinatorComponents.values.toSet.exists(_ =:= g1ExpectedInfo))
    }
    it(s"should include $g2ExpectedInfo") {
      assert(result.combinatorComponents.values.toSet.exists(_ =:= g2ExpectedInfo))
    }
    it(s"should include $h1ExpectedInfo") {
      assert(result.combinatorComponents.values.toSet.exists(_ =:= h1ExpectedInfo))
    }
    it(s"should include $h2ExpectedInfo") {
      assert(result.combinatorComponents.values.toSet.exists(_ =:= h2ExpectedInfo))
    }
    it(s"should include $repeatedExpectedInfo") {
      assert(result.combinatorComponents.values.toSet.exists(_ =:= repeatedExpectedInfo))
    }
    it(s"should include $repeatedStartExpectedInfo") {
      assert(result.combinatorComponents.values.toSet.exists(_ =:= repeatedStartExpectedInfo))
    }
    it(s"should include nothing else") {
      val allExpected = Set(
        fExpectedInfo,
        g1ExpectedInfo,
        g2ExpectedInfo,
        h1ExpectedInfo,
        h2ExpectedInfo,
        repeatedExpectedInfo,
        repeatedStartExpectedInfo
      )
      assert(result.combinatorComponents.values.toSet.filter(p => !allExpected.exists(_ =:= p)).isEmpty)
    }

  }

  describe("The native taxonomy of the reflected repository") {
    describe("when asked for subtypes of List[Super]") {
      it("should include List[Sub]") {
        assert(
          result
            .nativeTypeTaxonomy.taxonomy(ReflectedRepository.nativeTypeOf[List[Super]].name)
            .contains(ReflectedRepository.nativeTypeOf[List[Sub]].name))
      }
    }
  }

  val fTree = Tree("f", Tree("h1"), Tree("h2"))
  val g2Tree = Tree("g2", Tree("h1"), Tree("h2"))

  describe("when used for inhabitation of List[Top] :&: 'foo") {
    val inhabitants = result.inhabit[List[Top]]('foo)
    it(s"should yield $fTree and $g2Tree") {
      assert(!inhabitants.isInfinite)
      assert(inhabitants.terms.values.flatMap(_._2).forall(tree => tree == fTree || tree == g2Tree))
    }
  }

  describe("when used for inhabitation of Double :&: 'A") {
    val inhabitants = result.inhabit[Double]('A)
    val terms = inhabitants.interpretedTerms

    it(s"should be infinite") {
      assert(inhabitants.isInfinite)
    }

    it(s"should yield 42") {
      assert(terms.values.flatMap(_._2).contains(42))
    }
    it("should yield 84") {
      assert(terms.values.flatMap(_._2).contains(84))
    }
    it("should yield 126") {
      assert(terms.values.flatMap(_._2).contains(126))
    }
  }

  describe("Interpretation of inhabitants") {
    describe(s"Interpretation of $fTree") {
      it("should yield List.empty[Super]") {
        assert(result.evalInhabitant[List[Super]](fTree) == List.empty[List[Super]])
      }
    }
    describe(s"Interpretation of $g2Tree") {
      it("should yield List(Sub())") {
        assert(result.evalInhabitant[List[Sub]](g2Tree) == List(Sub()))
      }
    }
  }



  describe("Generic Instantiation") {

    val genericInstanceRepo = new GenericTestRepo {}
    val reflectedGenericInstanceRepo = ReflectedRepository[GenericTestRepo](genericInstanceRepo)
    val intTag: WeakTypeTag[Int] = implicitly
    val IntIdExpectedInfo = CombinatorInfo("IntId", Some(List(intTag.tpe)), intTag.tpe, None)
    it("should resolve type variables") {
      val result = reflectedGenericInstanceRepo.combinatorComponents("IntId")
      assert(result =:= IntIdExpectedInfo)
    }

  }

}
