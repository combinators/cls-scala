package de.tu_dortmund.cs.ls14.cls.interpreter

import de.tu_dortmund.cs.ls14.cls.inhabitation.{FiniteCombinatoryLogic, Tree, TreeGrammarEnumeration}
import org.scalatest._

import scala.reflect.runtime.universe.{Type => UType, _}
import de.tu_dortmund.cs.ls14.cls.types._
import Type.syntax._


class InterpreterTest extends FunSpec {

  trait Super
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

  class Repo extends Repository1 with Repository2 {
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
  val result = ReflectedRepository(repository)

  describe("The reflected repository") {
    val intTag: WeakTypeTag[Int] = implicitly
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

    it(s"should include $fExpectedInfo") {
      assert(result.combinatorComponents.values.toSet.contains(fExpectedInfo))
    }
    it(s"should include $g1ExpectedInfo") {
      assert(result.combinatorComponents.values.toSet.contains(g1ExpectedInfo))
    }
    it(s"should include $g2ExpectedInfo") {
      assert(result.combinatorComponents.values.toSet.contains(g2ExpectedInfo))
    }
    it(s"should include $h1ExpectedInfo") {
      assert(result.combinatorComponents.values.toSet.contains(h1ExpectedInfo))
    }
    it(s"should include $h2ExpectedInfo") {
      assert(result.combinatorComponents.values.toSet.contains(h2ExpectedInfo))
    }
    it(s"should include nothing else") {
      assert((result.combinatorComponents.values.toSet -
        fExpectedInfo -
        g1ExpectedInfo -
        g2ExpectedInfo -
        h1ExpectedInfo -
        h2ExpectedInfo
        ).isEmpty)
    }

  }

  describe("The native taxonomy of the reflected repository") {
    describe("when asked for subtypes of List[Super]") {
      it("should include List[Sub]") {
        assert(
          result
            .nativeTypeTaxonomy(ReflectedRepository.nativeTypeOf[List[Super]].name)
            .contains(ReflectedRepository.nativeTypeOf[List[Super]].name))
      }
    }
  }

  val fTree = Tree("f", Tree("h1"), Tree("h2"))
  val g2Tree = Tree("g2", Tree("h1"), Tree("h2"))

  describe("when used for inhabitation of List[Super] :&: 'foo") {
    val logic = new FiniteCombinatoryLogic(SubtypeEnvironment(result.nativeTypeTaxonomy), result.combinators)
    val target = ReflectedRepository.nativeTypeOf[List[Super]] :&: 'foo
    val grammar = logic.inhabit(target)
    val inhabitants = TreeGrammarEnumeration(grammar, target)
    it("should yield $fTree and $g2Tree") {
      assert(inhabitants.values.flatMap(_._2).forall(tree => tree == fTree || tree == g2Tree))
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



}