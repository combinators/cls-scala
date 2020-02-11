/*
 * Copyright 2018-2020 Jan Bessai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.combinators.cls.interpreter

import org.scalatest._

import org.combinators.cls.types._
import syntax._


class DynamicCombinatorTest extends FunSpec {

  object SemanticTypes {
    val NonSense: Type = Constructor("NonSense")
    val Sense: Type = Constructor("Sense")
    val Sense1: Type = Constructor("Sense1")
    val Sense2: Type = Constructor("Sense2")
  }
  import SemanticTypes._

  class Repo {
    @combinator object NonSense {
      def apply: Int = 41
      val semanticType: Type = SemanticTypes.NonSense
    }
    @combinator object Show {
      def apply(x: Int): String = x.toString
      val semanticType: Type = (Sense =>: Sense) :&: (SemanticTypes.NonSense =>: SemanticTypes.NonSense)
    }
  }

  object MakeSense {
    def apply(x: Int): Int = x + 1
    val semanticType: Type = NonSense =>: Sense
  }

  val repository = new Repo
  val result = ReflectedRepository(repository)


  describe("The augmented repository") {
    val augmentedResult = result.addCombinator(MakeSense)
    describe("when inhabiting NonSense") {
      val inhabitants = result.inhabit[String](NonSense)
      it("should find NonSense") {
        assert(!inhabitants.isEmpty)
        assert(inhabitants.interpretedTerms.index(0) == "41")
      }
    }
    describe("when inhabiting Sense") {
      val inhabitants = result.inhabit[String](Sense)
      it("should not find anything") {
        assert(!inhabitants.isInfinite)
        assert(inhabitants.isEmpty)
        assert(inhabitants.size.contains(0))
        assert(inhabitants.interpretedTerms.values.isEmpty)
      }
    }
    describe("When dynamically agumented with MakeSense") {
     describe("when inhabiting NonSense") {
        val inhabitants = augmentedResult.inhabit[String](NonSense)
        it("Should find NonSense") {
          assert(!inhabitants.isEmpty)
          assert(inhabitants.size.exists(_ > 0))
          assert(inhabitants.interpretedTerms.index(0) == "41")
        }
      }
      describe("when inhabiting Sense") {
        val inhabitants = augmentedResult.inhabit[String](Sense)
        it("should find 42") {
          assert(!inhabitants.isEmpty)
          assert(inhabitants.size.exists(_ > 0))
          assert(inhabitants.interpretedTerms.index(0) == "42")
        }
      }
    }
  }

  class IncrementCombinator(delta: Int, semantics: Type) {
    def apply(x: Int): Int = x + delta
    val semanticType: Type = semantics
  }

  describe("The reflected repository with two IncrementCombinator instances") {
    val incOne = new IncrementCombinator(1, NonSense =>: Sense1)
    val incTwo = new IncrementCombinator(2, Sense1 =>: Sense2)
    val augmentedResult = result.addCombinator(incOne).addCombinator(incTwo)
    describe("when inhabiting Sense2") {
      val inhabitants = augmentedResult.inhabit[Int](Sense2)
      it("should find 44") {
        assert(!inhabitants.isEmpty)
        assert(inhabitants.size.exists(_ > 0))
        assert(inhabitants.interpretedTerms.index(0) == 44)
      }
    }
  }

}
