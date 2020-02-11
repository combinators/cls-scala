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

package org.combinators.cls.types

import org.scalatest.FunSpec
import syntax._

class SyntaxTest extends FunSpec {

  val a = Constructor("a")
  val b = Constructor("b")
  val c = Constructor("c")

  describe("Arrow notations") {
    it("should go from source to target") {
      assert((a =>: b) == Arrow(a, b))
    }
    it("should be right associative") {
      assert((a =>: b =>: c) == Arrow(a, Arrow(b, c)))
    }
    it("should pretty print almost identically") {
      assert(
        (((a =>: b) =>: c :&: b =>: a) :&: (a =>: a)).toString ==
          "((a -> b) -> c & b -> a) & (a -> a)"
      )
    }
  }
  describe("Intersection notations") {
    it("should preserve order") {
      assert(a :&: b == Intersection(a, b))
    }
    it("should be right associative") {
      assert(a :&: b :&: c == Intersection(a, Intersection(b, c)))
    }
    it("should take precedence over arrow sources") {
      assert((a :&: b =>: c) == Arrow(Intersection(a, b), c))
    }
    it("should take precedence over arrow targets") {
      assert((a =>: b :&: c) == Arrow(a, Intersection(b, c)))
    }
    it("should take precedence over products to the right ") {
      assert((a :&: b <*> c) == Product(Intersection(a, b), c))
    }
    it("should take precedence over products to the left ") {
      assert((a <*> b :&: c) == Product(a, Intersection(b, c)))
    }
    it("should pretty print almost identically") {
      assert(
        ((a :&: b) :&: c :&: (a :&: c =>: b) :&: (a =>: c:&: b) :&: (a :&: b <*> c) :&: (a <*> b :&: c)).toString ==
          "a & b & c & (a & c -> b) & (a -> c & b) & (a & b * c) & (a * b & c)"
      )
    }
  }
  describe("Product notations") {
    it("should preserve order") {
      assert((a <*> b) == Product(a, b))
    }
    it("should be left associative") {
      assert((a <*> b <*> c) == Product(Product(a, b), c))
    }
    it("should take precedence over arrow sources") {
      assert((a <*> b =>: c) == Arrow(Product(a, b), c))
    }
    it("should take precedence over arrow targets") {
      assert((a =>: b <*> c) == Arrow(a, Product(b, c)))
    }
  }
  describe("Constructor notations") {
    val asym = Symbol("a")
    val bsym = Symbol("b")
    val csym = Symbol("c")
    val dsym = Symbol("d")
    val xsym = Symbol("x")

    it("should work unapplied") {
      val aTest: Type = asym
      assert(asym.argument == Omega)
      assert(aTest == a)
    }
    it("should work with arguments") {
      assert(asym(bsym, csym) == Constructor("a", Product(Constructor("b"), Constructor("c"))))
      assert(asym(bsym, csym, dsym) == Constructor("a", Product(Product(Constructor("b"), Constructor("c")), Constructor("d"))))
    }
    it("should work combined with arrows") {
      assert((asym =>: bsym) == Arrow(a, b))
      assert((asym(bsym) =>: csym) == Arrow(Constructor("a", b), c))
      assert((asym =>: bsym(asym, csym)) == Arrow(a, Constructor("b", Product(a, c))))
    }
    it("should work combined with intersections") {
      assert(asym :&: bsym == Intersection(a, b))
      assert(asym(bsym) :&: csym == Intersection(Constructor("a", b), c))
      assert(asym :&: bsym(asym, csym) == Intersection(a, Constructor("b", Product(a, c))))
    }
    it("should pretty print almost identically") {
      assert((asym(bsym, Omega) :&: xsym).toString == "a(b * omega) & x")
    }
  }
}
