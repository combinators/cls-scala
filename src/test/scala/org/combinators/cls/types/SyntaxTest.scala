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
    it("should pretty print almost identically") {
      assert(
        ((a :&: b) :&: c :&: (a :&: c =>: b)).toString ==
          "a & b & c & (a & c -> b)"
      )
    }

  }
  describe("Constructor notations") {
    it("should work unapplied") {
      val aTest: Type = 'a
      assert(aTest == a)
    }
    it("should work with arguments") {
      assert('a('b, 'c) == Constructor("a", Constructor("b"), Constructor("c")))
    }
    it("should work combined with arrows") {
      assert(('a =>: 'b) == Arrow(a, b))
      assert(('a('b) =>: 'c) == Arrow(Constructor("a", b), c))
      assert(('a =>: 'b('a, 'c)) == Arrow(a, Constructor("b", a, c)))
    }
    it("should work combined with intersections") {
      assert('a :&: 'b == Intersection(a, b))
      assert('a('b) :&: 'c == Intersection(Constructor("a", b), c))
      assert('a :&: 'b('a, 'c) == Intersection(a, Constructor("b", a, c)))
    }
    it("should pretty print almost identically") {
      assert(('a('b, Omega) :&: 'x).toString == "a(b, omega) & x")
    }
  }
}
