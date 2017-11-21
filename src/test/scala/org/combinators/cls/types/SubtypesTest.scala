package de.tu_dortmund.cs.ls14.cls.types

import org.scalatest.FunSpec

class SubtypesTest extends FunSpec {
  val onlyInt = SubtypeEnvironment(Taxonomy("Int").underlyingMap)
  describe("The environment only containing Int") {
    import onlyInt._
    it ("Should validate Int <= Int") {
      assert(Constructor("Int").isSupertypeOf(Constructor("Int")))
    }
    it ("Should validate Int -> Int <= Int -> Int") {
      assert(Arrow(Constructor("Int"), Constructor("Int"))
        .isSupertypeOf(Arrow(Constructor("Int"), Constructor("Int"))))
    }
  }
}
