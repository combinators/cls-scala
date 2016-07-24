package de.tu_dortmund.cs.ls14.cls.types

import org.scalatest.FunSpec

class SubtypesTest extends FunSpec {
  val onlyInt = SubtypeEnvironment(Taxonomy("Int"))
  describe("The environment only containing Int") {
    import onlyInt._
    it ("Should validate Int <= Int") {
      assert(Constructor("Int").isSupertype(Constructor("Int")))
    }
    it ("Should validate Int -> Int <= Int -> Int") {
      assert(Arrow(Constructor("Int"), Constructor("Int"))
        .isSupertype(Arrow(Constructor("Int"), Constructor("Int"))))
    }
  }
}
