package org.combinators.cls.types

import org.scalatest.FunSpec

class SubtypesTest extends FunSpec {
  val onlyInt = SubtypeEnvironment(Taxonomy("Int").underlyingMap)
  val acbdEnv = SubtypeEnvironment(Taxonomy("a").addSubtype("c").merge(Taxonomy("b").addSubtypes(Taxonomy("d").addSubtype("x"))).underlyingMap)
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
  describe("The type omega") {
    import acbdEnv._
    it("should satisfy omega <= omega :&: omega") {
      assert(Omega.isSubtypeOf(Arrow(Omega, Omega)))
      assert(Arrow(Omega, Omega).isSupertypeOf(Omega))
    }
    it("should satisfy omega :&: omega <= omega") {
      assert(Arrow(Omega, Omega).isSubtypeOf(Omega))
      assert(Omega.isSupertypeOf(Arrow(Omega, Omega)))
    }
    it("should be supertype of any other type") {
      assert(Omega.isSupertypeOf(Constructor("a")))
      assert(Omega.isSupertypeOf(Constructor("b")))
      assert(Omega.isSupertypeOf(Constructor("c")))
      assert(Omega.isSupertypeOf(Arrow(Constructor("a"), Constructor("a"))))
      assert(Omega.isSupertypeOf(Arrow(Constructor("a"), Omega)))
      assert(Omega.isSupertypeOf(Arrow(Omega, Intersection(Constructor("a"), Omega))))
    }
    it("should not be subtype of any other type") {
      assert(!Omega.isSubtypeOf(Constructor("a")))
      assert(!Omega.isSubtypeOf(Constructor("b")))
      assert(!Omega.isSubtypeOf(Constructor("c")))
      assert(!Omega.isSubtypeOf(Arrow(Constructor("a"), Constructor("a"))))
      assert(!Omega.isSubtypeOf(Arrow(Omega, Intersection(Constructor("a"), Omega))))
    }
    it("should not have any paths") {
      assert(Organized(Omega).paths.isEmpty)
      assert(Organized(Arrow(Constructor("a"), Omega)).paths.isEmpty)
    }
  }
  describe("Constructors") {
    import acbdEnv._
    it("should allow subtyping without arguments") {
      assert(Constructor("a").isSupertypeOf(Constructor("c")))
      assert(Constructor("d").isSubtypeOf(Constructor("b")))
    }
    it("should allow subtyping without omega only arguments") {
      assert(Constructor("a", Omega, Omega).isSupertypeOf(Constructor("c", Omega, Omega)))
      assert(Constructor("d", Omega).isSubtypeOf(Constructor("b", Omega)))
    }
    it("should distribute wrt. to subtyping") {
      assert(Intersection(Constructor("a", Constructor("d"), Omega), Constructor("a", Omega, Constructor("d")))
              .isSubtypeOf(Constructor("a", Constructor("b"), Constructor("d"))))
      assert(Intersection(Constructor("a", Constructor("d"), Omega), Constructor("a", Omega, Constructor("d")))
        .isSupertypeOf(Constructor("a", Constructor("d"), Constructor("d"))))
    }
    it("should split into multiple paths with omega arguments") {
      assert(
        Organized(
          Constructor("a",
            Constructor("b", Omega),
            Intersection(Constructor("d", Constructor("e")), Constructor("e")))).paths.toSet ==
        Set(
          Constructor("a", Constructor("b", Omega), Omega),
          Constructor("a", Omega, Constructor("d", Constructor("e"))),
          Constructor("a", Omega, Constructor("e"))
        ))
    }
    it("should work transitively") {
      assert(
        Arrow(Arrow(Constructor("x", Constructor("c")), Constructor("a")),
            Intersection(Constructor("e"), Constructor("f")))
          .isSubtypeOf(
            Arrow(Arrow(Constructor("b", Constructor("a")), Constructor("c")), Constructor("f"))
          )
      )
    }
  }
  describe("Arrows") {
    import acbdEnv._
    it("should be co-contra variant") {
      assert(Arrow(Constructor("a"), Constructor("d")).isSubtypeOf(Arrow(Constructor("c"), Constructor("b"))))
      assert(Arrow(Constructor("a"), Constructor("d")).isSubtypeOf(Arrow(Constructor("c"), Constructor("d"))))
    }
    it("should distribute wrt. intersection") {
      assert(
        Intersection(
          Arrow(Constructor("a"), Constructor("c")),
          Arrow(Constructor("c"), Constructor("c"))
        ).isSubtypeOf(
          Arrow(Intersection(Constructor("a"), Constructor("b")), Constructor("c"))
        )
      )
    }
    it("should work nested") {
      assert(
        Arrow(Constructor("a"), Arrow(Constructor("b"), Arrow(Constructor("e"), Constructor("c"))))
            .isSubtypeOf(Arrow(Constructor("c"), Arrow(Constructor("d"), Arrow(Constructor("e"), Constructor("a")))))
      )
      assert(
        Arrow(Arrow(Constructor("c"), Constructor("b")), Constructor("c"))
            .isSubtypeOf(Arrow(Arrow(Constructor("a"), Constructor("d")), Constructor("a")))
      )
    }
  }
  describe("minimized path sets") {
    import acbdEnv._
    val originalSeq =
      Seq(
        Constructor("a"),
        Constructor("b"),
        Constructor("c"),
        Constructor("e"),
        Constructor("d")
      )
    val originalType = originalSeq.foldRight[Type](Omega)((x, y) => Intersection(x, y))
    val minimalPaths = Organized(originalType).paths.minimize
    it("should be equal to their original types") {
      assert(originalType.isSubtypeOf(Organized.intersect(minimalPaths)))
    }
    it("should not contain redundant paths") {
      assert(
        minimalPaths.forall(p => !minimalPaths.exists(otherPath => p != otherPath && p.isSubtypeOf(otherPath)))
      )
    }
  }
}
