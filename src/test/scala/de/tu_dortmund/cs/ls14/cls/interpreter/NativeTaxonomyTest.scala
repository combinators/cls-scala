package de.tu_dortmund.cs.ls14.cls.interpreter

import org.scalatest.FunSpec

class NativeTaxonomyTest extends FunSpec {
  trait Super
  class SubA extends Super
  class SubB extends Super

  val superTypeName = ReflectedRepository.nativeTypeOf[Super].name
  val subATypeName = ReflectedRepository.nativeTypeOf[SubA].name
  val subBTypeName = ReflectedRepository.nativeTypeOf[SubB].name
  val stringTypeName = ReflectedRepository.nativeTypeOf[String].name

  val taxonomy =
    NativeTaxonomy[Super, SubA]
      .merge(NativeTaxonomy[SubB, Super])
      .merge(NativeTaxonomy[Super, String])

  describe(taxonomy.underlyingMap.toString()) {
    describe("when asked for subtypes of Super") {
      it("should include SubA") {
        assert(taxonomy(superTypeName).contains(subATypeName))
      }
      it("should include SubB") {
        assert(taxonomy(superTypeName).contains(subBTypeName))
      }
      it("should not include String") {
        assert(!taxonomy(superTypeName).contains(stringTypeName))
      }
    }
    describe("when asked for subtypes of another type") {
      it("should not include entries for String") {
        assert(taxonomy(stringTypeName).isEmpty)
      }
      it("should not include entries for SubA") {
        assert(taxonomy(subATypeName).isEmpty)
      }
      it("should not include entries for SubB") {
        assert(taxonomy(subBTypeName).isEmpty)
      }
    }
  }
}
