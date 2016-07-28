package de.tu_dortmund.cs.ls14.cls.interpreter

import de.tu_dortmund.cs.ls14.cls.types.Taxonomy
import ReflectedRepository._
import scala.reflect.runtime.universe._

class NativeTaxonomyBuilder(types: Set[Type] = Set.empty) {

  def addNativeTypes(tys: Set[Type]): NativeTaxonomyBuilder =
    new NativeTaxonomyBuilder(types.union(tys))

  def addNativeType(ty: Type): NativeTaxonomyBuilder =
    addNativeTypes(Set(ty))

  def addNativeType[A](implicit aTag: WeakTypeTag[A]): NativeTaxonomyBuilder =
    addNativeType(aTag.tpe)

  def taxonomy: Taxonomy = {
    def addTypeIfLte(taxonomy: Taxonomy, supertype: Type, subtype: Type) =
      if (subtype <:< supertype)
        taxonomy.merge(Taxonomy(nativeTypeOf(supertype).name).addSubtype(nativeTypeOf(subtype).name))
      else taxonomy

    types.foldLeft((Taxonomy.empty, Set.empty[Type])) {
      case ((taxonomy, inserted), ty) =>
        val tyName = nativeTypeOf(ty).name
        val newTaxonomy = inserted.foldLeft(taxonomy) {
          case (newTaxonomy, otherType) =>
            addTypeIfLte(addTypeIfLte(newTaxonomy, ty, otherType), otherType, ty)
        }
        (newTaxonomy, inserted + ty)
    }._1
  }
}

