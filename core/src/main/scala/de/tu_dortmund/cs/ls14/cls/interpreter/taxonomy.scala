package de.tu_dortmund.cs.ls14.cls.interpreter

import de.tu_dortmund.cs.ls14.cls.types.Taxonomy

import scala.reflect.runtime.universe._

case class TypeRelation(superType: Type, subType: Type)

trait NativeTaxonomy[A, B] {
  val taxonomy: Taxonomy
}

trait Unrelated {
  implicit def inferUnrelatedTaxonomy[A, B](implicit
    aTag: WeakTypeTag[A],
    bTag: WeakTypeTag[B]
  ): NativeTaxonomy[A, B] =
    new NativeTaxonomy[A, B] {
      lazy val taxonomy =
        Taxonomy(ReflectedRepository.nativeTypeOf[A].name)
          .merge(Taxonomy(ReflectedRepository.nativeTypeOf[B].name))
    }
}

trait SubtypeRelated extends Unrelated {
  implicit def inferSubtypeTaxonomy[A, B <: A](implicit
    aTag: WeakTypeTag[A],
    bTag: WeakTypeTag[B]
  ): NativeTaxonomy[A, B] =
    new NativeTaxonomy[A, B] {
      lazy val taxonomy =
        Taxonomy(ReflectedRepository.nativeTypeOf[A].name)
          .addSubtype(ReflectedRepository.nativeTypeOf[B].name)
    }
}

trait SupertypeRelated extends SubtypeRelated {
  implicit def inferSupertypeTaxonomy[A, B <: A](implicit
    aTag: WeakTypeTag[A],
    bTag: WeakTypeTag[B],
    inverseTaxonomy: NativeTaxonomy[B, A]
  ): NativeTaxonomy[B, A] =
    new NativeTaxonomy[B, A] {
      lazy val taxonomy =
        Taxonomy(ReflectedRepository.nativeTypeOf[A].name)
          .addSubtype(ReflectedRepository.nativeTypeOf[B].name)
          .merge(inverseTaxonomy.taxonomy)
    }
}

object NativeTaxonomy extends SupertypeRelated {
  def apply[A, B](implicit nativeTaxonomy: NativeTaxonomy[A, B]): Taxonomy = nativeTaxonomy.taxonomy
}

