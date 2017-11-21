/*
 * Copyright 2017 Jan Bessai
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

import org.combinators.cls.types.Taxonomy
import ReflectedRepository._
import scala.reflect.runtime.universe._

/** Builds a taxonomy out of native Scala types using reflection.
  * If we add A and B, we have `B in taxonomy(A) iff A <: B`.
  * Used by [[ReflectedRepository]] to automatically add subtype information, so you probably don't ever
  * have to use this.
  */
class NativeTaxonomyBuilder(types: Set[Type] = Set.empty) {

  /** Adds a set of reflected scala types to the taxonomy. */
  def addNativeTypes(tys: Set[Type]): NativeTaxonomyBuilder =
    new NativeTaxonomyBuilder(types.union(tys))

  /** Adds a reflected scala type to the taxonomy. */
  def addNativeType(ty: Type): NativeTaxonomyBuilder =
    addNativeTypes(Set(ty))

  /** Adds a set scala type to the taxonomy. */
  def addNativeType[A](implicit aTag: WeakTypeTag[A]): NativeTaxonomyBuilder =
    addNativeType(aTag.tpe)

  /** Obtains the constructed taxonomy. */
  def taxonomy: Taxonomy = {
    def addTypeIfLte(taxonomy: Taxonomy, supertype: Type, subtype: Type) =
      if (subtype <:< supertype)
        taxonomy.merge(Taxonomy(nativeTypeOf(supertype).name).addSubtype(nativeTypeOf(subtype).name))
      else taxonomy

    types.foldLeft((Taxonomy.empty, Set.empty[Type])) {
      case ((taxonomy, inserted), ty) =>
        val newTaxonomy = inserted.foldLeft(taxonomy) {
          case (newTaxonomy, otherType) =>
            addTypeIfLte(addTypeIfLte(newTaxonomy, ty, otherType), otherType, ty)
        }
        (newTaxonomy, inserted + ty)
    }._1
  }
}

