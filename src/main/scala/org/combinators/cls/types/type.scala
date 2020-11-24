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

/** Abstract base for all intersection types. */
sealed trait Type {

  /** Parenthesizes the String `s` */
  def parens(s: String) = s"($s)"

  /** Returns the String representation of this type, parenthesized according to the precedence level `prec`.
    * Higher precedence levels call for more parenthesis.
    */
  def toStringPrec(prec: Int): String

  /** Cached precomputed string representation of this type. */
  lazy private val stringRep = toStringPrec(0)

  /** Returns the String representation of this type. */
  override def toString: String = stringRep

  /** Returns whether this type is subtype-equal to `Omega`. */
  val isOmega: Boolean

  /** Returns whether this type contains no variables. */
  val isClosed: Boolean

  /** Returns the size of the AST of this type. */
  val size: Int
}

/** Standard operations on types. */
object Type {
  def intersect(types: Seq[Type]): Type =
    types match {
      case Seq()        => Omega
      case Seq(sigma)   => sigma
      case sigma +: tys => Intersection(sigma, intersect(tys))
    }
}

/** Represents intersection type constructors. */
case class Constructor(name: String, argument: Type = Omega) extends Type {
  def this(name: String, argument: Type, arguments: Type*) = {
    this(name, arguments.foldLeft(argument)(Product))
  }

  def toStringPrec(prec: Int): String = {
    if (argument == Omega) s"$name" else s"$name($argument)"
  }

  override val isOmega: Boolean = false
  override val isClosed: Boolean = argument.isClosed
  override val size: Int = 1 + argument.size
}

object Constructor {
  def apply(name: String, argument: Type, arguments: Type*): Constructor = {
    Constructor(name, arguments.foldLeft(argument)(Product))
  }
}

/** Represents a product of two types. */
case class Product(sigma: Type, tau: Type) extends Type {
  def toStringPrec(prec: Int): String = {
    val productPrec = 9
    def productShowAssoc(ty: Type) = ty match {
      case Product(_, _) => ty.toStringPrec(productPrec)
      case _             => ty.toStringPrec(productPrec + 1)
    }
    val r = s"${productShowAssoc(sigma)} * ${productShowAssoc(tau)}"
    if (prec > productPrec) parens(r) else r
  }

  override final val isOmega: Boolean = false
  override final val isClosed: Boolean = sigma.isClosed && tau.isClosed
  override final val size: Int = 1 + sigma.size + tau.size
}

/** Represents intersections between types.
  * Intersections have precedence level 10, so all operators with levels below 10 need parenthesis
  * when converted into strings.
  */
case class Intersection(sigma: Type, tau: Type) extends Type {
  def toStringPrec(prec: Int): String = {
    val interPrec = 10
    def interShowAssoc(ty: Type) = ty match {
      case Intersection(_, _) => ty.toStringPrec(interPrec)
      case _                  => ty.toStringPrec(interPrec + 1)
    }
    val r = s"${interShowAssoc(sigma)} & ${interShowAssoc(tau)}"
    if (prec > interPrec) parens(r) else r
  }

  override final val isOmega: Boolean = sigma.isOmega && tau.isOmega
  override final val isClosed: Boolean = sigma.isClosed && tau.isClosed
  override final val size: Int = 1 + sigma.size + tau.size
}

/** The universal intersection type &omega;, which is a supertype of everything. */
case object Omega extends Type with Organized {
  def toStringPrec(prec: Int): String = "omega"

  /** Omega has no paths, so its organization is the empty intersection */
  final val paths: List[Type with Path] = List.empty

  override final val isOmega: Boolean = true
  override final val isClosed: Boolean = true
  override final val size: Int = 1
}

/** Represents arrows between types.
  * Intersections have precedence level 9, so all operators with levels below 9 need parenthesis
  * when converted into strings.
  */
case class Arrow(source: Type, target: Type) extends Type {
  def toStringPrec(prec: Int): String = {
    val arrowPrec = 8
    val r = target match {
      case Arrow(_, _) =>
        s"${source.toStringPrec(arrowPrec + 1)} -> ${target.toStringPrec(arrowPrec)}"
      case _ =>
        s"${source.toStringPrec(arrowPrec + 1)} -> ${target.toStringPrec(arrowPrec + 1)}"
    }
    if (prec > arrowPrec) parens(r) else r
  }

  override final val isOmega: Boolean = target.isOmega
  override final val isClosed: Boolean = source.isClosed && target.isClosed
  override final val size: Int = 1 + source.size + target.size
}

/** Variables in intersection type schemes. */
case class Variable(name: String) extends Type {
  def toStringPrec(prec: Int): String = name

  override final val isOmega: Boolean = false
  override final val isClosed: Boolean = false
  override final val size: Int = 1
}
