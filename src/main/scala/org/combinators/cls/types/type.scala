package org.combinators.cls.types

/** Abstract base for all intersection types. */
sealed trait Type {
  /** Parenthesizes the String `s` */
  def parens(s: String) = s"($s)"
  /** Returns the String representation of this type, parenthesized according to the precedence level `prec`.
    * Higher precedence levels call for more parenthesis.
    */
  def toStringPrec(prec: Int): String

  /** Returns the String representation of this type. */
  override def toString: String = toStringPrec(0)
}

/** Represents intersection type constructors. */
case class Constructor(name: String, arguments: Type*) extends Type {
  def toStringPrec(prec: Int): String = {
    if (arguments.isEmpty) s"$name" else s"$name(${arguments.mkString(",")})"
  }
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
      case _ => ty.toStringPrec(interPrec + 1)
    }
    val r = s"${interShowAssoc(sigma)} & ${interShowAssoc(tau)}"
    if (prec > interPrec) parens(r) else r
  }
}

/** The universal intersection type &omega;, which is a supertype of everything. */
case object Omega extends Type with Organized {
  def toStringPrec(prec: Int): String = "omega"

  /** Omega has no paths, so its organization is the empty intersection */
  val paths: Stream[Type with Path] = Stream.empty
}

/** Represents arrows between types.
  * Intersections have precedence level 9, so all operators with levels below 9 need parenthesis
  * when converted into strings.
  */
case class Arrow(source: Type, target: Type) extends Type {
  def toStringPrec(prec: Int): String = {
    val arrowPrec = 9
    val r = target match {
      case Arrow(_, _) => s"${source.toStringPrec(arrowPrec + 1)} -> ${target.toStringPrec(arrowPrec)}"
      case _ => s"${source.toStringPrec(arrowPrec + 1)} -> ${target.toStringPrec(arrowPrec + 1)}"
    }
    if (prec > arrowPrec) parens(r) else r
  }
}

/** Variables in intersection type schemes. */
case class Variable(name: String) extends Type {
  def toStringPrec(prec: Int): String = name
}



