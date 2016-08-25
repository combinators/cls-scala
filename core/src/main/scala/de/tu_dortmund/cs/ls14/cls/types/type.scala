package de.tu_dortmund.cs.ls14.cls.types

sealed trait Type {
  def parens(s: String) = s"($s)"
  def toStringPrec(prec: Int): String
  override def toString(): String = toStringPrec(0)
}

case class Constructor(name: String, arguments: Type*) extends Type {
  def toStringPrec(prec: Int): String = {
    if (arguments.isEmpty) s"$name" else s"$name(${arguments.mkString(",")})"
  }
}

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
case object Omega extends Type with Organized {
  def toStringPrec(prec: Int): String = "omega"
  val paths = Stream.empty
}
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
case class Variable(name: String) extends Type {
  def toStringPrec(prec: Int): String = name
}



