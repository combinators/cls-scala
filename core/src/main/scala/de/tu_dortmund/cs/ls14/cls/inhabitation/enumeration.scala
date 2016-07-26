package de.tu_dortmund.cs.ls14.cls.inhabitation

import de.tu_dortmund.cs.ls14.cls.types._
import shapeless.feat.Enumeration
import scala.collection.mutable

case class Tree(name: String, arguments: Tree*)

trait TreeGrammarEnumeration {
  val fromGrammar: TreeGrammar

  lazy val enumerationMap: Type => Enumeration[Tree] =
    new mutable.HashMap[Type, Enumeration[Tree]] {
      override def apply(entry: Type): Enumeration[Tree] =
        getOrElseUpdate(entry, {
          fromGrammar.getOrElse(entry, Seq.empty).foldLeft[Enumeration[Tree]](Enumeration.empty) { case (enum, (combinator, args)) =>
            val argEnum =
              args.foldRight[Enumeration[Seq[Tree]]](Enumeration.singleton(Seq.empty)) { (arg, enum) =>
                val recArgs = enumerationMap(arg).pay
                enum.product(recArgs).map { case (args, arg) => arg +: args }
              }
            val terms = argEnum.map((args: Seq[Tree]) => Tree(combinator, args: _*)).pay
            enum.union(terms)
          } }
        )
    }
}

object TreeGrammarEnumeration {
  def apply(grammar: TreeGrammar, root: Type): Enumeration[Tree] = {
    if (!grammar.contains(root)) Enumeration.empty
    else new TreeGrammarEnumeration { lazy val fromGrammar = grammar }.enumerationMap(root)
  }
}
