package de.tu_dortmund.cs.ls14.cls

import de.tu_dortmund.cs.ls14.cls.types.{Kinding, SubtypeEnvironment, Type}

package object inhabitation {
  type Repository = Map[String, Type]
  type TreeGrammar = Map[Type, Set[(String, Seq[Type])]]

  def prettyPrintTreeGrammar(grammar: TreeGrammar): String = {
    grammar.map {
      case (ty, entries) =>
        val prettyEntries = entries.map {
          case (c, tgts) => s"$c${tgts.mkString("(", ",", ")")}"
        }
        s"$ty -> ${prettyEntries.mkString(" | ")}"
    }.mkString("{", "; ", "}")
  }

  type InhabitationAlgorithm =
    (Kinding, SubtypeEnvironment, Repository) => Seq[Type] => TreeGrammar
}
