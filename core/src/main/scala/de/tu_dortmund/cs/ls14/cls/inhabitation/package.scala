package de.tu_dortmund.cs.ls14.cls

import de.tu_dortmund.cs.ls14.cls.types.{Kinding, SubtypeEnvironment, Type}

package object inhabitation {
  type Repository = Map[String, Type]
  type TreeGrammar = Map[Type, Set[(String, Seq[Type])]]

  type InhabitationAlgorithm =
    (Kinding, SubtypeEnvironment, Repository) => Type => TreeGrammar
}
