package de.tu_dortmund.cs.ls14.cls.inhabitation

import de.tu_dortmund.cs.ls14.cls.types._
import shapeless.feat.Enumeration

case class Tree(name: String, arguments: Tree*)

object TreeGrammarEnumeration {
  def apply(grammar: TreeGrammar, root: Type): Enumeration[Tree] = {
    def unroll(root: Type): Enumeration[Tree] =
      grammar(root).foldLeft[Enumeration[Tree]](Enumeration.empty) {
        case (s, (name, arguments)) =>
          lazy val args =
            arguments
              .foldLeft[Enumeration[Seq[Tree]]](Enumeration.singleton(Seq())) {
              case (r, arg) => r.product(unroll(arg)).map {
                case (rs, r) => rs :+ r
              }
            }
          s.union(args.map(Tree(name, _ : _*)).pay)
      }
    unroll(root)
  }
}
