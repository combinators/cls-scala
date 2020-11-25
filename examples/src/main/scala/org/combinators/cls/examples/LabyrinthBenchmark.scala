package org.combinators.cls.examples

import org.combinators.cls.inhabitation.{
  BoundedCombinatoryLogic,
  TreeGrammarEnumeration
}
import org.combinators.cls.types._
import org.combinators.cls.types.syntax._

object LabyrinthBenchmark extends App {
  val startTime = System.currentTimeMillis()
  val labyrinthSize = 30
  val start: (Int, Int) = (0, 0)
  val goal: (Int, Int) = (labyrinthSize - 1, labyrinthSize - 1)

  val free: Array[Array[Boolean]] = {
    val arr = Array[Array[Boolean]](
      Array(true, false, true, true, true, true, true, false, true, true, true,
        true, true, false, true, false, true, true, true, false, true, true,
        true, false, true, true, true, false, true, true),
      Array(true, true, true, true, true, true, true, true, true, false, true,
        false, false, true, true, false, false, true, true, true, true, true,
        true, true, true, true, false, false, false, true),
      Array(true, true, true, true, true, true, true, false, true, false, true,
        true, true, true, false, true, true, true, true, true, false, false,
        true, true, false, true, true, false, true, false),
      Array(true, true, true, true, true, false, true, true, false, false, true,
        true, true, true, true, true, true, true, true, true, true, true, true,
        true, false, true, true, true, true, false),
      Array(true, true, true, true, true, false, true, true, false, false, true,
        false, true, false, true, true, false, true, true, true, true, true,
        true, false, true, true, true, true, true, false),
      Array(true, true, false, false, true, true, true, true, false, true, true,
        true, true, true, false, true, true, true, false, true, true, true,
        true, true, false, true, true, true, true, true),
      Array(true, true, true, false, true, false, false, true, true, false,
        true, true, true, false, true, true, true, true, true, true, true, true,
        true, true, true, true, true, true, true, true),
      Array(true, true, true, false, true, false, false, true, true, false,
        false, false, true, true, true, true, false, true, true, true, false,
        true, true, true, true, false, true, true, false, true),
      Array(true, true, true, false, true, true, false, true, true, false, true,
        false, true, true, false, false, false, true, false, true, true, true,
        true, true, false, false, true, false, true, true),
      Array(true, false, false, true, true, true, true, true, true, true, true,
        true, false, true, true, true, false, true, true, true, true, true,
        true, true, false, false, true, false, true, true),
      Array(true, false, true, false, true, true, false, true, true, true, true,
        true, true, true, true, false, true, true, true, false, true, false,
        true, false, true, true, false, false, true, false),
      Array(true, true, true, false, true, true, true, true, true, false, true,
        true, true, true, true, false, true, true, false, false, true, false,
        true, true, true, true, true, true, true, true),
      Array(true, false, true, true, false, false, true, false, false, true,
        true, true, false, true, true, true, true, true, false, true, true,
        true, true, true, true, true, true, true, true, true),
      Array(true, true, true, false, true, true, false, true, true, true, true,
        true, true, true, true, true, false, true, true, true, true, true, true,
        true, true, true, false, true, true, true),
      Array(false, true, true, false, true, true, false, true, true, true, true,
        true, false, true, true, false, true, true, true, false, true, true,
        true, false, true, true, true, true, true, true),
      Array(true, false, false, true, true, true, false, false, true, true,
        false, false, true, true, false, true, true, false, false, true, true,
        true, true, true, true, true, false, true, true, true),
      Array(true, true, false, false, true, true, true, true, true, true, true,
        false, true, true, true, true, true, true, true, true, true, true, true,
        true, true, false, true, true, true, true),
      Array(true, true, true, true, true, true, true, true, true, true, false,
        false, true, true, true, false, false, false, true, false, true, true,
        true, true, true, true, true, true, true, true),
      Array(true, true, true, true, true, false, true, true, true, false, true,
        false, false, true, true, true, true, true, false, true, true, true,
        true, true, true, true, true, false, false, true),
      Array(false, false, true, true, false, true, false, true, true, false,
        false, true, false, true, true, true, true, true, true, true, true,
        true, true, true, true, false, true, true, true, true),
      Array(true, true, true, false, false, false, true, false, true, true,
        true, true, true, false, true, true, true, true, true, true, true, true,
        false, true, false, true, true, true, true, false),
      Array(false, true, true, true, true, true, true, true, true, true, true,
        false, true, true, true, true, true, true, true, true, true, true, true,
        true, true, false, true, false, false, true),
      Array(true, true, true, true, true, false, true, true, true, true, true,
        true, true, true, false, true, false, true, true, false, true, true,
        true, true, true, true, true, true, true, true),
      Array(true, true, true, false, true, true, true, true, false, true, true,
        false, true, true, true, true, true, false, true, true, false, true,
        true, true, true, false, true, false, true, true),
      Array(true, true, true, true, true, true, true, false, true, true, true,
        true, true, true, true, true, true, false, true, true, true, true, true,
        true, true, false, false, true, true, true),
      Array(true, true, true, false, true, true, false, true, true, true, true,
        false, true, true, true, true, true, true, false, false, true, true,
        true, true, false, true, true, true, false, true),
      Array(false, false, true, true, false, true, true, true, true, false,
        true, true, true, false, true, true, false, false, true, true, true,
        true, true, true, true, true, false, false, false, true),
      Array(true, true, true, true, true, true, true, false, true, false, true,
        false, true, false, true, true, false, false, false, true, true, true,
        true, true, true, true, true, true, true, true),
      Array(true, true, false, true, false, true, true, true, true, true, false,
        true, true, true, true, true, false, true, true, false, false, true,
        false, true, false, true, true, false, true, true),
      Array(true, true, true, true, true, false, true, true, false, false,
        false, true, true, false, false, true, false, false, true, true, true,
        false, true, true, true, true, true, true, true, true)
    )
    arr
  }

  object SemanticTypes {
    val Z = Constructor("Z")
    def S(of: Type): Type = Constructor("S", of)
    def Free(posRow: Type, posCol: Type): Type =
      Constructor("Free", posRow, posCol)
    def Pos(row: Type, col: Type): Type = Constructor("Pos", row, col)
  }
  import SemanticTypes._

  def intToType(x: Int): Type =
    (1 to x).foldLeft[Type](Z)((n, _) => S(n))

  def anyPos(v: Variable): Kinding =
    (0 until labyrinthSize).foldLeft(Kinding(v))((k, n) =>
      k.addOption(intToType(n))
    )
  val positionRow = Variable("posRow")
  val positionColumn = Variable("posCol")
  val kinding = anyPos(positionRow).merge(anyPos(positionColumn))

  val freeFields: Map[String, Type] =
    (0 until labyrinthSize)
      .flatMap(row =>
        (0 until labyrinthSize).collect {
          case col if free(row)(col) =>
            s"Pos_at_($row, $col)" -> Free(intToType(row), intToType(col))
        }
      )
      .toMap

  val movements: Map[String, Type] =
    Map(
      "start" -> Pos(intToType(start._1), intToType(start._2)),
      "up" -> (Pos(positionRow, S(positionColumn)) =>: Free(
        positionRow,
        positionColumn
      ) =>: Pos(positionRow, positionColumn)),
      "down" -> (Pos(positionRow, positionColumn) =>: Free(
        positionRow,
        S(positionColumn)
      ) =>: Pos(positionRow, S(positionColumn))),
      "left" -> (Pos(S(positionRow), positionColumn) =>: Free(
        positionRow,
        positionColumn
      ) =>: Pos(positionRow, positionColumn)),
      "right" -> (Pos(positionRow, positionColumn) =>: Free(
        S(positionRow),
        positionColumn
      ) =>: Pos(S(positionRow), positionColumn))
    )

  println(
    s"Labyrinth: \n ${free.map(row => row.map(e => if (!e) "x" else " ").mkString("|", "|", "|")).mkString("\n")}"
  )
  println((movements ++ freeFields).mkString("{", "; \n", "}"))
  val tgt = Pos(intToType(goal._1), intToType(goal._2))
  println(s"|- ? : $tgt")
  lazy val Gamma = new BoundedCombinatoryLogic(
    kinding,
    SubtypeEnvironment(Map.empty),
    movements ++ freeFields
  )
  lazy val results = Gamma.inhabit(tgt)
  println("Grammar:")
  println(results.mkString("\n"))
  println("Results 1 - 3:")
  val enum = TreeGrammarEnumeration(results, tgt)
  println(enum.index(0))
  println(enum.index(1))
  println(enum.index(2))
  println(s"TotalTime: ${System.currentTimeMillis() - startTime} ms")
}
