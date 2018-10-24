package org.combinators.cls.inhabitation

import org.combinators.cls.types._
import org.combinators.cls.types.syntax._

import org.scalatest.FunSpec

import scala.util.Random

class LabyrinthTest extends FunSpec {
  val labyrinthSize = 30
  val start: (Int, Int) = (Random.nextInt(labyrinthSize), Random.nextInt(labyrinthSize))
  val goal: (Int, Int) = (Random.nextInt(labyrinthSize), Random.nextInt(labyrinthSize))

  val blocked: Array[Array[Boolean]] = {
    val arr = Array.ofDim[Boolean](labyrinthSize, labyrinthSize).map(row => row.map(_ => Random.nextBoolean()))
    arr(start._2).update(start._1, false)
    arr
  }

  def intToType(x: Int): Type =
    (1 to x).foldLeft[Type]('Z)((n, _) => 'S(n))

  def anyPos(v: Variable): Kinding =
    (1 to labyrinthSize).foldLeft(Kinding(v))((k, n) => k.addOption(intToType(n)))
  val positionRow = Variable("posRow")
  val positionColumn = Variable("posCol")
  val kinding = anyPos(positionRow).merge(anyPos(positionColumn))

  val freeFields: Map[String, Type] =
    blocked.indices.foldLeft(Map.empty[String, Type]){
      case (m, row) =>
        blocked(row).indices.foldLeft(m){
          case (m, col) if !blocked(row)(col) =>
            m.updated(s"Pos_at_($row, $col)", 'Free(intToType(row), intToType(col)))
          case _ => m
        }
    }

  val movements: Map[String, Type] =
    Map(
      "start" -> 'Pos(intToType(start._1), intToType(start._2)),
      "up" -> ('Pos(positionRow, 'S(positionColumn)) =>: 'Free(positionRow, positionColumn) =>: 'Pos(positionRow, positionColumn)),
      "down" -> ('Pos(positionRow, positionColumn) =>: 'Free(positionRow, 'S(positionColumn)) =>: 'Pos(positionRow, 'S(positionColumn))),
      "left" -> ('Pos('S(positionRow), positionColumn) =>: 'Free(positionRow, positionColumn) =>: 'Pos(positionRow, positionColumn)),
      "right" -> ('Pos(positionRow, positionColumn) =>: 'Free('S(positionRow), positionColumn) =>: 'Pos('S(positionRow), positionColumn))
    )


  describe(s"Labyrinth: \n ${blocked.map(row => row.map(e => if (e) "x" else " ").mkString("|", "|", "|")).mkString("\n")}") {
    describe((movements ++ freeFields).mkString("{", "; \n", "}")) {
      val tgt = 'Pos (intToType(goal._1), intToType(goal._2))
      describe(s"|- ? : $tgt") {
        lazy val Gamma = new BoundedCombinatoryLogic(kinding, SubtypeEnvironment(Map.empty), movements ++ freeFields)
        it("should finish constructing the repository") {
          assert(Gamma.repository.nonEmpty)
        }
        lazy val results = Gamma.inhabit(tgt)
        it("should finish inhabitation") {
          assert(results.isEmpty || results.nonEmpty)
        }
      }
    }
  }
}
