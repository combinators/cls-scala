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

package org.combinators.cls.inhabitation

import org.combinators.cls.types._
import org.combinators.cls.types.syntax._

import org.scalatest.FunSpec

import scala.util.Random

class LabyrinthTest extends FunSpec {

  val labyrinthSize = { Random.setSeed(4242); 10 }
  val start: (Int, Int) = (0, 0)
  val goal: (Int, Int) = (labyrinthSize-1, labyrinthSize-1)
  val p: Double = 1.0/4.0

  val blocked: Array[Array[Boolean]] = {
    val arr = Array.ofDim[Boolean](labyrinthSize, labyrinthSize).map(row => row.map(_ => Random.nextDouble() <= p))
    arr(start._1).update(start._2, false)
    arr(goal._1).update(goal._2, false)
    arr
  }
  
  def Pos(row: Type, column: Type): Type = Constructor("Pos", Product(row, column))
  def Free(row: Type, column: Type): Type = Constructor("Free", Product(row, column))
  def S(x: Type): Type = Constructor("S", x)
  val Z: Type = Constructor("Z")

  def intToType(x: Int): Type =
    (1 to x).foldLeft[Type](Z)((n, _) => S(n))

  def anyPos(v: Variable): Kinding =
    (0 until labyrinthSize).foldLeft(Kinding(v))((k, n) => k.addOption(intToType(n)))
  val positionRow = Variable("posRow")
  val positionColumn = Variable("posCol")
  val kinding = anyPos(positionRow).merge(anyPos(positionColumn))

  
  val freeFields: Map[String, Type] =
    (0 until labyrinthSize).flatMap(row => (0 until labyrinthSize).collect {
      case col if !blocked(row)(col) => s"Pos_at_($row, $col)" -> Free(intToType(row), intToType(col))
     }).toMap

  val movements: Map[String, Type] =
    Map(
      "start" -> Pos(intToType(start._1), intToType(start._2)),
      "up" -> (Pos(positionRow, S(positionColumn)) =>: Free(positionRow, positionColumn) =>: Pos(positionRow, positionColumn)),
      "down" -> (Pos(positionRow, positionColumn) =>: Free(positionRow, S(positionColumn)) =>: Pos(positionRow, S(positionColumn))),
      "left" -> (Pos(S(positionRow), positionColumn) =>: Free(positionRow, positionColumn) =>: Pos(positionRow, positionColumn)),
      "right" -> (Pos(positionRow, positionColumn) =>: Free(S(positionRow), positionColumn) =>: Pos(S(positionRow), positionColumn))
    )


  describe(s"Labyrinth: \n ${blocked.map(row => row.map(e => if (e) "x" else " ").mkString("|", "|", "|")).mkString("\n")}") {
    describe((movements ++ freeFields).mkString("{", "; \n", "}")) {
      val tgt = Pos(intToType(goal._1), intToType(goal._2))
      describe(s"|- ? : $tgt") {
        lazy val Gamma = new BoundedCombinatoryLogic(kinding, SubtypeEnvironment(Map.empty), movements ++ freeFields)
        it("should finish constructing the repository") {
          assert(Gamma.repository.nonEmpty)
        }
        lazy val results = Gamma.inhabit(tgt)
        it("should finish inhabitation") {
          assert(results.exists(_.target == tgt) || !results.exists(_.target == tgt))
        }
        lazy val enum = TreeGrammarEnumeration(results, tgt)
        if (results.exists(_.target == tgt)) {
          it("should find some results if the grammar is non-empty") {
            assert(enum.index(0) != null)
          }
        }
      }
    }
  }
}
