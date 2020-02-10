/*
 * Copyright 2017 Jan Bessai
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

import scala.collection.parallel.{ParSeq, ParSet}
import org.combinators.cls.compat.ParallelCollectionConverters._
import org.combinators.cls.types._
import com.typesafe.scalalogging.LazyLogging


/** Type inhabitation for finite combinatory logic (FCL) */
class FiniteCombinatoryLogic(val subtypes: SubtypeEnvironment, val repository: Repository) extends LazyLogging {
  import subtypes._
  
  type MultiArrow = (Seq[Type], Type)


  /*private final val times: scala.collection.mutable.Map[String, BigInt] =
    scala.collection.mutable.Map.empty[String, BigInt].withDefaultValue(0)*/

  private val splittedRepository: ParSeq[(String, Seq[Seq[MultiArrow]])] =
    time("splitting combinator types")(repository.view.mapValues(splitTy).toSeq.par)

  private def time[R](location: String)(x: => R): R = {
    /*val before = System.currentTimeMillis()
    val forcedResult = x
    val after = System.currentTimeMillis()
    times.synchronized(times.update(location, times(location) + (after - before)))
    debugPrint(s"$location used:  ${after - before}",  forcedResult.toString)*/
    x
  }


  private final def splitTy(ty: Type): Seq[Seq[MultiArrow]] = {
    def safeSplit[A](xss: Seq[Seq[A]]): (Seq[A], Seq[Seq[A]]) =
      xss match {
        case Seq() => (List.empty, List.empty)
        case xs +: Seq() => (xs, List.empty)
        case xs +: xsstl => (xs, xsstl)
      }
    def splitRec(ty: Type, srcs: Seq[Type], delta: Seq[Seq[(Seq[Type], Type)]]): Seq[Seq[(Seq[Type], Type)]] = {
      ty match {
        case Arrow(src, tgt) =>
          val (xs, xss) = safeSplit(delta)
          ((src +: srcs, tgt) +: xs) +: splitRec(tgt, src +: srcs, xss)
        case Intersection(sigma, tau) if sigma.isOmega =>
          splitRec(tau, srcs, delta)
        case Intersection(sigma, tau) if tau.isOmega =>
          splitRec(sigma, srcs, delta)
        case Intersection(sigma, tau) =>
          splitRec(sigma, srcs, splitRec(tau, srcs, delta))
        case _ => delta
      }
    }
    if (ty.isOmega) { List.empty }
    else List((List.empty, ty)) +: splitRec(ty, List.empty, List.empty)
  }

  private final def dcap(sigma: Type, tau: Type): Type =
    if (sigma.isSubtypeOf(tau)) sigma
    else if (tau.isSubtypeOf(sigma)) tau
    else Intersection(sigma, tau)

  private final def partitionCover(covered: Set[Type], toCover: Seq[Type]): (Seq[Type], Seq[Type]) =
    toCover.partition(covered.contains)

  private final def stillPossible(splits: Seq[(MultiArrow, Set[Type])], toCover: Seq[Type]): Boolean =
    toCover.forall(sigma => splits.exists(covered => covered._2.contains(sigma)))

  private final def mergeMultiArrow(arrow1: MultiArrow, arrow2: MultiArrow): MultiArrow =
    (arrow1._1.zip(arrow2._1).map{ case (srcs1, srcs2) => dcap(srcs1, srcs2) }, dcap(arrow1._2, arrow2._2))

  private type State = Seq[MultiArrow]
  private sealed trait CoverMachineInstruction
  private case class Cover(splits: Seq[(MultiArrow, Set[Type])], toCover: Seq[Type])
    extends CoverMachineInstruction
  private case class CheckCover(splits: Seq[(MultiArrow, Set[Type])], toCover: Seq[Type])
    extends CoverMachineInstruction
  private case class ContinueCover(
    splits: Seq[(MultiArrow, Set[Type])],
    toCover: Seq[Type],
    currentResult: MultiArrow) extends CoverMachineInstruction
  private case class CheckContinueCover(
    splits: Seq[(MultiArrow, Set[Type])],
    toCover: Seq[Type],
    currentResult: MultiArrow) extends CoverMachineInstruction

  private final def step(state: State, program: Seq[CoverMachineInstruction]): (State, Seq[CoverMachineInstruction]) = {
    program match {
      case CheckCover(splits, toCover) +: restOfProgram if stillPossible(splits, toCover) =>
        (state, Cover(splits, toCover) +: restOfProgram)
      case CheckContinueCover(splits, toCover, currentResult) +: restOfProgram if stillPossible(splits, toCover) =>
        (state, ContinueCover(splits, toCover, currentResult) +: restOfProgram)
      case ContinueCover((m, covered) +: splits, toCover, currentResult) +: restOfProgram =>
        val (freshlyCovered, uncovered) = partitionCover(covered, toCover)
        if (freshlyCovered.isEmpty) (state, ContinueCover(splits, toCover, currentResult) +: restOfProgram)
        else {
          val merged = mergeMultiArrow(currentResult, m)
          if (uncovered.isEmpty)
            (merged +: state, ContinueCover(splits, toCover, currentResult) +: restOfProgram)
          else if (merged._1 == currentResult._1) (state, ContinueCover(splits, uncovered, merged) +: restOfProgram)
          else (state, ContinueCover(splits, uncovered, merged) +: CheckContinueCover(splits, toCover, currentResult) +: restOfProgram)
        }
      case Cover((m, covered) +: splits, toCover) +: restOfProgram =>
        val (freshlyCovered, uncovered) = partitionCover(covered, toCover)
        if (freshlyCovered.isEmpty) (state, Cover(splits, toCover) +: restOfProgram)
        else if (uncovered.isEmpty) (m +: state, CheckCover(splits, toCover) +: restOfProgram)
        else (state, ContinueCover(splits, uncovered, m) +: CheckCover(splits, toCover) +: restOfProgram)
      case _ +: restOfProgram => (state, restOfProgram)
      case Seq() => (state, program)
    }
  }

  private final def coverMachine(state: State, program: Seq[CoverMachineInstruction]): State = {
    var machine = (state, program)
    while (!machine._2.isEmpty) {
      machine = step(machine._1, machine._2)
    }
    machine._1
  }

  private final def reduceMultiArrows(ms: Seq[MultiArrow]): Seq[MultiArrow] = {
    def check(lesserArgVect: MultiArrow, greaterArgVect: MultiArrow): Boolean =
      lesserArgVect._1.corresponds(greaterArgVect._1) {
        case (lesser, greater) => lesser.isSubtypeOf(greater)
      }
    def averageArgumentTypeSize(m: MultiArrow): Int = {
      if (m._1.size > 0) m._1.foldLeft(0){ case (x, y) => x + y.size } / m._1.size
      else 0
    }
    ms.sortBy(averageArgumentTypeSize) // heuristic 
      .foldLeft(Seq.empty[MultiArrow]) {
        case (result, m) if result.exists(check(m, _)) => result
        case (result, m) => m +: result.filterNot(check(_, m))
      }
  }

  private final def computeFailExisting(rules: Set[Rule], sigma: Type): (Boolean, Boolean) = {
    var toCheck: Set[Rule] = rules
    while (toCheck.nonEmpty) {
      toCheck.head match {
        case Failed(tau) if (sigma == tau) => 
          return (true, true)
        case Failed(tau) if (sigma.isSubtypeOf(tau)) => 
          return (true, toCheck.contains(Failed(sigma)))
        case Apply(target, _, tau) if (sigma == tau) =>
          return (false, true)
        case _ => toCheck = toCheck.tail
      }
    }
    (false, false)
  }

  private final def commitMultiArrow(rules: Seq[Rule], combinator: String, m: MultiArrow): Seq[Rule] = {
    var srcs = m._1
    var tgt = m._2
    var result = rules
    while (srcs.nonEmpty) {
      val src = srcs.head
      val arr = Arrow(src, tgt)
      result = Apply(tgt, arr, src) +: result
      srcs = srcs.tail
      tgt = arr
    }
    Combinator(tgt, combinator) +: result
  }

  private final def commitUpdates(rules: Seq[Rule], target: Type, combinator: String, covers: Seq[MultiArrow]): Seq[Rule] = {
    var result = rules
    var remainingCovers = covers
    while (remainingCovers.nonEmpty) {
      result = commitMultiArrow(result, combinator, (remainingCovers.head._1, target))
      remainingCovers = remainingCovers.tail
    }
    result
  }
 
  private final def dropTargets(rules: Seq[Rule]): Seq[Rule] = {
    rules.dropWhile {
      case Combinator(_, _) => false
      case _ => true
    }
  }

  private final def accumulateCovers(target: Type, toCover: Set[Type], state: (Seq[Rule], Boolean), combinator: String, combinatorType: Seq[Seq[MultiArrow]]): (Seq[Rule], Boolean) = {
    val covers = 
      coverMachine(
        Seq.empty,
        combinatorType.map(ms => Cover(ms.map(m => (m, toCover.filter(B => m._2.isSubtypeOf(B)))), toCover.toSeq))
      )
    (commitUpdates(state._1, target, combinator, reduceMultiArrows(covers)), state._2 && covers.isEmpty)
  }

  private final def inhabitCover(rules: Seq[Rule], target: Type): (Boolean, Seq[Rule]) = {
    val primeFactors: Set[Type] = Organized(target).paths.minimize.toSet
    val (todo, failed) =
      splittedRepository.par.aggregate((Seq.empty[Rule], true))(
        { case (s, (combinator, combinatorType)) => accumulateCovers(target, primeFactors, s, combinator, combinatorType) },
        { case ((rules1, failed1), (rules2, failed2)) => (rules1 ++ rules2, failed1 && failed2) }
      )
    (failed, if (failed) rules else rules ++ todo)
  }

  final private def omegaRules(target: Type): Set[Rule] = {
    splittedRepository.aggregate(Set[Rule](Apply(target, target, target)))(
      { case (rules, (combinator, _)) => rules + Combinator(target, combinator) },
      { case (rules1, rules2) => rules1.union(rules2) }
    )
  }

  final private def inhabitationStep(stable: Set[Rule], targets: Seq[Rule]): (Set[Rule], Seq[Rule]) = {
    targets match {
      case (c@Combinator(_, _)) +: restOfTargets => (stable + c, restOfTargets)
      case (app@Apply(_, _, _)) +: restOfTargets if stable.contains(app) => (stable, restOfTargets)
      case (app@Apply(sigma, tau, target)) +: restOfTargets =>
        val (failed, existing) = computeFailExisting(stable, target)
        if (failed) (if (existing) stable else stable + Failed(target), dropTargets(restOfTargets))
        else if (existing) (stable + app, restOfTargets)
        else if (target.isOmega) (stable.union(omegaRules(target)) + app, restOfTargets)
        else {
          val (inhabitFailed, nextTargets) = inhabitCover(restOfTargets, target)
          if (inhabitFailed) (stable + Failed(target), dropTargets(restOfTargets))
          else (stable + app, nextTargets)
        }
      case Failed(_) +: restOfTargets => (stable, dropTargets(restOfTargets))
      case Seq() => (stable, Seq.empty[Rule])
    }
  }

  final private def inhabitationMachine(stable: Set[Rule], targets: Seq[Rule]): Set[Rule] = {
    var state = (stable, targets)
    while (state._2.nonEmpty) {
      state = inhabitationStep(state._1, state._2)
    }
    state._1
  }

  /** Inhabits all types in targets and return a set of tree grammar rules to represent results.
    * The resulting tree grammar is pruned to eliminate unproductive derivation chains.
    */
  def inhabit(targets: Type*): Set[Rule] = {
    val resultRules =
      targets.foldLeft(Set.empty[Rule]) {
        case (results, target) =>
          if (target.isOmega) results.union(omegaRules(target))
          else {
            val (failed, existing) = computeFailExisting(results, target)
            if (failed) {
              if (existing) results else results + Failed(target)
            } else {
              val (inhabitFailed, targets) = inhabitCover(Seq.empty[Rule], target)
              if (inhabitFailed) results + Failed(target)
              else inhabitationMachine(results, targets)
            }
          }
      }
    prune(resultRules)
  }

  /** Finds all productive left hand sides in `rules`.
    * A left hand side is productive, if any of its right hand sides only requires arguments, which are productive
    * left hand sides of the grammar.
    */
  final def groundTypesOf(rules: ParSet[Rule]): ParSet[Type] = time("groundTypes") {
    def groundStep(previousGroundTypes: ParSet[Type]): ParSet[Type] = {
      rules.par.aggregate(previousGroundTypes)(
        { case (s, Apply(sigma, arr, tgt)) if s.contains(arr) && s.contains(tgt) => s + sigma
          case (s, _) => s },
        { case (s1, s2) => s1.union(s2) }
      )
    }
    var lastGround: ParSet[Type] = ParSet.empty[Type]
    var nextGround: ParSet[Type] = rules.collect { case Combinator(target, _) => target }
    while (lastGround.size < nextGround.size) {
      lastGround = nextGround
      nextGround = groundStep(lastGround)
    }
    nextGround
  }

  /** Removes all unproductive left hand sides in `rules`.
    * @see `FiniteCombinatoryLogic.groundTypesOf(Set[Rule])` for a description of productivity.
    */
  def prune(rules: Set[Rule]): Set[Rule] = time("prune") {
    val parRules = rules.par
    lazy val groundTypes = groundTypesOf(parRules)
    parRules.filter {
      case app@Apply(_, arr, tgt) if groundTypes.contains(arr) && groundTypes.contains(tgt) => true
      case Apply(_, _, _) => false
      case _ => true
    }.seq.toSet
  }
}

/** Provides a type inhabitation algorithm for finite combinatory logic (FCL) */
object FiniteCombinatoryLogic {
  def algorithm: InhabitationAlgorithm = {
    case (_, subtypes, repository) =>
      targets => new FiniteCombinatoryLogic(subtypes, repository).inhabit(targets: _*)
  }
}
