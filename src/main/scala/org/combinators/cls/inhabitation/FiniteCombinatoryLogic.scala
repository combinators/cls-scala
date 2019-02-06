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

import org.combinators.cls.types._
import com.typesafe.scalalogging.LazyLogging


/** Type inhabitation for finite combinatory logic (FCL) */
class FiniteCombinatoryLogic(val subtypes: SubtypeEnvironment, val repository: Repository) extends LazyLogging {
  import subtypes._
  private final val times: scala.collection.mutable.Map[String, BigInt] =
    scala.collection.mutable.Map.empty[String, BigInt].withDefaultValue(0)


  /** An organized version of `repository` given in the constructor. */
  //private val organizedRepository: Map[String, Type with Organized] = repository.mapValues(ty => Organized(ty))

  private val splittedRepository: Map[String, (Seq[Seq[(Seq[Type], Type)]], Seq[Type])] =
    time("splitting combinator types")(repository.mapValues {ty =>
      val splits = splitTy(ty)
      val minimalTypes = splits.foldLeft[Seq[Type]](Seq.empty){
        case (s, (_, tgt) +: split) => Type.intersect(tgt +: split.map(_._2)) +: s
        case (s, Seq()) => s
      }
      (splits, minimalTypes)
    })



  private def time[R](location: String)(x: => R): R = {
    /*val before = System.currentTimeMillis()
    val forcedResult = x
    val after = System.currentTimeMillis()
    times.synchronized(times.update(location, times(location) + (after - before)))
    debugPrint(s"$location used:  ${after - before}",  forcedResult.toString)*/
    x
  }

  type MultiArrow = (Seq[Type], Type)

  private final def splitTy(ty: Type): Seq[Seq[MultiArrow]] = {
    def safeSplit[A](xss: Seq[Seq[A]]): (Seq[A], Seq[Seq[A]]) =
      xss match {
        case Seq() => (List.empty, List(List.empty))
        case xs +: Seq() => (xs, List(List.empty))
        case xs +: xsstl => (xs, xsstl)
      }
    def addToHead[A](x: A, xss: Seq[Seq[A]]): Seq[Seq[A]] =
      xss match {
        case Seq() => List(List(x))
        case xs +: xsstl => (x +: xs) +: xsstl
      }
    def splitRec(ty: Type, srcs: Seq[Type], delta: Seq[Seq[(Seq[Type], Type)]]): Seq[Seq[(Seq[Type], Type)]] = {
      ty match {
        case ty if ty.isOmega => delta
        case Arrow(src, tgt) =>
          val (xs, xss) = safeSplit(delta)
          ((src +: srcs, tgt) +: xs) +: splitRec(tgt, src +: srcs, xss)
        case Intersection(sigma, tau) =>
          splitRec(sigma, srcs, splitRec(tau, srcs, delta))
        case _ => delta
      }
    }
    if (ty.isOmega) { List.empty }
    else splitRec(ty, List.empty, List((List.empty, ty) +: List.empty))
  }

  private final def partitionCover(covered: Set[Type with Path], toCover: Seq[Type with Path]):
    (Seq[Type with Path], Seq[Type with Path]) =
    toCover.partition(covered.contains)

  private final def dcap(sigma: Type, tau: Type): Type =
    if (sigma.isSubtypeOf(tau)) sigma
    else if (tau.isSubtypeOf(sigma)) tau
    else Intersection(sigma, tau)

  private final def mergeMultiArrow(arrow: MultiArrow, srcs: Seq[Type], tgt: Type): MultiArrow =
    (arrow._1.zip(srcs).map(srcs => dcap(srcs._1, srcs._2)), Intersection(arrow._2, tgt))

  private type State = Seq[MultiArrow]
  private sealed trait Instruction
  private case class Cover(splits: Seq[(MultiArrow, Set[Type with Path])], toCover: Seq[Type with Path])
    extends Instruction
  private case class ContinueCover(
    splits: Seq[(MultiArrow, Set[Type with Path])],
    toCover: Seq[Type with Path],
    currentResult: MultiArrow) extends Instruction

  private final def step(state: State, program: Seq[Instruction]): (State, Seq[Instruction]) = {
    program match {
      case ContinueCover((m, covered) +: splits, toCover, currentResult) +: restOfProgram =>
        val (freshlyCovered, uncovered) = partitionCover(covered, toCover)
        if (freshlyCovered.isEmpty) (state, ContinueCover(splits, toCover, currentResult) +: restOfProgram)
        else {
          val merged = mergeMultiArrow(currentResult, m._1, m._2)
          if (uncovered.isEmpty)
            (merged +: state, ContinueCover(splits, toCover, currentResult) +: restOfProgram)
          else if (merged._1 == currentResult._1) (state, ContinueCover(splits, uncovered, merged) +: restOfProgram)
          else (state, ContinueCover(splits, uncovered, merged) +: ContinueCover(splits, toCover, currentResult) +: restOfProgram)
        }
      case Cover((m, covered) +: splits, toCover) +: restOfProgram =>
        val (freshlyCovered, uncovered) = partitionCover(covered, toCover)
        if (freshlyCovered.isEmpty) (state, Cover(splits, toCover) +: restOfProgram)
        else if (uncovered.isEmpty) (m +: state, Cover(splits, toCover) +: restOfProgram)
        else (state, ContinueCover(splits, uncovered, m) +: Cover(splits, toCover) +: restOfProgram)
      case ContinueCover(Seq(), _, _) +: restOfProgram => (state, restOfProgram)
      case Cover(Seq(), _) +: restOfProgram => (state, restOfProgram)
      case Seq() => (state, program)
    }
  }

  private final def minimize(s: State): State = {
    def check(lesserArgVect: MultiArrow, greaterArgVect: MultiArrow): Boolean =
      lesserArgVect._1.corresponds(greaterArgVect._1) {
        case (lesser, greater) => lesser.isSubtypeOf(greater)
      }
    s.foldLeft(Seq.empty[MultiArrow]) {
      case (result, m) if result.exists(check(m, _)) => result
      case (result, m) => m +: result.filterNot(check(_, m))
    }
  }

  private final def pruneInstructions(state: State, program: Seq[Instruction]): Seq[Instruction] = {
    program.collect {
      case i@ContinueCover(_, _, currentResult)
        if !state.exists(r => r._1.corresponds(currentResult._1){case (s, arg) => arg.isSubtypeOf(s)}) => i
      case i@Cover(_, _) => i
    }
  }


  private final def cover(splits: Seq[Seq[MultiArrow]], targets: Seq[Type with Path]): State = {
    val instructions = splits.collect { case ms =>
      Cover(ms.map(m => (m, targets.filter(m._2.isSubtypeOf).toSet)), targets)
    }
    /*val machineState: (State, Seq[Instruction]) = (List.empty, instructions)
    val res = Stream.iterate[(State, Seq[Instruction])](machineState)(x => step(x._1, x._2)).span(_._2.nonEmpty)._2.head._1
    minimize(res)*/
    //logger.debug(s"${splits.map(_.size).sum} -> ${res.size}")
    //if (res.size > 100) {
    //  logger.debug(s"${res.size} -> ${minimize(res).size}")
    //}


    var machineState: (State, Seq[Instruction]) = (Seq.empty, instructions)
    //var i: Int = 0
    while (machineState._2.nonEmpty) {
      machineState = step(machineState._1, machineState._2)
      /*i = (i + 1) % 100
      if (i == 0) {
        val prunedState = minimize(machineState._1)
        machineState = (prunedState, pruneInstructions(prunedState, machineState._2))
      }*/
    }
    minimize(machineState._1)

  }

  /** Substitutes all right hand side occurences in `grammar` of `oldType` by `newType`. */
  final def substituteArguments(grammar: TreeGrammar, oldType: Type, newType: Type): TreeGrammar =
    grammar
      .mapValues(entries =>
        entries.map {
          case (c, tgts) => (c, tgts.map(tgt => if (tgt == oldType) newType else tgt))
        })


  /** Finds all entries of `grammar` where the left hand side is a supertype of `ty`. */
  final def findSupertypeEntries(grammar: TreeGrammar, ty: Type): TreeGrammar =
    grammar.filter {
      case (k, _) => k.isSupertypeOf(ty)
    }

  /** Finds an entries of `grammar` where the left hand side is a subtype of `ty`. */
  final def findSmallerEntry(grammar: TreeGrammar, ty: Type): Option[(Type, Set[(String, Seq[Type])])] = time("findSmallerEntry") {
    grammar.find {
      case (k, _) => k.isSubtypeOf(ty)
    }
  }

  /** Rearranges intermediate results to a set of new grammar rules and recursive targets.
    * Input format: a map containing combinator names and alternative recursive targets to use that combinator.
    * Output format: a set of combinator names with the respective parameter types to inhabit for using the combinator,
    * and a collection of all parameter type collections to use the combinators in the set.
    * Example:
    * <code>
    *   newProductionsAndTargets(Map("c" -> Seq(Seq('A, 'B), Seq('C), Seq('A, 'B))), "d" -> Seq(), "e" -> Seq(Seq())) ==
    *     (Set(("c", Seq('A, 'B)), ("c", Seq('C)), ("e", Seq())),
    *      Stream.empty #:: ('C #:: Stream.empty) #:: ('A #:: 'B #:: Stream.empty) #:: Stream.empty
    * </code>
    */

  final def newProductionsAndTargets(results: scala.collection.parallel.ParIterable[(String, Iterable[Seq[Type]])]):
    (Set[(String, Seq[Type])], Stream[Stream[Type]]) = time("newProductionsAndTargets") {
    results.aggregate((Set.empty[(String, Seq[Type])], Stream.empty[Stream[Type]]))({
        case ((productions, targetLists), (combinatorName, newTargetLists)) =>
          newTargetLists.foldLeft((productions, targetLists)) {
            case ((nextProductions, nextTargetsLists), nextTargetList) =>
              (nextProductions + ((combinatorName, nextTargetList)), nextTargetList.toStream #:: nextTargetsLists)
          }
      }, {case ((rules1, tgts1), (rules2, tgts2)) => (rules1.union(rules2), tgts1 ++ tgts2) })
  }

  /** Performs a single inhabitation step.
    * Finds combinators which can inhabit `tgt`, adds their application as right hand sides for the left hand side `tgt`
    * and returns a stream of new targets for each combinator that was used.
    * Replaces all occurences of `tgt` if a (subtype-)equal left hand side is already present.
    *
    * @param result the tree grammar constructed so far.
    * @param tgt the current target.
    * @return an updated tree grammar and a stream of new targets for every new right hand side;
    *         empty target streams indicate failure, while `Stream.empty #:: Stream.empty[Stream[Type]]` indicates
    *         success without fresh targets.
    */
  final def inhabitStep(result: TreeGrammar, tgt: Type): (TreeGrammar, Stream[Stream[Type]]) = time("inhabitStep") {
    //logger.debug(s">>> Current target $tgt")
    //logger.debug(s">>> Result so far $result")
    val knownSupertypes = findSupertypeEntries(result, tgt)
    //logger.debug(s"<<<<>>>>> SupertypeEntries: $knownSupertypes")
    findSmallerEntry(knownSupertypes, tgt) match {
      case Some(kv) =>
        //logger.debug(s">>> Already present $kv")
        (substituteArguments(result, tgt, kv._1), Stream.empty #:: Stream.empty[Stream[Type]]) // replace the target everywhere
      case None =>
        val toCover = time("target organization") { Organized(tgt).paths.minimize }
        //logger.debug(s"covering: ${toCover}")
        val recursiveTargets =
          splittedRepository
            .par
            .filter { case (_, (_, minimalTypes)) => toCover.par.forall(tgt => minimalTypes.par.exists(ty => ty.isSubtypeOf(tgt))) }
            .mapValues(sigma => cover(sigma._1, toCover).map(_._1.reverse))
        //logger.debug(s"recursive targets: ${recursiveTargets}")
        val (newProductions, newTargets) = newProductionsAndTargets(recursiveTargets)
        /*newTargets.map(debugPrint(_, "Recursively inhabiting"))*/

        (result + (tgt -> newProductions), newTargets)
    }

  }

  /** Inhabits the arguments of a single combinator.
    * Sequentially performs `inhabitStep` for each target in `tgts`.
    * Aborts and rolls back, whenever a step along the way fails.
    */
  final def inhabitSequentially(grammar: TreeGrammar, tgts: Stream[Type]): (TreeGrammar, Stream[Stream[Type]]) = {
    val (newGrammar, newTgts) =
      tgts.foldLeft[(TreeGrammar, Option[Stream[Stream[Type]]])]((grammar, Some(Stream.empty))) {
        case (s@(_, None), _) => s
        case ((g, Some(result)), tgt) =>
          inhabitStep(g, tgt) match {
            case (newG, r@(_ #:: _)) => (newG, Some(result.append(r)))
            case _ => (grammar, None)
          }
      }
    (newGrammar, newTgts.getOrElse(Stream.empty))
  }

  /** Inhabits the arguments of multiple combinators.
    * Sequentially performs `inhabitSequentially` for each target stream in `tgts`.
    * Continues with the next stream when a step along the way fails.
    */
  final def inhabitSequentiallyContinueIfFailing(grammar: TreeGrammar,
    tgts: Stream[Stream[Type]]): (TreeGrammar, Stream[Stream[Type]]) = {
    tgts.foldLeft((grammar, Stream.empty[Stream[Type]])) {
      case ((g, accumulatedTgts), oldTgts) =>
        val (newGrammar, newTgts) = inhabitSequentially(g, oldTgts)
        (newGrammar, accumulatedTgts.append(newTgts))
    }
  }

  /** Inhabits all elements of `targets`.
    * Recursively performs inhabitation until there are now fresh targets.
    * Continues with the next target on failure.
    */
  final def inhabitRec(targets: Type*): Stream[(TreeGrammar, Stream[Stream[Type]])] = {
    val (steps, stable) =
      Stream
        .iterate((Map.empty[Type, Set[(String, Seq[Type])]], targets.map(_ #:: Stream.empty[Type]).toStream)) {
          case (grammar, tgts) => inhabitSequentiallyContinueIfFailing(grammar, tgts)
        }.span(_._2.nonEmpty)
    steps :+ stable.head
  }

  /** Ensures `target` is present as a left hand side of `grammar`, if a (subtype-)equal left hand side exists.
    * The preexisting left hand sides will be cloned.
    * This is necessary, when `inhabitStep` substitutes equal types.
    */
  final def ensureTargetExistsIfEqualTypePresent(grammar: TreeGrammar, target: Type): TreeGrammar = {
    if (!grammar.contains(target)) {
      grammar.keys.find(ty => target.isSubtypeOf(ty) && target.isSupertypeOf(ty)) match {
        case None => grammar
        case Some(ty) => grammar + (target -> grammar(ty))
      }
    } else grammar
  }

  /** Inhabits all types in targets and return a tree grammar to represent results.
    * The tree grammar will contain entries (sigma -> Seq((c, args), rhs)), where
    * sigma is a target type, c is a combinator name, args are the argument types necessary for c to inhabit sigma and
    * rhs are all possible other combinator name, argument type pairs for target sigma.
    * The resulting tree grammar is pruned to eliminate unproductive derivation chains.
    */
  def inhabit(targets: Type*): TreeGrammar = {
    //logger.debug(s"Repository: $repository")
    val resultGrammar = inhabitRec(targets: _*).last._1
    logger.debug("now pruning")
    //logger.debug(s"before pruning $resultGrammar")
    times.foreach(println(_))
    val resultGrammarWithAllTargets = targets.foldLeft(resultGrammar)(ensureTargetExistsIfEqualTypePresent)
    prune(resultGrammarWithAllTargets)
  }

  /** Finds all productive left hand sides in `grammar`.
    * A left hand side is productive, if any of its right hand sides only requires arguments, which are productive
    * left hand sides of the grammar.
    */
  final def groundTypesOf(grammar: TreeGrammar): Set[Type] = time("groundTypes") {
    def groundStep(previousGroundTypes: Set[Type]): Set[Type] = {
        grammar.foldLeft(previousGroundTypes) {
          case (s, (k, vs))
            if vs.exists { case (_, args) =>
              args.forall(previousGroundTypes)
            } => s + k
          case (s, _) => s
        }
      }
    lazy val groundStream = Stream.iterate(Set.empty[Type])(groundStep)
    groundStream
      .zip(groundStream.tail)
      .takeWhile{ case (oldTypes, newTypes) => newTypes.size != oldTypes.size }
      .lastOption
      .map(_._2)
      .getOrElse(Set.empty[Type])
  }

  /** Removes all unproductive left hand sides in `grammar`.
    * @see `FiniteCombinatoryLogic.groundTypesOf(TreeGrammar)` for a description of productivity.
    */
  def prune(grammar: TreeGrammar): TreeGrammar = time("prune") {
    lazy val groundTypes = groundTypesOf(grammar)
    grammar.foldLeft[TreeGrammar](Map.empty) {
      case (g, (k, vs)) =>
        val pruned = vs.filter {
          case (_, args) => args.forall(groundTypes)
        }
        if (pruned.isEmpty) g else g + (k -> pruned)
    }
  }
}

/** Provides a type inhabitation algorithm for finite combinatory logic (FCL) */
object FiniteCombinatoryLogic {
  def algorithm: InhabitationAlgorithm = {
    case (_, subtypes, repository) =>
      targets => new FiniteCombinatoryLogic(subtypes, repository).inhabit(targets: _*)
  }
}