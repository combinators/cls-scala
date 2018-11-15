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
import shapeless.feat.Finite
import com.typesafe.scalalogging.LazyLogging

import scala.PartialFunction
import scala.annotation.tailrec
import scala.collection.BitSet

/** Type inhabitation for finite combinatory logic (FCL) */
class FiniteCombinatoryLogic(val subtypes: SubtypeEnvironment, val repository: Repository) extends LazyLogging {
  import subtypes._
  private final val times: scala.collection.mutable.Map[String, BigInt] =
    scala.collection.mutable.Map.empty[String, BigInt].withDefaultValue(0)


  /** An organized version of `repository` given in the constructor. */
  //private val organizedRepository: Map[String, Type with Organized] = repository.mapValues(ty => Organized(ty))

  private val splittedRepository: Map[String, Seq[Seq[(Seq[Type], Type)]]] =
    time ("splitting combinator types") ( repository.mapValues(split) )



  private def time[R](location: String)(x: => R): R = {
    /*val before = System.currentTimeMillis()
    val forcedResult = x
    val after = System.currentTimeMillis()
    times.synchronized(times.update(location, times(location) + (after - before)))
    debugPrint(s"$location used:  ${after - before}",  forcedResult.toString)*/
    x
  }

  /*sealed trait Split {
    def step: Split
    def run: Seq[Seq[(Seq[Type], Type)]] = {
      var last = this
      var next = step
      while (last != next) {
        last = next
        next = last.step
      }
      last.asInstanceOf[Result].delta
    }
  }
  case class Result(delta: Seq[Seq[(Seq[Type], Type)]]) extends Split {
    def step: Split = this
  }
  case class SplitRec(ty: Type, srcs: Seq[Type], delta: Seq[Seq[(Seq[Type], Type)]]) extends Split {
    def addToHead[A](x: A, xss: Seq[Seq[A]]): Seq[Seq[A]] =
      xss match {
        case Seq() => Seq(Seq(x))
        case xs +: xsstl => (x +: xs) +: xsstl
      }
    def safeSplit[A](xss: Seq[Seq[A]]): (Seq[A], Seq[Seq[A]]) =
      xss match {
        case Seq() => (Seq.empty, Seq(Seq.empty))
        case xs +: Seq() => (xs, Seq(Seq.empty))
        case xs +: xsstl => (xs, xsstl)
      }
    def step: Split =
      ty match {
        case ctor@Constructor(_, _) => Result(addToHead((srcs, ctor), delta))
        case p@Product(_, _) => Result(addToHead((srcs, p), delta))
        case Omega => Result(delta)
        case Arrow(_, tgt) if tgt.isOmega => Result(delta)
        case arr@Arrow(src, tgt) =>
          ContinueAfter(SplitRec(tgt, src +: srcs, delta), delta => Result(Seq((srcs, arr)) +: delta))
        case Intersection(ctor@Constructor(_, _), tau) =>
          ContinueAfter(SplitRec(tau, srcs, delta), delta => Result(addToHead((srcs, ctor), delta)))
        case Intersection(p@Product(_, _), tau) =>
          ContinueAfter(SplitRec(tau, srcs, delta), delta => Result(addToHead((srcs, p), delta)))
        case Intersection(Omega, tau) => SplitRec(tau, srcs, delta)
        case Intersection(Arrow(_, tgt), tau) if tgt.isOmega => SplitRec(tau, srcs, delta)
        case Intersection(arr@Arrow(src, tgt), tau) =>
          ContinueAfter(SplitRec(tau, srcs, delta), delta => {
            val (delta1, delta2) = safeSplit(delta)
            ContinueAfter(
              SplitRec(tgt, src +: srcs, delta2),
              delta => Result(((srcs, arr) +: delta1) +: delta)
            )
          })
        case Intersection(Intersection(sigma1, sigma2), tau) =>
          SplitRec(Intersection(sigma1, Intersection(sigma2, tau)), srcs, delta)
      }
  }
  case class ContinueAfter(task: Split, continue: Seq[Seq[(Seq[Type], Type)]] => Split) extends Split {
    def step: Split =
      task match {
        case Result(delta) => continue(delta)
        case ContinueAfter(nextTask, nextContinue) =>
          ContinueAfter(nextTask, delta => ContinueAfter(nextContinue(delta), continue))
        case _ => ContinueAfter(task.step, continue)
      }
  }*/

  private def split(ty: Type): Seq[Seq[(Seq[Type], Type)]] = {
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

  private def addToSplit(split: (Seq[Type], Type), toAdd: (Seq[Type], Type)): (Seq[Type], Type) =
    (split._1.zip(toAdd._1).map { case (src1, src2) => Intersection(src1, src2) },
      Intersection(split._2, toAdd._2))


  sealed trait Cover {
    def step: Cover
    def run: Seq[(Seq[Type], Type)] = {
      var last = this
      var next = step
      while (last != next) {
        last = next
        next = last.step
      }
      last.asInstanceOf[CoverResult].delta
    }
  }
  case class SplitCover(splits: Seq[(Seq[Type], Type, Set[Type with Path])],
    toCover: Seq[Type with Path],
    currentResult: Option[(Seq[Type], Type)],
    delta: Seq[(Seq[Type], Type)]) extends Cover {
    def changedCover(covered: Set[Type with Path], toCover: Seq[Type with Path]): Option[Seq[Type with Path]] = {
      val (removed, result) = toCover.partition(covered)
      if (removed.isEmpty) None else Some(result)
    }
    def areSubsumedBy(srcs: Seq[Type], bySrcs: Seq[Type]): Boolean =
      srcs.corresponds(bySrcs) { case (src, bySrc) => bySrc.isSubtypeOf(src) }
    def step: Cover = {
      splits match {
        case (srcs, tgt, covered) +: splitsTl =>
          (changedCover(covered, toCover), currentResult) match {
            case (Some(Seq()), None) =>
              ContinueToCover(SplitCover(splitsTl, toCover, currentResult, delta), delta => CoverResult((srcs, tgt) +: delta))
            case (Some(Seq()), Some(result)) =>
              ContinueToCover(SplitCover(splitsTl, toCover, currentResult, delta),
                delta => CoverResult(addToSplit(result, (srcs, tgt)) +: delta))
            case (Some(remaining), Some((currentSrcs, currentTgt)))
              if areSubsumedBy(srcs, currentSrcs) =>
              SplitCover(splitsTl, remaining, Some((currentSrcs, Intersection(tgt, currentTgt))), delta)
            case (Some(remaining), Some(result)) =>
              ContinueToCover(
                SplitCover(splitsTl, toCover, currentResult, delta),
                delta => SplitCover(splitsTl, remaining, Some(addToSplit(result, (srcs, tgt))), delta)
              )
            case (Some(remaining), None) =>
              ContinueToCover(
                SplitCover(splitsTl, toCover, currentResult, delta),
                delta => SplitCover(splitsTl, remaining, Some((srcs, tgt)), delta)
              )
            case (None, _) => SplitCover(splitsTl, toCover, currentResult, delta)
          }

        case Seq() => CoverResult(delta)
      }
    }

  }
  case class CoverResult(delta: Seq[(Seq[Type], Type)]) extends Cover {
    def step: Cover = this
  }
  case class ContinueToCover(task: Cover, continue: Seq[(Seq[Type], Type)] => Cover) extends Cover {
    def step: Cover =
      task match {
        case CoverResult(delta) => continue(delta)
        case ContinueToCover(nextTask, nextContinue) =>
          ContinueToCover(nextTask, delta => ContinueToCover(nextContinue(delta), continue))
        case _ => ContinueToCover(task.step, continue)
      }
  }


  private def splitCover(allSplits: Seq[Seq[(Seq[Type], Type)]], toCover: Type with Organized): Seq[(Seq[Type], Type)] = {
    val coverSet = MinimalPathSet(toCover.paths).minimize.toSet
    allSplits.foldLeft[Seq[(Seq[Type], Type)]](List.empty[(Seq[Type], Type)]) {
      case (delta, splits) =>
        val splitsWithCover =
          splits
            .map { case (srcs, tgt) => (srcs, tgt, coverSet.filter(_.isSupertypeOf(tgt))) }
            .filter { case (_, _, covered) => covered.nonEmpty }
        SplitCover(splitsWithCover, toCover.paths, None, delta).run
    }
  }

  /*private def splitCover(allSplits: Seq[Seq[(Seq[Type], Type)]], toCover: Type with Organized): Seq[(Seq[Type], Type)] =
    time("computing covers") {

    def subsumedResult(options: Seq[(Seq[Type], Type)], subsumedBy: (Seq[Type], Type)): (Boolean, (Seq[Type], Type)) = {
      options match {
        case Seq() => (false, subsumedBy)
        case (args, tgt) +: otherOptions
          if args.corresponds(subsumedBy._1) { case (arg, subsuming) => subsuming.isSupertypeOf(arg) } =>
          val continue = if (subsumedBy._2.isSubtypeOf(tgt)) subsumedBy else (subsumedBy._1, Intersection(subsumedBy._2, tgt))
          (true, subsumedResult(otherOptions, continue)._2)
        case _ +: otherOptions =>
          subsumedResult(otherOptions, subsumedBy)
      }
    }

    def splitCoverRec(toCover: Seq[(Type with Path, Seq[(Seq[Type], Type)])], currentResult: (Seq[Type], Type), delta: Seq[(Seq[Type], Type)]): Seq[(Seq[Type], Type)] = {
      toCover match {
        case Seq() => currentResult +: delta
        case (path, _) +: restToCover if currentResult._2.isSubtypeOf(path) =>
          splitCoverRec(restToCover, currentResult, delta)
        case (path, options) +: restToCover =>
          val (redundant, nextResult) = subsumedResult(options, currentResult)
          if (redundant) splitCoverRec(restToCover, nextResult, delta)
          else {
            options.foldLeft(delta){ case (s, option) =>
              val nextResult = addToSplit(currentResult, option)
              splitCoverRec(restToCover, nextResult, s) }

          }
      }
    }

    allSplits.foldLeft[Seq[(Seq[Type], Type)]](List.empty){
      case (delta, splits) => {
        val pathsWithSplits = toCover.paths.map(path => (path, splits.filter(_._2.isSubtypeOf(path))))
        if (pathsWithSplits.isEmpty || pathsWithSplits.exists(_._2.isEmpty)) delta else {
          pathsWithSplits.head._2.foldLeft(delta) {
            case (nextDelta, option) => splitCoverRec(pathsWithSplits.tail, option, nextDelta)
          }
        }
      }
    }
  }*/
  /*private def splitCover(allSplits: Seq[Seq[(Seq[Type], Type)]], toCover: Type with Organized): Seq[(Seq[Type], Type)] =
  time("computing covers") {
    def splitCoverRec(
      splits: Seq[(Seq[Type], Type)],
      toCover: Seq[(Type with Path, BitSet)],
      currentResult: (Seq[Type], Type),
      delta: Seq[(Seq[Type], Type)]): Seq[(Seq[Type], Type)] = {
      toCover match {
        case Seq() => currentResult +: delta
        case (path, options) +: restToCover =>
          options.foldLeft(delta) {
            case (nextDelta, idx) =>
              val split = splits(idx)
              /*val redundant =
                split._1.corresponds(currentResult._1)((arg, existingArg) => arg.isSubtypeOf(existingArg)) &&
                  currentResult._2.isSubtypeOf(split._2)*/
              /*val potentialNextToCover =
                restToCover.foldLeft[Option[Seq[(Type with Path, BitSet)]]](Some(List.empty)) {
                  case (None, _) => None
                  case (rest, (_, coveredBy)) if coveredBy(idx) => rest
                  case (Some(rest), (nextPath, coveredBy)) =>
                    val coveredByRemaining = coveredBy.&~(options)
                    if (coveredByRemaining.isEmpty) None
                    else Some((nextPath, coveredByRemaining) +: rest)
                }
              potentialNextToCover.map { nextToCover =>
                /*if (redundant) { println("skippio"); splitCoverRec(splits, nextToCover, currentResult, nextDelta) }
                else*/ splitCoverRec(splits, nextToCover, addToSplit(currentResult, split), nextDelta)
              }.getOrElse(nextDelta)*/
            val potentialNextToCover =
                restToCover.par.filterNot(_._2(idx)).map { case (p, cs) => (p, cs.&~(options)) }.seq
            if (potentialNextToCover.exists(_._2.isEmpty)) nextDelta
            else splitCoverRec(splits, potentialNextToCover, addToSplit(currentResult, split), nextDelta)
          }
      }
    }

    allSplits.map(_.toVector).foldLeft[Seq[(Seq[Type], Type)]](List.empty){
      case (delta, splits) => {
        val pathsWithSplits =
          toCover.paths.map(path =>
            (path, BitSet(splits.zipWithIndex.collect {
              case (split, idx) if split._2.isSubtypeOf(path) => idx
            } : _*)))
        if (pathsWithSplits.isEmpty || pathsWithSplits.exists(_._2.isEmpty)) delta else {
          pathsWithSplits.head._2.foldLeft(delta) {
            case (nextDelta, idx) =>
              val split = splits(idx)
              /*val potentialNextToCover =
                pathsWithSplits.tail.foldLeft[Option[Seq[(Type with Path, BitSet)]]](Some(List.empty)) {
                  case (None, _) => None
                  case (rest, (_, coveredBy)) if coveredBy(idx) => rest
                  case (Some(rest), (nextPath, coveredBy)) =>
                    val coveredByRemaining = coveredBy.&~(pathsWithSplits.head._2)
                    if (coveredByRemaining.isEmpty) None
                    else Some((nextPath, coveredByRemaining) +: rest)
                }
              potentialNextToCover
                .map(nextToCover => splitCoverRec(splits, nextToCover, split, nextDelta))
                .getOrElse(nextDelta)*/
              val potentialNextToCover =
                pathsWithSplits.tail.par.filterNot(_._2(idx)).map { case (p, cs) => (p, cs.&~(pathsWithSplits.head._2)) }.seq
              if (potentialNextToCover.exists(_._2.isEmpty)) nextDelta
              else splitCoverRec(splits, potentialNextToCover, split, nextDelta)
          }
        }
      }
    }
  }*/


  /** Splits `path` into (argument, target)-pairs, relevant for inhabiting `target`.
    * Relevant target components are path suffixes, which are supertypes of `target`.
    * <code>
    *   relevantFor('Int :&: ('Float =>: 'Int), 'String =>: 'Float =>: 'Int) =
    *     Seq((Seq('String, 'Float), 'Int), (Seq('String), 'Float =>: 'Int))
    * </code>
    */
  private def relevantFor(target: Type with Organized,
    path: Type with Organized with Path): Seq[(Seq[Type], Type with Path)] = time("relevantFor") {
    path match {
      case Path(args, tgt) =>
        args
          .inits
          .toSeq
          .zip(args.tails.toSeq.reverse.map(Path(_, tgt)))
          .filter { case (_, selected) => target.paths.exists(tgtP => tgtP.isSupertypeOf(selected)) }
    }
  }

  /**
    * Computes the piecewise intersection of argument types.
    * Assumes all elements of `arguments` are of the same length.
    * Example:
    * <code>
    *   intersectArguments(Seq('A, 'B =>: 'C :&: 'D), Seq('B, 'B =>: 'C :&: 'E)) =
    *     Seq('A :&: 'B, ('B =>: 'C) :&: ('B =>: 'D) :&: ('B =>: 'E))
    * </code>
    */
  private def intersectArguments(arguments: Seq[Seq[Type]]): Seq[Type with Organized] = time("intersecting arguments") {
    if (arguments.isEmpty) Seq.empty else {
      arguments.tail.aggregate(arguments.head.map(Organized(_)))(
        (xs, ys) => Organized.intersectPiecewise(xs, ys.map(Organized(_))),
        Organized.intersectPiecewise
      )
    }
  }

  /** Typeclass instance to minimize an argument collection wrt. to its cardinality under the constraint
    * that inhabitant sets remain equal.
    * We have:
    * <code>
    *   forall argvect in args,
    *     exists argvect' in args.minimize,
    *       forall i, argvect(i) <= argvect'(i)
    * </code>
    * Example:
    * <code>
    *   Seq(Seq('A :&: 'B, 'C), Seq('A, 'D), Seq('A :&: 'B, 'C :&: D)).minimize =
    *     Seq(Seq('A :&: 'B, 'C), Seq('A, 'D))
    * </code>
    */
  implicit class MinimalArguments(args: Seq[Seq[Type]]) extends Minimizable {
    type T = Seq[Type]
    def minimize: Seq[T] = {
      def checkArgvectorRelation(lesserArgVect: Seq[Type], greaterArgVect: Seq[Type]): Boolean =
        lesserArgVect.corresponds(greaterArgVect) {
          case (lesserArg, greaterArg) => lesserArg.isSubtypeOf(greaterArg)
        }
      args.foldLeft(Seq.empty[Seq[Type]]) {
        case (result, argVect) if result.exists(checkArgvectorRelation(argVect, _)) => result
        case (result, argVect) => argVect +: result.filterNot(checkArgvectorRelation(_, argVect))
      }
    }
  }

  /**
    * Finds all piecewise intersections of argument sequences in `paths`, such that
    * correspondingly intersected targets are subtypes of `tgt`.
    * Avoids selecting redundant paths and argument vectors.
    * Assumes all argument sequences in `paths` are of equal length.
    * Example:
    * <code>
    *   covers('A :&: 'B,
    *     (Seq('X, P), 'A) +: (Seq('Y, Q), 'A) +: (Seq('Z, P), 'B) +: Seq.empty) ==
    *     Seq('X :&: 'Z, P) +: Seq('Y :&: 'Z, P) +: Seq.empty
    * </code>
    */
  final def covers(tgt: Type with Organized, paths: Seq[(Seq[Type], Type with Path)]): Seq[Seq[Type]] = time("covers") {
    val coveringArgs: Iterable[Finite[Seq[Type]]] =
      tgt.paths.foldLeft(Map.empty[Type with Path, Seq[Seq[Type]]]) {
        case (r, toCover) =>
          r.updated(toCover, paths.foldLeft(Seq.empty[Seq[Type]]) {
            case (s, (args, tgt)) if tgt.isSubtypeOf(toCover) => args +: s
            case (s, _) => s
          })
      }.values
        .map(xs => xs.aggregate(Finite.empty[Seq[Type]])((xs, x) => xs.:+:(Finite.singleton(x)), (x, y) => x :+: y))

    if (coveringArgs.isEmpty || coveringArgs.exists(_.cardinality == 0)) Seq.empty else {
      coveringArgs.view.tail.foldLeft(coveringArgs.head.map(Seq(_))) {
        case (s, args) => (args :*: s).map { case (x, y) => x +: y }
      }.map(intersectArguments)
        .values
        .minimize
    }
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

  final def newProductionsAndTargets(results: Map[String, Iterable[Seq[Type]]]):
    (Set[(String, Seq[Type])], Stream[Stream[Type]]) = time("newProductionsAndTargets") {
    results.foldLeft((Set.empty[(String, Seq[Type])], Stream.empty[Stream[Type]])){
        case ((productions, targetLists), (combinatorName, newTargetLists)) =>
          newTargetLists.foldLeft((productions, targetLists)) {
            case ((nextProductions, nextTargetsLists), nextTargetList) =>
              (nextProductions + ((combinatorName, nextTargetList)), nextTargetList.toStream #:: nextTargetsLists)
          }
      }
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
    logger.debug(s">>> Current target $tgt")
    logger.debug(s">>> Result so far $result")
    val knownSupertypes = findSupertypeEntries(result, tgt)
    logger.debug(s"<<<<>>>>> SupertypeEntries: $knownSupertypes")
    findSmallerEntry(knownSupertypes, tgt) match {
      case Some(kv) =>
        logger.debug(s">>> Already present $kv")
        (substituteArguments(result, tgt, kv._1), Stream.empty #:: Stream.empty[Stream[Type]]) // replace the target everywhere
      case None =>
        val orgTgt = time("target organization") { Organized.intersect(Organized(tgt).paths.minimize) }

        /*val recursiveTargets: Map[String, Iterable[Seq[Type]]] =
          organizedRepository.par.mapValues { cType =>
            /*debugPrint(orgTgt, "Covering component")
            debugPrint(cType.paths, "Using paths")*/
            val relevant = cType.paths
              .flatMap(relevantFor(orgTgt, _))
              /*.map(debugPrint(_, "Of those are relevant"))*/
            if (orgTgt.paths.exists(tgtP => !relevant.exists(r => r._2.isSubtypeOf(tgtP)))) {
              Iterable.empty
            } else {
              relevant
                .groupBy(x => x._1.size)
                .mapValues(pathComponents => covers(orgTgt, pathComponents))
                .values.flatten
            }
          }.toMap.seq*/
        val recursiveTargets =
          splittedRepository.par.mapValues { cType =>
            splitCover(cType, orgTgt).map(_._1.reverse).minimize
          }.toMap.seq
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
    logger.debug(s"Repository: $repository")
    val resultGrammar = inhabitRec(targets: _*).last._1
    logger.debug(s"before pruning $resultGrammar")
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