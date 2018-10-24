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

import scala.annotation.tailrec

/** Type inhabitation for finite combinatory logic (FCL) */
class FiniteCombinatoryLogic(val subtypes: SubtypeEnvironment, val repository: Repository) {

  import subtypes._



  /** An organized version of `repository` given in the constructor. */
  //private val organizedRepository: Map[String, Type with Organized] = repository.mapValues(ty => Organized(ty))

  private val splittedRepository: Map[String, Seq[Seq[(Seq[Type], Type)]]] = repository.mapValues(split)

  /** Prints a debug message. */
  private final def debugPrint[A](x: A, msg: String = ""): A = {
    //println(s"$msg : ${ if (x.isInstanceOf[Stream[_]]) x.asInstanceOf[Stream[_]].toList.toString else x.toString}")
    x
  }

  private final val times: scala.collection.mutable.Map[String, BigInt] =
    scala.collection.mutable.Map.empty[String, BigInt].withDefaultValue(0)
  private def time[R](location: String)(x: => R) = {
    /*val before = System.currentTimeMillis()
    val forcedResult = x
    val after = System.currentTimeMillis()
    times.synchronized(times.update(location, times(location) + (after - before)))
    debugPrint(s"$location used:  ${after - before}",  forcedResult)*/
    x
  }

  private def split(ty: Type): Seq[Seq[(Seq[Type], Type)]] = {
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
    def splitRec(ty: Type, srcs: Seq[Type], delta: Seq[Seq[(Seq[Type], Type)]]): Seq[Seq[(Seq[Type], Type)]] =
      ty match {
        case ctor@Constructor(_, _) => addToHead((srcs, ctor), delta)
        case p@Product(_, _) => addToHead((srcs, p), delta)
        case Omega => delta
        case Arrow(_, tgt) if tgt.isOmega => delta
        case arr@Arrow(src, tgt) =>
          Seq((srcs, arr)) +: splitRec(tgt, src +: srcs, delta)
        case Intersection(ctor@Constructor(_, _), tau) =>
          addToHead((srcs, ctor), splitRec(tau, srcs, delta))
        case Intersection(p@Product(_, _), tau) => addToHead((srcs, p), splitRec(tau, srcs, delta))
        case Intersection(Omega, tau) => splitRec(tau, srcs, delta)
        case Intersection(Arrow(_, tgt), tau) if tgt.isOmega => splitRec(tau, srcs, delta)
        case Intersection(arr@Arrow(src, tgt), tau) =>
          val (delta1, delta2) = safeSplit(splitRec(tau, srcs, delta))
          ((srcs, arr) +: delta1) +: splitRec(tgt, src +: srcs, delta2)
        case Intersection(Intersection(sigma1, sigma2), tau) =>
          splitRec(Intersection(sigma1, Intersection(sigma2, tau)), srcs, delta)
      }
    splitRec(ty, Seq.empty, Seq(Seq.empty))
    /*
    @tailrec def splitRec(
      ty: Type, srcs: Seq[Type],
      delta: Seq[Seq[(Seq[Type], Type)]],
      k: Seq[Seq[(Seq[Type], Type)]] => Seq[Seq[(Seq[Type], Type)]]): Seq[Seq[(Seq[Type], Type)]] =
      ty match {
        case ctor@Constructor(_, _) => k(addToHead((srcs, ctor), delta))
        case p@Product(_, _) => k(addToHead((srcs, p), delta))
        case Omega => k(delta)
        case Arrow(_, tgt) if tgt.isOmega => k(delta)
        case arr@Arrow(src, tgt) =>
          k(Seq((srcs, arr)) +: splitRec(tgt, src +: srcs, delta, x => x))
        case Intersection(ctor@Constructor(_, _), tau) =>
          splitRec(tau, srcs, delta, r => k(addToHead((srcs, ctor), r)))
        case Intersection(p@Product(_, _), tau) =>
          splitRec(tau, srcs, delta, r => k(addToHead((srcs, p), r)))
        case Intersection(Omega, tau) => splitRec(tau, srcs, delta, k)
        case Intersection(Arrow(_, tgt), tau) if tgt.isOmega => splitRec(tau, srcs, delta, k)
        case Intersection(arr@Arrow(src, tgt), tau) =>
          splitRec(tau, srcs, delta, r => k {
            val (delta1, delta2) = safeSplit(r)
            ((srcs, arr) +: delta1) +: splitRec(tgt, src +: srcs, delta2, x => x)
          })
        case Intersection(Intersection(sigma1, sigma2), tau) =>
          splitRec(Intersection(sigma1, Intersection(sigma2, tau)), srcs, delta, k)
      }
    splitRec(ty, Seq.empty, Seq(Seq.empty), x => x)*/
  }

  private def addToSplit(split: (Seq[Type], Type), toAdd: (Seq[Type], Type)): (Seq[Type], Type) =
    (split._1.zip(toAdd._1).map { case (src1, src2) => Intersection(src1, src2) },
      Intersection(split._2, toAdd._2))

  private def splitCover(allSplits: Seq[Seq[(Seq[Type], Type)]], toCover: Type with Organized): Seq[(Seq[Type], Type)] = {
    def changedCover(tgt: Type, toCover: Seq[Type with Path]): Option[Seq[Type with Path]] = {
      val (result, removed) = toCover.partition(!_.isSupertypeOf(tgt))
      if (removed.isEmpty) None else Some(result)
    }

    def areSubsumedBy(srcs: Seq[Type], bySrcs: Seq[Type]): Boolean =
      srcs.zip(bySrcs).forall { case (src, bySrc) => bySrc.isSubtypeOf(src) }

    def splitCoverRec(
      splits: Seq[(Seq[Type], Type)],
      toCover: Seq[Type with Path],
      currentResult: Option[(Seq[Type], Type)],
      delta: Seq[(Seq[Type], Type)]): Seq[(Seq[Type], Type)] =
      splits match {
        case (srcs, tgt) +: splitsTl =>
          (changedCover(tgt, toCover), currentResult) match {
            case (Some(Seq()), None) =>
              (srcs, tgt) +: splitCoverRec(splitsTl, toCover, currentResult, delta)
            case (Some(Seq()), Some(result)) =>
              addToSplit(result, (srcs, tgt)) +: splitCoverRec(splitsTl, toCover, currentResult, delta)
            case (Some(remaining), Some((currentSrcs, currentTgt)))
              if areSubsumedBy(srcs, currentSrcs) =>
              splitCoverRec(splitsTl, remaining, Some((currentSrcs, Intersection(tgt, currentTgt))), delta)
            case (Some(remaining), Some(result)) =>
              splitCoverRec(splitsTl, remaining, Some(addToSplit(result, (srcs, tgt))),
                splitCoverRec(splitsTl, toCover, currentResult, delta))
            case (Some(remaining), None) =>
              splitCoverRec(splitsTl, remaining, Some((srcs, tgt)),
                splitCoverRec(splitsTl, toCover, currentResult, delta))
            case (None, _) => splitCoverRec(splitsTl, toCover, currentResult, delta)
          }
        case Seq() => delta
      }

    allSplits.foldLeft(Seq.empty[(Seq[Type], Type)]) {
      case (delta, splits) => splitCoverRec(splits, toCover.paths, None, delta)
    }
  }



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
  implicit class MinimalArguments(args: Seq[Seq[Type with Organized]]) extends Minimizable {
    type T = Seq[Type with Organized]
    def minimize: Seq[T] = {
      def checkArgvectorRelation(lesserArgVect: Seq[Type with Organized], greaterArgVect: Seq[Type with Organized]): Boolean =
        lesserArgVect.corresponds(greaterArgVect) {
          case (lesserArg, greaterArg) => lesserArg.isSubtypeOf(greaterArg)
        }
      args.foldLeft(Seq.empty[Seq[Type with Organized]]) {
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
    debugPrint(tgt, ">>> Current target")
    debugPrint(result, ">>> Result so far")
    val knownSupertypes = findSupertypeEntries(result, tgt)
    debugPrint(knownSupertypes, "<<<<>>>>> SupertypeEntries:")
    findSmallerEntry(knownSupertypes, tgt) match {
      case Some(kv) =>
        debugPrint(kv, ">>> Already present")
        (substituteArguments(result, tgt, kv._1), Stream.empty #:: Stream.empty[Stream[Type]]) // replace the target everywhere
      case None =>
        val orgTgt = time("target organization") { Organized.intersect(Organized(tgt).paths.minimize.toSeq) }

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
            splitCover(cType, orgTgt).map(_._1.reverse)
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
    debugPrint(repository, "Repository: ")
    val resultGrammar = debugPrint(inhabitRec(targets: _*).last._1, "before pruning")
    times.foreach(debugPrint(_))
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