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

/** Type inhabitation for finite combinatory logic (FCL) */
class FiniteCombinatoryLogic(val subtypes: SubtypeEnvironment, val repository: Repository) {

  import subtypes._

  /** An organized version of `repository` given in the constructor. */
  private val organizedRepository: Map[String, Type with Organized] = repository.mapValues(Organized(_))

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
    //println(s"$location used:  ${after - before}")
    forcedResult*/
    x
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
    * For performance reasons duplicate paths are not necessarily removed, so `=` above is modulo subtyping.
    */
  private def intersectArguments(arguments: Seq[Seq[Type]]): Seq[Type with Organized] = time("intersecting arguments") {
    def intersectPiecewise(xs: Seq[Type with Organized], ys: Seq[Type with Organized]) =
      xs.zip(ys).map{
        case (t1, t2) => Organized.intersect(t1.paths ++ t2.paths)
      }
    if (arguments.isEmpty) Seq.empty else {
      arguments.tail.aggregate(arguments.head.map(Organized(_)))(
        (xs, ys) => intersectPiecewise(xs, ys.map(Organized(_))),
        intersectPiecewise
      )
    }
  }

  /**
    * Finds all piecewise intersections of argument sequences in `paths`, such that
    * correspondingly intersected targets are subtypes of `tgt`.
    * Avoids selecting redundant paths.
    * Assumes all argument sequences in `paths` are of equal length.
    * Example:
    * <code>
    *   covers('A :&: 'B,
    *     (Seq('X, P), 'A) #:: (Seq('Y, Q), 'A) #:: (Seq('Z, P), 'B) #:: Stream.empty) ==
    *     Seq('X :&: 'Z, P) #:: Seq('Y :&: 'Z, P) #:: Stream.empty
    * </code>
    */
  final def covers(tgt: Type with Organized, paths: Seq[(Seq[Type], Type with Path)]): Seq[Seq[Type]] = time("covers") {
    val coveringArgs: Iterable[Finite[Seq[Type]]] =
      tgt.paths.foldLeft(Map.empty[Type with Path, Set[Seq[Type]]]) {
        case (r, toCover) if r.keys.exists(_.isSubtypeOf(toCover)) => r
        case (r, toCover) => r.updated(toCover, paths.foldLeft(Set.empty[Seq[Type]]) {
            case (s, (args, tgt)) if !s.contains(args) && tgt.isSubtypeOf(toCover) => s + args
            case (s, _) => s
          })
      }.values
        .map(xs => xs.aggregate(Finite.empty[Seq[Type]])((xs, x) => xs.:+:(Finite.singleton(x)), (x, y) => x :+: y))

    if (coveringArgs.isEmpty || coveringArgs.exists(_.cardinality == 0)) Seq.empty else {
      coveringArgs.view.tail.foldLeft(coveringArgs.head.map(Seq(_))) {
        case (s, args) => (args :*: s).map { case (x, y) => x +: y }
      }.map(intersectArguments)
        .values
        .distinct
    }
  }

  /** Substitutes all right hand side occurences in `grammar` of `oldType` by `newType`. */
  final def substituteArguments(grammar: TreeGrammar, oldType: Type, newType: Type): TreeGrammar =
    grammar
      .mapValues(entries =>
        entries.map {
          case (c, tgts) => (c, tgts.map(tgt => if (tgt == oldType) newType else tgt))
        })

  /** Removes all entries of `grammar` where `arg` occurs in a right hand side. */
  def removeEntriesWithArgument(grammar: TreeGrammar, arg: Type): TreeGrammar =
    grammar.mapValues(entries => entries.filterNot(_._2.contains(arg)))


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
            case ((nextProductions, nextTargetsLists), nextTargetList)
              if !nextProductions.contains((combinatorName, nextTargetList)) =>
              (nextProductions + ((combinatorName, nextTargetList)), nextTargetList.toStream #:: nextTargetsLists)
            case (s, _) => s
          }
      }
  }

  /** Performs a single inhabitation step.
    * Finds combinators which can inhabit `tgt`, adds their application as right hand sides for the left hand side `tgt`
    * and returns a stream of new targets for each combinator that was used.
    * Removes combinators depending on `tgt` if `grammar` contains a pair `(sigma -> rhs)`, where `sigma` is a supertype
    * of `tgt` and `rhs` is an empty set.
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
    if (knownSupertypes.values.exists(_ == Set.empty)) {
      debugPrint(tgt, ">>> Already present and failed")
      (removeEntriesWithArgument(result, tgt), Stream.empty)
    } else {
      findSmallerEntry(knownSupertypes, tgt) match {
        case Some(kv) =>
          debugPrint(kv, ">>> Already present")
          (substituteArguments(result, tgt, kv._1), Stream.empty #:: Stream.empty[Stream[Type]]) // replace the target everywhere
        case None =>
          val orgTgt = time("target organization") { Organized(tgt) }

          val recursiveTargets: Map[String, Iterable[Seq[Type]]] =
            organizedRepository.par.mapValues { cType =>
              debugPrint(orgTgt, "Covering component")
              debugPrint(cType.paths, "Using paths")
              val relevant = cType.paths
                .flatMap(relevantFor(orgTgt, _))
                .map(debugPrint(_, "Of those are relevant"))

              if (orgTgt.paths.exists(tgtP => !relevant.exists(r => r._2.isSubtypeOf(tgtP)))) {
                Iterable.empty
              } else {
                relevant
                  .groupBy(x => x._1.size)
                  .mapValues(pathComponents => covers(orgTgt, pathComponents))
                  .values.flatten
              }
            }.toMap.seq
          val (newProductions, newTargets) = newProductionsAndTargets(recursiveTargets)
          newTargets.map(debugPrint(_, "Recursively inhabiting"))

          (result + (tgt -> newProductions), newTargets)
      }
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