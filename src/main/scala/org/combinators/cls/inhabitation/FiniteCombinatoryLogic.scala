package de.tu_dortmund.cs.ls14.cls.inhabitation

import de.tu_dortmund.cs.ls14.cls.types._

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

  /** Splits `path` into (argument, target)-pairs, relevant for inhabiting `target`.
    * Relevant target components are path suffixes, which are supertypes of `target`.
    * <code>
    *   relevantFor('Int :&: ('Float =>: 'Int), 'String =>: 'Float =>: 'Int) =
    *     Seq((Seq('String, 'Float), 'Int), (Seq('String), 'Float =>: 'Int))
    * </code>
    */
  private def relevantFor(target: Type with Organized,
    path: Type with Organized with Path): Seq[(Seq[Type], Type with Path)] = {
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
    * <code>
    *   intersectArguments(Seq('A, 'B =>: 'C :&: 'D), Seq('B, 'B =>: 'C :&: 'E)) =
    *     Seq('A :&: 'B, ('B =>: 'C) :&: ('B =>: 'D) :&: ('B =>: 'E))
    * </code>
    */
  private def intersectArguments(arguments: Seq[Seq[Type]]): Seq[Type with Organized] =
    arguments match {
      case Seq() => Seq.empty
      case _ +: _ =>
        val orgArguments = arguments.map(_.map(Organized(_)))
        orgArguments.init.foldRight[Seq[Type with Organized]](orgArguments.last) {
          case (args, rs) =>
            rs.zip(args).map { case (arg, r) =>
              arg.paths.foldLeft(r) { case (intersectedArgs, argPath) =>
                if (argPath.isSupertypeOf(intersectedArgs)) intersectedArgs
                else Organized.intersect(argPath +: intersectedArgs.paths)
              }
            }
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
  final def covers(tgt: Type, paths: Stream[(Seq[Type], Type with Path)]): Stream[Seq[Type]] = {
    final case class Step(args: Seq[Seq[Type]], tgt: Type with Organized, rest: Stream[(Seq[Type], Type with Path)])
    def coversOfStep(step: Step): (Stream[Step], Stream[Seq[Type]]) = {
      if (step.tgt.isSubtypeOf(tgt)) {
        debugPrint(step.args, "---------------->>>> Args:")
        debugPrint(intersectArguments(step.args), "---------------->>>> Intersected Args:")
        debugPrint((step.tgt, tgt), "---------------->>>> Tgt (left <= right):")
        (Stream.empty, intersectArguments(step.args) #:: Stream.empty[Seq[Type]])
      }
      else {
        step.rest match {
          case (_, newTgt) #:: others if step.tgt.isSubtypeOf(newTgt) =>
            (step.copy(rest = others) #:: Stream.empty[Step], Stream.empty[Seq[Type]])
          case (newArgs, newTgt) #:: others =>
            (Step(newArgs +: step.args, Organized.intersect(newTgt +: step.tgt.paths), others) #::
              step.copy(rest = others) #::
              Stream.empty[Step],
              Stream.empty[Seq[Type]]
            )
          case _ => (Stream.empty, Stream.empty)
        }
      }
    }
    def coversOf(currentArgs: Seq[Type],
      currentTgt: Type with Organized,
      otherPaths: Stream[(Seq[Type], Type with Path)]): Stream[Seq[Type]] = {
      val start: (Stream[Step], Stream[Seq[Type]]) =
        (Step(Seq(currentArgs), currentTgt, otherPaths) #:: Stream.empty[Step], Stream.empty)
      val (iterating, stable) = Stream.iterate(start) {
        case (steps, _) =>
          val (newSteps, results) = steps.map(coversOfStep).unzip
          (newSteps.flatten, results.flatten)
      }.span(state => state._1.nonEmpty)

      (stable.head +: iterating).flatMap(_._2)
    }

    val pathPairs = paths.zip(paths.tails.toStream.tail)
    pathPairs.flatMap {
      case ((currentArgs, currentTgt), otherPaths) => coversOf(currentArgs, currentTgt, otherPaths)
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
  final def removeEntriesWithArgument(grammar: TreeGrammar, arg: Type): TreeGrammar =
    grammar.mapValues(entries => entries.filterNot(_._2.contains(arg)))


  /** Finds all entries of `grammar` where the left hand side is a supertype of `ty`. */
  final def findSupertypeEntries(grammar: TreeGrammar, ty: Type): TreeGrammar =
    grammar.filter {
      case (k, _) => k.isSupertypeOf(ty)
    }

  /** Finds an entries of `grammar` where the left hand side is a subtype of `ty`. */
  final def findSmallerEntry(grammar: TreeGrammar, ty: Type): Option[(Type, Set[(String, Seq[Type])])] =
    grammar.find {
      case (k, _) => k.isSubtypeOf(ty)
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
  final def inhabitStep(result: TreeGrammar, tgt: Type): (TreeGrammar, Stream[Stream[Type]]) = {
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
          val orgTgt = Organized(tgt)

          val recursiveTargets =
            organizedRepository.par.mapValues { cType =>
              debugPrint(orgTgt, "Covering component")
              debugPrint(cType.paths, "Using paths")
              cType.paths
                .flatMap(relevantFor(orgTgt, _))
                .map(debugPrint(_, "Of those are relevant"))
                .groupBy(x => x._1.size)
                .mapValues(pathComponents => covers(orgTgt, pathComponents))
                .values.flatMap(_.distinct)
            }
          val newProductions: Set[(String, Seq[Type])] =
            recursiveTargets.toSeq.flatMap { case (c, tgts) =>
              tgts.map((c, _))
            }.toSet.seq

          val newTargets: Stream[Stream[Type]] =
            recursiveTargets.values.flatMap { tgtss =>
              tgtss.toStream.map(tgts => tgts.toStream)
            }.toStream
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
    val resultGrammarWithAllTargets = targets.foldLeft(resultGrammar)(ensureTargetExistsIfEqualTypePresent)
    prune(resultGrammarWithAllTargets)
  }

  /** Finds all productive left hand sides in `grammar`.
    * A left hand side is productive, if any of its right hand sides only requires arguments, which are productive
    * left hand sides of the grammar.
    */
  final def groundTypesOf(grammar: TreeGrammar): Set[Type] = {
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
  def prune(grammar: TreeGrammar): TreeGrammar = {
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