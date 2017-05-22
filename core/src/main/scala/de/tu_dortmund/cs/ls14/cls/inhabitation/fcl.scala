package de.tu_dortmund.cs.ls14.cls.inhabitation

import de.tu_dortmund.cs.ls14.cls.types._

class FiniteCombinatoryLogic(val subtypes: SubtypeEnvironment, val repository: Repository) {

  import subtypes._

  private val organizedRepository: Map[String, Type with Organized] = repository.mapValues(Organized(_))

  private final def debugPrint[A](x: A, msg: String = ""): A = {
    //println(s"$msg : ${ if (x.isInstanceOf[Stream[_]]) x.asInstanceOf[Stream[_]].toList.toString else x.toString}")
    x
  }

  /**
    * Splits `path` into (argument, target)-pairs, relevant for inhabiting `target`.
    * Relevant target components are path suffixes, which are supertypes of `target`.
    */
  private def relevantFor(target: Type with Organized,
    path: Type with Organized with Path): Seq[(Seq[Type], Type with Path)] = {
    path match {
      case Path(args, tgt) =>
        args
          .inits
          .toSeq
          .zip(args.tails.toSeq.reverse.map(Path(_, tgt)))
          .filter { case (_, selected) => target.paths.exists(tgtP => tgtP.isSupertype(selected)) }
    }
  }

  /**
    * Piecewise intersection of argument types.
    * Assumes all elements of `arguments` are of the same length.
    */
  private def intersectArguments(arguments: Seq[Seq[Type]]): Seq[Type] =
    arguments match {
      case Seq() => Seq.empty
      case _ +: _ =>
        val orgArguments = arguments.map(_.map(Organized(_)))
        orgArguments.init.foldRight[Seq[Type with Organized]](orgArguments.last) {
          case (args, rs) =>
            rs.zip(args).map { case (arg, r) =>
              arg.paths.foldLeft(r) { case (intersectedArgs, argPath) =>
                if (argPath.isSupertype(intersectedArgs)) intersectedArgs
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
    * covers(tgt = A & B,
    * paths = (Seq(X, P), A) #:: (Seq(Y, Q), A) #:: (Seq(Z, P), B) #:: Stream.empty) ==
    * Seq(X & Z, P) #:: Seq(Y & Z, P) #:: Stream.empty
    * </code>
    */
  final def covers(tgt: Type, paths: Stream[(Seq[Type], Type with Path)]): Stream[Seq[Type]] = {
    final case class Step(args: Seq[Seq[Type]], tgt: Type with Organized, rest: Stream[(Seq[Type], Type with Path)])
    def coversOfStep(step: Step): (Stream[Step], Stream[Seq[Type]]) = {
      if (step.tgt.isSubtype(tgt)) {
        debugPrint(step.args, "---------------->>>> Args:")
        debugPrint(intersectArguments(step.args), "---------------->>>> Intersected Args:")
        debugPrint((step.tgt, tgt), "---------------->>>> Tgt (left <= right):")
        (Stream.empty, intersectArguments(step.args) #:: Stream.empty)
      }
      else {
        step.rest match {
          case (_, newTgt) #:: others if step.tgt.isSubtype(newTgt) =>
            (step.copy(rest = others) #:: Stream.empty, Stream.empty)
          case (newArgs, newTgt) #:: others =>
            (Step(newArgs +: step.args, Organized.intersect(newTgt +: step.tgt.paths), others) #::
              step.copy(rest = others) #::
              Stream.empty,
              Stream.empty
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

  final def substituteArguments(grammar: TreeGrammar, oldType: Type, newType: Type): TreeGrammar =
    grammar
      .mapValues(entries =>
        entries.map {
          case (c, tgts) => (c, tgts.map(tgt => if (tgt == oldType) newType else tgt))
        })

  final def removeEntriesWithArgument(grammar: TreeGrammar, arg: Type): TreeGrammar =
    grammar.mapValues(entries => entries.filterNot(_._2.contains(arg)))


  final def findSupertypeEntries(grammar: TreeGrammar, ty: Type): TreeGrammar =
    grammar.filter {
      case (k, _) => k.isSupertype(ty)
    }

  final def findSmallerEntry(grammar: TreeGrammar, ty: Type): Option[(Type, Set[(String, Seq[Type])])] =
    grammar.find {
      case (k, _) => k.isSubtype(ty)
    }

  final def inhabitStep(result: TreeGrammar, tgt: Type): (TreeGrammar, Stream[Stream[Type]]) = {
    debugPrint(tgt, ">>> Current target")
    debugPrint(result, ">>> Result so far")
    val knownSupertypes = findSupertypeEntries(result, tgt)
    if (knownSupertypes.values.exists(Set.empty)) {
      debugPrint(tgt, ">>> Already present and failed")
      (removeEntriesWithArgument(result, tgt), Stream.empty)
    } else {
      findSmallerEntry(knownSupertypes, tgt) match {
        case Some(kv) =>
          debugPrint(kv, ">>> Already present")
          (substituteArguments(result, tgt, kv._1), Stream.empty #:: Stream.empty) // replace the target everywhere
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

  final def inhabitSequentially(grammar: TreeGrammar, tgts: Stream[Type]): (TreeGrammar, Stream[Stream[Type]]) = {
    val (newGrammar, newTgts) =
      tgts.foldLeft[(TreeGrammar, Option[Stream[Stream[Type]]])]((grammar, Some(Stream.empty))) {
        case (s@(_, None), _) => s
        case ((g, Some(result)), tgt) =>
          inhabitStep(g, tgt) match {
            case (newG, r@(_ #:: _)) => (newG, Some(result.append(r)))
            case (newG, _) => (newG, None)
          }
      }
    (newGrammar, newTgts.getOrElse(Stream.empty))
  }

  final def inhabitSequentiallyContinueIfFailing(grammar: TreeGrammar,
    tgts: Stream[Stream[Type]]): (TreeGrammar, Stream[Stream[Type]]) = {
    tgts.foldLeft((grammar, Stream.empty[Stream[Type]])) {
      case ((g, accumulatedTgts), oldTgts) =>
        val (newGrammar, newTgts) = inhabitSequentially(g, oldTgts)
        (newGrammar, accumulatedTgts.append(newTgts))
    }
  }

  // Iterate inhabitStep
  final def inhabitRec(targets: Type*): Stream[(TreeGrammar, Stream[Stream[Type]])] = {
    val (steps, stable) =
      Stream
        .iterate((Map.empty[Type, Set[(String, Seq[Type])]], targets.map(_ #:: Stream.empty).toStream)) {
          case (grammar, tgts) => inhabitSequentiallyContinueIfFailing(grammar, tgts)
        }.span(_._2.nonEmpty)
    steps :+ stable.head
  }

  final def ensureTargetExistsIfEqualTypePresent(grammar: TreeGrammar, target: Type): TreeGrammar = {
    if (!grammar.contains(target)) {
      grammar.keys.find(ty => target.isSubtype(ty) && target.isSupertype(ty)) match {
        case None => grammar
        case Some(ty) => grammar + (target -> grammar(ty))
      }
    } else grammar
  }

  def inhabit(targets: Type*): TreeGrammar = {
    debugPrint(repository, "Repository: ")
    val resultGrammar = debugPrint(inhabitRec(targets: _*).last._1, "before pruning")
    val resultGrammarWithAllTargets = targets.foldLeft(resultGrammar)(ensureTargetExistsIfEqualTypePresent)
    prune(resultGrammarWithAllTargets)
  }

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

  final def prune(grammar: TreeGrammar): TreeGrammar = {
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

object FiniteCombinatoryLogic {
  def algorithm: InhabitationAlgorithm = {
    case (_, subtypes, repository) =>
      targets => new FiniteCombinatoryLogic(subtypes, repository).inhabit(targets: _*)
  }
}