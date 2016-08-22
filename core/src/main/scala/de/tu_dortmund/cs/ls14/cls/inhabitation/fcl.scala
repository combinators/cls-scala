package de.tu_dortmund.cs.ls14.cls.inhabitation

import de.tu_dortmund.cs.ls14.cls.types._

import scala.annotation.tailrec

class FiniteCombinatoryLogic(val subtypes: SubtypeEnvironment, val repository: Repository) {
  import subtypes._

  private val organizedRepository = repository.mapValues {
    case Organized(ps) => Organized.intersect(ps)
  }

  /**
    * Splits `path` into (argument, target)-pairs, relevant for inhabiting `target`.
    * Relevant target components are path suffixes, which are supertypes of `target`.
    */
  private def relevantFor(target: Type, path: Type with Path): Seq[(Seq[Type], Type with Path)] =
    (target, path) match {
      case (Organized(tgtPaths), Path(args, tgt)) =>
        args
          .inits
          .toSeq
          .zip(args.tails.toSeq.reverse.map(Path(_, tgt)))
          .filter {
            case (_, Path(selectedArgs, selectedC)) =>
              tgtPaths.exists {
                case Path(tgtArgs, tgtC) =>
                  (selectedArgs.size == tgtArgs.size) &&
                    subtypes.transitiveReflexiveTaxonomicSubtypesOf(tgtC.name)(selectedC.name) &&
                    (selectedArgs.zip(tgtArgs).forall {
                      case (selArg, tgtArg) => selArg.isSupertype(tgtArg)
                    })
              }
            }
    }

  /**
    * Piecewise intersection of argument types.
    * Assumes all elements of `arguments` are of the same length.
    */
  private def intersectArguments(arguments: Seq[Seq[Type]]): Seq[Type] =
    arguments match {
      case Seq() => Seq.empty
      case arg +: args =>
        arguments.init.foldRight[Seq[Type]](arguments.last) {
          case (args, rs) =>
            rs.zip(args).map { case (Organized(argPaths), r) =>
              argPaths.foldLeft(r) { case (r, argPath) =>
                if (argPath.isSupertype(r)) r else Intersection(argPath, r)
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
    *        paths = (Seq(X, P), A) #:: (Seq(Y, Q), A) #:: (Seq(Z, P), B) #:: Stream.empty) ==
    * Seq(X & Z, P) #:: Seq(Y & Z, P) #:: Stream.empty
    * </code>
    */
  final def covers(tgt: Type, paths: Stream[(Seq[Type], Type with Path)]): Stream[Seq[Type]] = {
    final case class Step(args: Seq[Seq[Type]], tgt: Type, rest: Stream[(Seq[Type], Type with Path)])
    def coversOfStep(step: Step): (Stream[Step], Stream[Seq[Type]]) = {
      if (step.tgt.isSubtype(tgt)) (Stream.empty, intersectArguments(step.args) #:: Stream.empty)
      else {
        step.rest match {
          case (_, newTgt) #:: others if (step.tgt.isSubtype(newTgt)) =>
            (step.copy(rest = others) #:: Stream.empty, Stream.empty)
          case (newArgs, newTgt) #:: others =>
            (Step(newArgs +: step.args, Intersection(newTgt, step.tgt), others) #::
              step.copy(rest = others) #::
              Stream.empty,
              Stream.empty
            )
          case _ => (Stream.empty, Stream.empty)
        }
      }
    }
    def coversOf(currentArgs : Seq[Type], currentTgt: Type, otherPaths: Stream[(Seq[Type], Type with Path)]): Stream[Seq[Type]] = {
      val start: (Stream[Step], Stream[Seq[Type]]) =
        (Step(Seq(currentArgs), currentTgt, otherPaths) #:: Stream.empty[Step], Stream.empty)
      val (iterating, stable) = Stream.iterate(start) {
        case (steps, _) =>
          val (newSteps, results) = steps.map(coversOfStep).unzip
          (newSteps.flatten, results.flatten)
      }.span(state => !state._1.isEmpty)

      (stable.head +: iterating).flatMap(_._2)
    }

    val pathPairs = paths.zip(paths.tails.toStream.tail)
    pathPairs.flatMap {
      case ((currentArgs, currentTgt), otherPaths) => coversOf(currentArgs, currentTgt, otherPaths)
    }
  }

  final def reduceToMinimal(paths: Stream[Type with Path]): Stream[Type with Path] = {
    paths
      .zip(paths.tails.toStream.tail)
      .filterNot {
        case (path, otherPaths) =>
          otherPaths.exists{
            case otherPath => otherPath.isSubtype(path)
          }
      }.map(_._1)
  }


  private final def debugPrint[A](x: A, msg: String = ""): A = {
    //println(s"$msg : $x")
    x
  }

  final def substituteArguments(grammar: TreeGrammar, oldType: Type, newType: Type): TreeGrammar =
    grammar
      .mapValues(entries =>
        entries.map {
          case (c, tgts) => (c, tgts.map(tgt => if (tgt == oldType) newType else tgt))
        })

  final def findEqualEntry(grammar: TreeGrammar, ty: Type): Option[(Type, Set[(String, Seq[Type])])] =
    grammar.find {
      case (k, v) => k.isSupertype(ty) && k.isSubtype(ty)
    }

  final def equalEntryIsEmptyOrNotPresent(grammar: TreeGrammar, ty: Type): Boolean =
    findEqualEntry(grammar, ty) match {
      case None => true
      case Some((_, xs)) => xs.isEmpty
    }


  case class RecursiveInhabitationTarget(target: Type, skipIfUninhabited: Stream[Type] = Stream.empty)

  def toRecursiveInhabitationTargets(recursiveTargets: Stream[Type]): Stream[RecursiveInhabitationTarget] = {
    val skips = recursiveTargets.scanLeft(Stream.empty[Type]) {
      case (skip, tgt) => skip :+ tgt
    }
    recursiveTargets.zip(skips).map {
      case (tgt, skip) => RecursiveInhabitationTarget(tgt, skip)
    }
  }

  // Returns the new tree grammar after inhabiting target and a stream of new goals.
  final def inhabitStep(result: TreeGrammar)(recTarget: RecursiveInhabitationTarget): (TreeGrammar, Stream[RecursiveInhabitationTarget]) = {
    debugPrint(recTarget, ">>> Current target")
    debugPrint(result, ">>> Result so far")
    val RecursiveInhabitationTarget(target, skipIfUninhabited) = recTarget

    if (skipIfUninhabited.exists(ty => equalEntryIsEmptyOrNotPresent(result, ty))) (result, Stream.empty)
    else {
      findEqualEntry(result, target) match {
        case Some(kv) =>
          debugPrint(kv, ">>> Already present")
          (substituteArguments(result, target, kv._1), Stream.empty) // replace the target everywhere
        case None =>
          val orgTgt =
            Organized.intersect(target match {
              case Organized(paths) => paths
            })

          val recursiveTargets: Map[String, Iterable[Seq[Type]]] =
            organizedRepository.mapValues { case Organized(cPaths) =>
              debugPrint(orgTgt, "Covering component")
              debugPrint(cPaths, "Using paths")
              reduceToMinimal(cPaths.toStream)
                .flatMap(relevantFor(orgTgt, _))
                .map(debugPrint(_, "Of those are relevant"))
                .groupBy(x => x._1.size)
                .mapValues (pathComponents => covers(orgTgt, pathComponents))
                .map(debugPrint(_, "Recursively inhabiting"))
                .values.map(_.distinct).flatten
            }
          val newProductions: Set[(String, Seq[Type])] =
            recursiveTargets.toSeq.flatMap { case (c, tgts) =>
              tgts.map((c, _))
            }.toSet

          val newTargets: Stream[RecursiveInhabitationTarget] =
            recursiveTargets.values.flatMap { case tgtss =>
              tgtss.flatMap(tgts => toRecursiveInhabitationTargets(tgts.toStream))
            }.toStream

          (result + (target -> newProductions), newTargets)
      }
    }
  }

  // Iterate inhabitStep
  final def inhabitRec(target: Type): Stream[(TreeGrammar, Stream[RecursiveInhabitationTarget])] = {
    val (steps, stable) =
      Stream
        .iterate((Map.empty[Type, Set[(String, Seq[Type])]], RecursiveInhabitationTarget(target) #:: Stream.empty)) {
          case (grammar, target #:: targets) =>
          val (newGrammar, newTargets) = inhabitStep(grammar)(target)
          (newGrammar, targets.append(newTargets))
        } .span(_._2.nonEmpty)
    steps :+ stable.head
  }


  def inhabit(target: Type): TreeGrammar = {
    prune(inhabitRec(target).last._1)
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
        if (pruned.isEmpty) g else (g + (k -> pruned))
    }
  }
}

object FiniteCombinatoryLogic {
  def algorithm: InhabitationAlgorithm = {
    case (_, subtypes, repository) =>
      target => new FiniteCombinatoryLogic(subtypes, repository).inhabit(target)
  }
}