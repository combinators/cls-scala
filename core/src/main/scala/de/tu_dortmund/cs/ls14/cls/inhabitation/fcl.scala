package de.tu_dortmund.cs.ls14.cls.inhabitation

import de.tu_dortmund.cs.ls14.cls.types._

import scala.annotation.tailrec

class FiniteCombinatoryLogic(val subtypes: SubtypeEnvironment, val repository: Repository) {
  import subtypes._

  private val organizedRepository = repository.mapValues {
    case Organized(ps) => Organized.intersect(ps)
  }

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

  private def subseqs[A](xs: Seq[A]): Seq[Seq[A]] = {
    (1 to xs.size) flatMap (xs.combinations)
  }

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

  private final def debugPrint[A](x: A, msg: String = ""): A = {
    //println(s"$msg : $x")
    x
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

  final def equalEtryIsEmptyOrNotPresent(grammar: TreeGrammar, ty: Type): Boolean =
    findEqualEntry(grammar, ty) match {
      case None => true
      case Some((_, xs)) => xs.isEmpty
    }


  // Returns the new tree grammar after inhabiting target and a stream of new goals.
  final def inhabitStep(result: TreeGrammar)(recTarget: RecursiveInhabitationTarget): (TreeGrammar, Stream[RecursiveInhabitationTarget]) = {
    debugPrint(recTarget, ">>> Current target")
    debugPrint(result, ">>> Result so far")
    val RecursiveInhabitationTarget(target, skipIfUninhabited) = recTarget

    if (skipIfUninhabited.exists(ty => equalEtryIsEmptyOrNotPresent(result, ty))) (result, Stream.empty)
    else {
      findEqualEntry(result, target) match {
        case Some(kv) => (substituteArguments(result, target, kv._1), Stream.empty) // replace the target everywhere
        case None =>
          val orgTgt =
            Organized.intersect(target match {
              case Organized(paths) => paths
            })

          val recursiveTargets: Map[String, Iterable[Seq[Type]]] =
            organizedRepository.mapValues { case Organized(cPaths) =>
              debugPrint(orgTgt, "Covering component")
              debugPrint(cPaths, "Using paths")
              cPaths
                .flatMap(relevantFor(orgTgt, _))
                .map(debugPrint(_, "Of those are relevant"))
                .groupBy(x => x._1.size)
                .mapValues { case pathComponents =>
                  subseqs(pathComponents)
                    .map(debugPrint(_, "path component"))
                    .filter(components =>
                      target.isSupertype(Organized.intersect(components.map(_._2))))
                    .map(components => intersectArguments(components.map(_._1)))
                }
                .map(debugPrint(_, "Recursively inhabiting"))
                .values.toSet.flatten
            }
          val newProductions: Set[(String, Seq[Type])] =
            recursiveTargets.toSeq.flatMap { case (c, tgts) =>
              tgts.map((c, _))
            }.toSet

          (result + (target -> newProductions),
            newProductions.flatMap(tgts => toRecursiveInhabitationTargets(tgts._2.toStream)).toStream)
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