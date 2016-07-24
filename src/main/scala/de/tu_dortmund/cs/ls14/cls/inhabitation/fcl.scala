package de.tu_dortmund.cs.ls14.cls.inhabitation

import de.tu_dortmund.cs.ls14.cls.types._

class FiniteCombinatoryLogic(subtypes: SubtypeEnvironment, repository: Repository) {
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
    (1 to xs.size) flatMap xs.sliding
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

  private def debugPrint[A](x: A, msg: String = ""): A = {
    //println(s"$msg : $x")
    x
  }

  def inhabit(target: Type): TreeGrammar = {
    def inhabitRec(result: TreeGrammar)(target: Type): TreeGrammar = {
      debugPrint(target, ">>> Current target")
      debugPrint(result, ">>> Result so far")

      if (result.contains(target)) result else {

        result.find(kv => kv._1.isSupertype(target) && target.isSupertype(kv._1)) match {
          case Some(kv) => result + (target -> kv._2)
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

            newProductions
              .flatMap(_._2)
              .foldRight(result + (target -> newProductions)) { case (tgt, s) =>
                inhabitRec(s)(tgt)
              }
        }
      }
    }

    inhabitRec(Map.empty)(target)
  }
}