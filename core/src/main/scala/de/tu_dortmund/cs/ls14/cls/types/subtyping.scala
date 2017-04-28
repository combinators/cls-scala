package de.tu_dortmund.cs.ls14.cls.types

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait Path extends Organized { self: Type =>
  final val paths: Stream[Type with Path] = this #:: Stream.empty
}

trait Organized { self: Type =>
  val paths: Stream[Type with Path]
}

object Path {
  def unapply(t: Type): Option[(Seq[Type], Constructor with Path)] =
    t match {
      case c : Constructor with Path => Some((Seq.empty, c))
      case Constructor(name) => Some((Seq.empty, new Constructor(name) with Path))
      case Constructor(name, args @ _*) =>
        args.dropWhile(_ == Omega) match {
          case Path(_, _) +: rest =>
            rest.dropWhile(_ == Omega) match {
              case Seq() => Some((Seq(), new Constructor(name, args: _*) with Path))
              case _ => None
            }
          case Seq() => Some((Seq(), new Constructor(name, args: _*) with Path))
        }
      case Arrow(src, Path(srcs, tgt)) => Some((src +: srcs, tgt))
      case _ => None
    }

  def apply(args: Seq[Type] = Seq.empty, target: Constructor with Path): Type with Path =
    args.foldRight[Type with Organized with Path](target) {
      case (arg, result) => new Arrow(arg, result) with Path
    }
}

object Organized {
  private def isOmega(ty: Type): Boolean =
    ty match {
      case Omega => true
      case Arrow(_, tgt) => isOmega(tgt)
      case Intersection(l, r) => isOmega(l) && isOmega(r)
      case _ => false
    }

  final def addPaths(xs: Seq[Type with Path], ys: Seq[Type with Path]): Seq[Type with Path] =
    xs.toStream.append(ys.toStream)


  final def apply(t: Type): Type with Organized =
    t match {
      case ot : Organized => ot
      case Constructor(name) => new Constructor(name) with Path
      case Constructor(name, args @ _ *) if args.forall(isOmega) =>
        new Constructor(name, Seq.fill(args.size)(Omega): _*) with Path
      case Constructor(name, args @ _*) =>
        intersect(args.map(Organized(_)).zipWithIndex.flatMap {
            case (orgArg, argNo) =>
              orgArg.paths.map (orgArg => {
                val argVect = Stream.tabulate(args.size) {
                  case n if n == argNo => orgArg
                  case _ => Omega
                }
                new Constructor(name, argVect: _*) with Path
              })
          })
      case Arrow(src, tgt) =>
        intersect(Organized(tgt).paths map (tgt => new Arrow(src, tgt) with Path))
      case Intersection(sigma, tau) =>
        intersect(Organized(sigma).paths.append(Organized(tau).paths))
    }

    final def intersect(paths: Seq[Type with Organized with Path]): Type with Organized =
      paths match {
        case Seq() => Omega
        case _ =>
          paths.toStream.reduce[Type with Organized] {
            case (p1, p2) => new Intersection(p1, p2) with Organized {
              val paths = p1.paths.append(p2.paths)
            }
          }
      }
}



case class SubtypeEnvironment(taxonomicSubtypesOf: Map[String, Set[String]]) {

  final private def transitiveClosureStep(state: Map[String, Set[String]]): (Boolean, Map[String, Set[String]]) = {
    state.foldLeft((false, state)) {
      case ((hasChanged, newState), (sigma, currentSubtypes)) =>
        val recursiveSubtypes = currentSubtypes.flatMap(state.getOrElse(_, Set.empty))
        val newSubtypes = currentSubtypes.union(recursiveSubtypes)
        val changedNow = currentSubtypes.size != newSubtypes.size
        if (changedNow) (true, newState + (sigma -> newSubtypes))
        else (hasChanged, newState)
    }
  }

  final private def reflexiveClosure(state: Map[String, Set[String]]): Map[String, Set[String]] =
    state.map {
      case (sigma, taus) => (sigma, taus + sigma)
    }.withDefault(x => Set(x))

  lazy private val closedEnvironment: Map[String, Set[String]] =
    reflexiveClosure(
      Stream.iterate[(Boolean, Map[String, Set[String]])]((true, taxonomicSubtypesOf))(x => transitiveClosureStep(x._2))
        .dropWhile(_._1)
        .head
        ._2)


  lazy val transitiveReflexiveTaxonomicSubtypesOf: String => Set[String] = closedEnvironment.apply

  sealed trait TypeRelationOf {
    def isSupertype(tau: Type): Boolean
    def isSubtype(tau: Type): Boolean
  }

  sealed private class SupertypesOfPath(pathArgs: Seq[Type], tgt: Constructor) {
    private lazy val tgtSubs = transitiveReflexiveTaxonomicSubtypesOf(tgt.name)

    private def relevant(subArgs: Seq[Type], subTgt: Constructor): Boolean =
      (subArgs.size == pathArgs.size) &&
        tgtSubs(subTgt.name) &&
        subArgs.par.zip(pathArgs.par).forall {
          case (subArg, pathArg) => subArg.isSupertype(pathArg)
        }

    def isSuperTypeOf(taus: Seq[(Seq[Type], Constructor)]): Boolean = {
      taus
        .toStream
        .filter { case (argsTau, tgtTau) => relevant(argsTau, tgtTau) } match {
          case Seq() => false
          case tauPaths =>
            tauPaths.map(_._2.arguments) match {
              case Seq() => true
              case argss@(_ +: _) =>
                argss.reduce[Seq[Type]] {
                    case (args1, args2) =>
                      args1.zip(args2).map { case (arg1, arg2) => Intersection(arg1, arg2) }
                  }
                .zip(tgt.arguments)
                .forall { case (arg, tgtArg) => tgtArg.isSupertype(arg) }
            }
        }

    }
  }

  implicit class toTypeRelationOf(sigma: Type) extends TypeRelationOf {
    def isSupertype(tau: Type): Boolean = {
      val organizedTau =
        Organized(tau).paths.map {
          case Path(args, tgt) => (args, tgt)
        }

      Organized(sigma).paths match {
        case paths@(_ #:: _) =>
          paths.forall {
            case Path(srcs, tgt) => new SupertypesOfPath(srcs, tgt).isSuperTypeOf(organizedTau)
          }
        case _ => true
      }
    }
    def isSubtype(tau: Type): Boolean =
      toTypeRelationOf(tau).isSupertype(sigma)
  }
}



sealed trait Taxonomy extends (String => Set[String]) {
  val underlyingMap: Map[String, Set[String]]

  def merge(other: Taxonomy): Taxonomy
  def merge(other: NonEmptyTaxonomy): NonEmptyTaxonomy

  def apply(s: String): Set[String] = underlyingMap.getOrElse(s, Set.empty)
}


sealed trait NonEmptyTaxonomy extends Taxonomy { self =>
  val underlyingMap: Map[String, Set[String]]
  protected val head: String

  def addSubtype(entry: String): NonEmptyTaxonomy =
    new NonEmptyTaxonomy {
      val underlyingMap = self.underlyingMap.updated(self.head, self(self.head) + entry)
      val head: String = self.head
    }

  def addSubtypes(entries: NonEmptyTaxonomy): NonEmptyTaxonomy =
    new NonEmptyTaxonomy {
      val underlyingMap =
        self
          .merge(entries)
          .underlyingMap
          .updated(self.head, self(self.head) + entries.head)
      val head: String = self.head
    }

  override def merge(entries: Taxonomy): NonEmptyTaxonomy =
    new NonEmptyTaxonomy {
      val underlyingMap =
        entries.underlyingMap.foldLeft(self.underlyingMap) {
          case (m, (k, v)) => m.updated(k, m.getOrElse(k, Set.empty) ++ v)
        }
      val head: String = self.head
    }
  override def merge(entries: NonEmptyTaxonomy): NonEmptyTaxonomy =
    merge(entries.asInstanceOf[Taxonomy])
}

object Taxonomy {
  def apply(superType: String): NonEmptyTaxonomy =
    new NonEmptyTaxonomy {
      val underlyingMap: Map[String, Set[String]] = Map.empty
      val head: String = superType
    }
  def empty: Taxonomy =
    new Taxonomy {
      val underlyingMap: Map[String, Set[String]] = Map.empty

      override def merge(other: Taxonomy) = other
      override def merge(other: NonEmptyTaxonomy) = other
    }
}