package de.tu_dortmund.cs.ls14.cls.types

sealed trait Path

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
    args.foldRight[Type with Path](target) {
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

  def unapply(t: Type): Option[Seq[Type with Path]] =
    t match {
      case p : Path => Some(Seq(p))
      case Constructor(name) => Some(Seq(new Constructor(name) with Path))
      case Constructor(name, args @ _ *) if args.forall(isOmega) =>
        Some(Seq(new Constructor(name, Seq.fill(args.size)(Omega): _*) with Path))
      case Constructor(name, args @ _*) =>
        Some(
          args.zipWithIndex.flatMap {
            case (Organized(orgArgs), argNo) =>
              orgArgs map (orgArg => {
                val argVect = Seq.tabulate(args.size) {
                  case n if n == argNo => orgArg
                  case _ => Omega
                }
                new Constructor(name, argVect: _*) with Path
              })
          })
      case Arrow(src, tgt) =>
        tgt match {
          case Organized(orgTgt) => Some(orgTgt map (tgt => new Arrow(src, tgt) with Path))
        }
      case Intersection(sigma, tau) =>
        (sigma, tau) match {
          case (Organized(orgSigma), Organized(orgTau)) => Some(orgSigma ++ orgTau)
        }
      case Omega => Some(Seq.empty)
    }

    def intersect(paths: Seq[Type with Path]): Type =
      paths match {
        case Seq() => Omega
        case _ =>
          paths.init.foldRight[Type](paths.last) {
            case (path, result) => Intersection(path, result)
          }
      }

}

case class SubtypeEnvironment(taxonomicSubtypesOf: String => Set[String]) {
  import scala.collection.mutable
  lazy val transitiveReflexiveTaxonomicSubtypesOf: String => Set[String] =
    new mutable.HashMap[String, Set[String]] {
      override def apply(sigma: String): Set[String] =
        getOrElseUpdate(sigma, {
          update(sigma, Set(sigma))
          taxonomicSubtypesOf(sigma).flatMap {
            case tau => transitiveReflexiveTaxonomicSubtypesOf(tau)
          } + sigma
        })
    }


  sealed trait TypeRelationOf {
    def isSupertype(tau: Type): Boolean
    def isSubtype(tau: Type): Boolean
  }

  sealed private class SupertypesOfPath(pathArgs: Seq[Type], tgt: Constructor) {
    private lazy val tgtSubs = transitiveReflexiveTaxonomicSubtypesOf(tgt.name)

    private def relevant(subArgs: Seq[Type], subTgt: Constructor): Boolean =
      (subArgs.size == pathArgs.size) &&
        tgtSubs(subTgt.name) &&
        subArgs.zip(pathArgs).forall {
          case (subArg, pathArg) => subArg.isSupertype(pathArg)
        }

    def isSuperTypeOf(taus: Seq[(Seq[Type], Constructor)]): Boolean = {
      taus
        .filter { case (argsTau, tgtTau) => relevant(argsTau, tgtTau) } match {
          case Seq() => false
          case tauPaths =>
            tauPaths.map(_._2.arguments) match {
              case Seq() => true
              case argss@(_ +: _) =>
                argss.init.foldRight(argss.last) { case (args, resArgs) =>
                  resArgs.zip(args).map { case (res, arg) => Intersection(arg, res) }
                }.zip(tgt.arguments)
                 .forall { case (arg, tgtArg) => tgtArg.isSupertype(arg) }
            }
        }

    }
  }

  implicit class toTypeRelationOf(sigma: Type) extends TypeRelationOf {
    def isSupertype(tau: Type): Boolean = {
      val organizedTau = tau match {
        case Organized(paths) => paths map {
          case Path(args, tgt) => (args, tgt)
        }
      }
      sigma match {
        case Organized(Seq()) => true
        case Organized(paths) =>
          paths.forall {
            case Path(srcs, tgt) => new SupertypesOfPath(srcs, tgt).isSuperTypeOf(organizedTau)
          }
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