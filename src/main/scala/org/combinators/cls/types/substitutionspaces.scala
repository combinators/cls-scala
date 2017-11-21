package org.combinators.cls.types

import shapeless.feat.{Enumeration, Finite}

/** Things which induce finite spaces of substitutions. */
trait FiniteSubstitutionSpace {
  /** Obtains the space of well formed substitutions induced by `description`. */
  def allowedSubstitutions: Finite[Variable => Type]
}

/** Helpers to construct finite substitution spaces. */
object FiniteSubstitutionSpace {
  /** Returns the empty substitution space. */
  def empty: FiniteSubstitutionSpace = new FiniteSubstitutionSpace {
    override def allowedSubstitutions: Finite[(Variable) => Type] = Finite.empty
  }

  /** Returns the substitution space where every variable can get replaced by [[Omega]] */
  def omegaSpace: FiniteSubstitutionSpace = new FiniteSubstitutionSpace {
    override def allowedSubstitutions: Finite[(Variable) => Type] = Finite.singleton(_ => Omega)
  }
}

/** Kindings restrict variables by enumerating all of their possible substitutions.  */
sealed trait Kinding extends (Variable => Finite[Type]) with FiniteSubstitutionSpace {
  /** A map where each variable is assigned finitely many types it can be susbstituted by */
  protected[types] val underlyingMap: Map[Variable, Finite[Type]]

  /** Kindings induce finite substitution spaces.
    * We have: `S in WF iff S(alpha) = sigma for sigma in Kinding(alpha)`.
    */
  lazy val allowedSubstitutions: Finite[Variable => Type] = {
    lazy val varMappings: Stream[Finite[(Variable, Type)]] = underlyingMap.toStream.map {
      case (v, e) => e.map(v -> _)
    }
    if (varMappings.isEmpty) Finite.empty
    else
      varMappings.tail.foldLeft[Finite[Map[Variable, Type]]](varMappings.headOption.getOrElse(Finite.empty).map(Map(_))) {
        case (substs, e) => substs.:*:(e).map {
          case (vt: (Variable, Type), subst: Map[Variable,Type]) => subst + vt
        }
      }
  }

  /** Merges two kindings allowing the union of their substitutions.
    * @return a new kinding allowing all the options of this and the other kinding.
    */
  def merge(other: Kinding): Kinding
  /** Merges two kindings allowing the union of their substitutions.
    * All options of the other kinding will become options for this kinding.
    * The second kinding has to be non-empty (i.e. include information for at least one variable).
    * @return a new kinding allowing all the options of this and the other kinding.
    */
  def merge(other: NonEmptyKinding): NonEmptyKinding

  /** Looks up substitutions allowed for variable `v`.
    * @return the substitutions or an empty enumeration, if the variable has no defined kinding.
    */
  def apply(v: Variable): Finite[Type] =
    underlyingMap.getOrElse(v, Finite.empty)
}

/** Non empty kindings with a marked root variable. */
sealed trait NonEmptyKinding extends Kinding { self =>
  /** The marked root variable. */
  protected val head: Variable

  /** Adds a finite enumeration of options for the root variable.
    * @return a new non-empty kinding with the same root variable.
    */
  def addOptions(options: Finite[Type]) =
    new NonEmptyKinding {
      val underlyingMap: Map[Variable, Finite[Type]] =
        self.underlyingMap.updated(self.head, self(self.head).:+:(options))
      val head: Variable = self.head
    }

  /** Adds a single new option for the root variable.
    * @return a new non-empty kinding with the same root variable.
    */
  def addOption(ty: Type): NonEmptyKinding =
    addOptions(Finite.singleton(ty))

  override def merge(other: Kinding): NonEmptyKinding =
    new NonEmptyKinding {
      val underlyingMap: Map[Variable, Finite[Type]] =
        other.underlyingMap.foldLeft(self.underlyingMap) {
          case (m, (k, v)) => m.updated(k, m.getOrElse(k, Finite.empty).:+:(v))
        }
      val head: Variable = self.head
    }
  override def merge(other: NonEmptyKinding): NonEmptyKinding =
    merge(other.asInstanceOf[Kinding])
}

/** Helper object to construct [[Kinding]]. */
object Kinding {
  /** Creates a new non-empty Kinding rooted in variable `v`. */
  def apply(v: Variable): NonEmptyKinding =
    new NonEmptyKinding {
      val underlyingMap: Map[Variable, Finite[Type]] = Map.empty
      val head: Variable = v
    }

  /** Creates a new empty Kinding. */
  def empty: Kinding =
    new Kinding {
      val underlyingMap: Map[Variable, Finite[Type]] = Map.empty

      override def merge(other: Kinding): Kinding = other
      override def merge(other: NonEmptyKinding): NonEmptyKinding = other
    }
}