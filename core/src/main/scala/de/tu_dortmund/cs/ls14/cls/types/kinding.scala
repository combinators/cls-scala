package de.tu_dortmund.cs.ls14.cls.types

import shapeless.feat.Enumeration

sealed trait Kinding extends (Variable => Enumeration[Type]) {
  val underlyingMap: Map[Variable, Enumeration[Type]]
  def merge(other: Kinding): Kinding
  def merge(other: NonEmptyKinding): NonEmptyKinding

  def apply(v: Variable): Enumeration[Type] =
    underlyingMap.getOrElse(v, Enumeration.empty)
}

sealed trait NonEmptyKinding extends Kinding { self =>
  val underlyingMap: Map[Variable, Enumeration[Type]]
  protected val head: Variable

  def addOptions(options: Enumeration[Type]) =
    new NonEmptyKinding {
      val underlyingMap: Map[Variable, Enumeration[Type]] =
        self.underlyingMap.updated(self.head, self(self.head).union(options))
      val head: Variable = self.head
    }

  def addOption(ty: Type): NonEmptyKinding =
    addOptions(Enumeration.singleton(ty))

  override def merge(other: Kinding): NonEmptyKinding =
    new NonEmptyKinding {
      val underlyingMap: Map[Variable, Enumeration[Type]] =
        other.underlyingMap.foldLeft(self.underlyingMap) {
          case (m, (k, v)) => m.updated(k, m.getOrElse(k, Enumeration.empty).union(v))
        }
      val head: Variable = self.head
    }
  override def merge(other: NonEmptyKinding): NonEmptyKinding =
    merge(other.asInstanceOf[Kinding])
}

object Kinding {
  def apply(v: Variable): NonEmptyKinding =
    new NonEmptyKinding {
      val underlyingMap: Map[Variable, Enumeration[Type]] = Map.empty
      val head: Variable = v
    }

  def empty: Kinding =
    new Kinding {
      val underlyingMap: Map[Variable, Enumeration[Type]] = Map.empty

      override def merge(other: Kinding): Kinding = other
      override def merge(other: NonEmptyKinding): NonEmptyKinding = other
    }
}