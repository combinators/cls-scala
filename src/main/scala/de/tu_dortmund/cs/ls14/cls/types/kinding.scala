package de.tu_dortmund.cs.ls14.cls.types

import shapeless.feat.Enumeration

sealed trait Kinding extends (Variable => Enumeration[Type]) { self =>
  protected val underlyingMap: Map[Variable, Enumeration[Type]]
  protected val head: Variable

  def addOptions(options: Enumeration[Type]) =
    new Kinding {
      val underlyingMap: Map[Variable, Enumeration[Type]] =
        underlyingMap.updated(self.head, self(self.head).union(options))
      val head: Variable = self.head
    }

  def addOption(ty: Type): Kinding =
    addOptions(Enumeration.singleton(ty))

  def merge(other: Kinding): Kinding =
    new Kinding {
      val underlyingMap: Map[Variable, Enumeration[Type]] =
        other.underlyingMap.foldLeft(self.underlyingMap) {
          case (m, (k, v)) => m.updated(k, m.getOrElse(k, Enumeration.empty).union(v))
        }
      val head: Variable = self.head
    }

  def apply(v: Variable): Enumeration[Type] =
    underlyingMap.getOrElse(v, Enumeration.empty)
}

object Kinding {
  def apply(v: Variable): Kinding =
    new Kinding {
      val underlyingMap: Map[Variable, Enumeration[Type]] = Map.empty
      val head: Variable = v
    }
}