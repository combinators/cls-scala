package de.tu_dortmund.cs.ls14.cls.types

trait TypeSyntax {
  val ty: Type
  def :&:(other: Type): Type =
    Intersection(other, ty)
  def =>:(other: Type): Type =
    Arrow(other, ty)
}
trait ToTypeSyntax {
  implicit def toTypeSyntax(fromTy: Type): TypeSyntax =
    new TypeSyntax {
      lazy val ty: Type = fromTy
    }
}

trait ConstructorSyntax {
  val name: Symbol
  def apply(arg: Type, args: Type*): Constructor =
    Constructor(name.name, arg +: args:_*)
}
trait ToConstructorSyntax extends ToTypeSyntax {
  implicit def toConstructor(name: Symbol): Constructor =
    Constructor(name.name)
  implicit def toTypeSyntax(name: Symbol): TypeSyntax =
    new TypeSyntax {
      lazy val ty: Type = new Constructor(name.name)
    }
  implicit def toConstructorSyntax(fromName: Symbol): ConstructorSyntax =
    new ConstructorSyntax {
      lazy val name: Symbol = fromName
    }
}

object syntax extends ToConstructorSyntax
