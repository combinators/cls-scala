package de.tu_dortmund.cs.ls14.cls.persistable

import java.nio.file.Paths

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.expr.{Name, NameExpr}
import com.github.javaparser.ast.visitor.GenericVisitorAdapter

import scala.collection.JavaConverters._

trait JavaPersistable extends Persistable

object JavaPersistable {
  type Aux[TT] = Persistable { type T = TT }

  /** Persistable instance for a compilation unit.
    * Derives path and file names from the package and the name of the first declared type.
    */
  implicit def compilationUnitInstance: Aux[CompilationUnit] =
    new Persistable {
      type T = CompilationUnit
      override def rawText(compilationUnit: CompilationUnit) = compilationUnit.toString
      override def path(compilationUnit: CompilationUnit) = {
        val pkg: Seq[String] =
          compilationUnit.getPackageDeclaration.orElse(null) match {
            case null => Seq.empty
            case somePkg =>
              somePkg.accept(new GenericVisitorAdapter[Seq[String], Unit] {
                  override def visit(name: NameExpr, arg: Unit): Seq[String] = Seq(name.getNameAsString)
                  override def visit(name: Name, arg: Unit): Seq[String] =
                    Option(name.getQualifier.orElse(null))
                      .map((q: Name) => q.accept(this, arg))
                      .getOrElse(Seq.empty[String]) :+ name.getIdentifier
                },
                ()
              )
          }
        val clsName = s"${compilationUnit.getTypes.asScala.head.getName}.java"
        val fullPath = "src" +: "main" +: "java" +: pkg :+ clsName
        Paths.get(fullPath.head, fullPath.tail : _*)
      }
    }

  def apply[T](implicit persistable: Aux[T]): Aux[T] = persistable
}