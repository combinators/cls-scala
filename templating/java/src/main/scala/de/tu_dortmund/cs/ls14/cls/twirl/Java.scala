package de.tu_dortmund.cs.ls14.cls.twirl

import java.io.StringReader

import com.github.javaparser._
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.{BodyDeclaration, ConstructorDeclaration, FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.{Expression, Name, NameExpr, SimpleName}
import com.github.javaparser.ast.stmt.Statement
import com.github.javaparser.ast.{CompilationUnit, ImportDeclaration, Node}
import org.apache.commons.text.StringEscapeUtils
import play.twirl.api.{BufferedContent, Format, Formats}

import scala.collection.JavaConverters._
import scala.collection.immutable

/** A Java fragment. */
class Java private(elements: immutable.Seq[Java], text: String) extends BufferedContent[Java](elements, text) {
  def this(text: String) = this(Nil, Formats.safe(text))
  def this(elements: immutable.Seq[Java]) = this(elements, "")

  private lazy val fullText: String = (text +: elements).mkString

  /** Content type of Java */
  val contentType = "text/x-java"

  /** Parses this element as a java compilation unit. */
  def compilationUnit(): CompilationUnit = JavaParser.parse(new StringReader(fullText))

  /** Parses an import declaration. */
  def importDeclaration(): ImportDeclaration = JavaParser.parseImport(fullText)

  /** Parses this element as a single statement. */
  def statement(): Statement = JavaParser.parseStatement(fullText)

  /** Parses this element as multiple statements. */
  def statements(): Seq[Statement] = JavaParser.parseBlock(s"{ $fullText }").getStatements.asScala

  /** Parses this element as an expression. */
  def expression[T <: Expression](): T = JavaParser.parseExpression[T](fullText)

  /** Parses this element as a (potentially qualified) name. */
  def name(): Name =
    JavaParser.parseName(fullText)

  /** Parses this element as an unqualified name. */
  def simpleName(): SimpleName =
    JavaParser.parseExpression[NameExpr](fullText).getName

  /** Parses this element as a (unqualified) name expression. */
  def nameExpression(): NameExpr = expression()

  /** Parses this element as a class body declaration (e.g. a method or a field). */
  def classBodyDeclaration(): BodyDeclaration[_] = JavaParser.parseBodyDeclaration(fullText)

  /** Parses this element as multiple class body declarations. */
  def classBodyDeclarations(): Seq[BodyDeclaration[_]] =
    JavaParser.parse(s"class C { $fullText }").getTypes.asScala.head.getMembers.asScala

  /**
    * Parses this element as multiple field declarations.
    */
  def fieldDeclarations(): Seq[FieldDeclaration] =
    classBodyDeclarations().map(_.asInstanceOf[FieldDeclaration])

  /**
    * Parses this element as multiple method declarations.
    */
  def methodDeclarations(): Seq[MethodDeclaration] =
    classBodyDeclarations().map(_.asInstanceOf[MethodDeclaration])

  /**
    * Parses this element as multiple constructor declarations.
    */
  def constructors(): Seq[ConstructorDeclaration] =
    classBodyDeclarations().map(_.asInstanceOf[ConstructorDeclaration])

  /** Parses this element as an interface body declaration (e.g. a method signature). */
  def interfaceBodyDeclaration(): BodyDeclaration[_] = JavaParser.parseBodyDeclaration(fullText)

  /** Parses this element as a type (e.g. the in  X foo = (X)bar). */
  def tpe(): Type = JavaParser.parseType(fullText)
}

/** Helper for Java utility methods. */
object Java {
  /** Creates a Java fragment with initial content specified. */
  def apply(text: String): Java = {
    new Java(text)
  }

  /** Creates a Java fragment with initial content from the given `text` separated by `separator`. */
  def apply(text: Seq[String], separator: String = ";"): Java = {
    apply(text.mkString(separator))
  }

  /** Creates a Java fragment with initial content from the ast `node`. */
  def apply(node: Node): Java = Java(node.toString)

  /** Creates a Java fragment with initial content from the asts `nodes`. */
  def apply(nodes: Seq[Node]): Java = new Java(immutable.Seq(nodes map apply : _*))
}

object JavaFormat extends Format[Java] {
  /** Integrates `text` without performing any escaping process.
    *
    * @param text Text to integrate
    */
  def raw(text: String): Java = Java(text)

  /** Escapes `text` using Java String rules.
    *
    * @param text Text to integrate
    */
  def escape(text: String): Java = Java(StringEscapeUtils.escapeJava(text))

  /** Generates an empty Java fragment */
  val empty: Java = new Java("")

  /** Creates an Java Fragment that holds other fragments. */
  def fill(elements: immutable.Seq[Java]): Java = new Java(elements)

}
