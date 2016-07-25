package de.tu_dortmund.cs.ls14.cls.interpreter

import de.tu_dortmund.cs.ls14.cls.inhabitation.Tree
import de.tu_dortmund.cs.ls14.cls.types.{Arrow, Constructor, Taxonomy, Type}

import scala.annotation.StaticAnnotation
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.reflect.NameTransformer

class combinator extends StaticAnnotation

case class CombinatorInfo(name: String,
  parameters: Option[Seq[universe.Type]],
  result: universe.Type,
  semanticType: Type)

trait ReflectedRepository[A] {
  import ReflectedRepository._
  import scala.reflect.runtime.currentMirror
  import scala.tools.reflect.ToolBox

  val typeTag: WeakTypeTag[A]
  val instance: A

  private lazy val tb = currentMirror.mkToolBox()

  lazy val combinatorComponents = {
    typeTag.tpe.members.flatMap (member =>
        member.annotations.foldLeft[Seq[CombinatorInfo]](Seq()) {
          case (Seq(), c) if c.tree.tpe == universe.typeOf[combinator] =>
            val combinatorName = member.name.toString
            val applyMethod = member.typeSignature.member(TermName("apply")).asMethod
            val (applyMethodParameters, applyMethodResult) =
              applyMethod.typeSignature match {
                case NullaryMethodType(result) => (None, result.finalResultType)
                case MethodType(params, result) => (Some(params.map(p => p.info)), result)
              }
            val semanticType =
              tb.eval(
                q"""${reify(instance).in(tb.mirror)}
                      .asInstanceOf[${typeTag.tpe}]
                      .${TermName(combinatorName)}
                      .semanticType"""
              ).asInstanceOf[Type]
            Seq(CombinatorInfo(combinatorName, applyMethodParameters, applyMethodResult, semanticType))
          case (s, _) => s
        }
    ).map(cInfo => cInfo.name -> cInfo).toMap
  }

  lazy val scalaTypes: Set[universe.Type] =
    combinatorComponents.values.flatMap(combinatorInfo =>
      combinatorInfo.parameters.map(_.toSet).getOrElse(Set.empty) + combinatorInfo.result
    ).toSet

  lazy val nativeTypes: Set[Constructor] =
    scalaTypes.map((ty: universe.Type) => nativeTypeOf(ty))

  lazy val combinators: Map[String, Type] =
    combinatorComponents
      .mapValues(combinatorInfo => nativeTypeOf(combinatorInfo))

  lazy val nativeTypeTaxonomy: Taxonomy = {
    val (taxonomyStatement, _) =
      scalaTypes.foldLeft(
        (q"Taxonomy(${nativeTypeOf[Any].name})",
          Set.empty[universe.Type])
      ){ case ((stmt, tys), ty) =>
        val newStmt =
          tys.foldLeft(stmt){ case (newStmt, otherTy) =>
            q"${newStmt}.merge(NativeTaxonomy[${ty}, ${otherTy}])"
          }
        (newStmt, tys + ty)
      }
    tb.eval(q"""{
      import de.tu_dortmund.cs.ls14.cls.interpreter.NativeTaxonomy;
      import de.tu_dortmund.cs.ls14.cls.types.Taxonomy;
      $taxonomyStatement
      }""").asInstanceOf[Taxonomy]
  }

  def evalInhabitant[A](inhabitant: Tree): A = {
    val instanceTerm = q"${reify(instance).in(tb.mirror)}.asInstanceOf[${typeTag.tpe}]"
    def toTermName(name: String) =
      TermName(NameTransformer.encode(name))
    def constructTerm(inhabitant: Tree): universe.Tree =
      inhabitant match {
        case Tree(name)
          if combinatorComponents(name).parameters.isEmpty =>
          q"$instanceTerm.${toTermName(name)}.apply"
        case Tree(name) =>
          q"$instanceTerm.${toTermName(name)}()"
        case Tree(name, arguments@_*) =>
          q"$instanceTerm.${toTermName(name)}(..${arguments.map(constructTerm)})"
      }
    tb.eval(q"${constructTerm(inhabitant)}").asInstanceOf[A]
  }

}

object ReflectedRepository {
  def nativeTypeOf[A](implicit aTag: WeakTypeTag[A]): Constructor =
    nativeTypeOf(aTag.tpe)

  def nativeTypeOf(ty: universe.Type): Constructor =
    Constructor(ty.dealias.typeSymbol.fullName)

  def nativeTypeOf(combinatorInfo: CombinatorInfo): Type =
    combinatorInfo.parameters
      .map(_.toSet)
      .getOrElse(Set.empty)
      .map((x: universe.Type) => nativeTypeOf(x))
      .foldRight[Type](nativeTypeOf(combinatorInfo.result)) {
        case (parameter, result) => Arrow(parameter, result)
      }

  def apply[R](inst: R)(implicit tag: WeakTypeTag[R]): ReflectedRepository[R] =
    new ReflectedRepository[R] {
      val typeTag = tag
      val instance = inst
    }
}



