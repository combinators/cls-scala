package de.tu_dortmund.cs.ls14.cls.interpreter

import de.tu_dortmund.cs.ls14.cls.types.{Omega, Taxonomy, Type}

import scala.annotation.StaticAnnotation
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

class combinator extends StaticAnnotation

trait Repository[A] {
  val typeTag: WeakTypeTag[A]
  val instance: A

  case class CombinatorInfo(name: String,
                            args: Seq[universe.Type],
                            result: universe.Type,
                            semanticType: Type)
  def combinatorComponents() = {
    import scala.reflect.runtime.currentMirror
    import scala.tools.reflect.ToolBox
    lazy val tb = currentMirror.mkToolBox()

    typeTag.tpe.baseClasses.flatMap(c => c.info.members).flatMap {
      case m =>
        m.annotations.foldLeft[Seq[CombinatorInfo]](Seq()) {
          case (Seq(), c) if c.tree.tpe == universe.typeOf[combinator] =>
            val module = m
            val applyMeth = module.typeSignature.member(TermName("apply")).asMethod
            val semMeth = TermName(s"semanticType")

            Seq(CombinatorInfo(module.name.toString,
              applyMeth.typeSignature match {
                case MethodType(params, r) => params.map(p => p.info)
              },
              applyMeth.returnType,
              tb.eval(
                q"""
                  val impl: ${typeTag.tpe} = ${reify(instance).in(tb.mirror)}.asInstanceOf[${typeTag.tpe}];
                  import impl.g;
                  g.$semMeth""").asInstanceOf[Type]))
          case (s, _) => s
        }
    }
  }

  /*lazy val combinators: Map[String, Type] =


  layz val inferredTaxonomy: Taxonomy*/
}

object Repository {

  def apply[R](inst: R)(implicit tag: WeakTypeTag[R]): Repository[R] =
    new Repository[R] {
      val typeTag = tag
      val instance = inst
    }
}



