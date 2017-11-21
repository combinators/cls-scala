package org.combinators.cls.examples

import org.combinators.cls.inhabitation._
import org.combinators.cls.interpreter.ReflectedRepository._
import org.combinators.cls.interpreter._
import org.combinators.cls.types._
import org.combinators.cls.types.syntax._

object Newsticker extends App {

  lazy val alpha = Variable("alpha")
  lazy val beta = Variable("beta")

  trait MessageRepository {
    @combinator object InhabitationRocks {
      def apply: String = "Inhabitation rocks!"
      val semanticType: Type = 'TrueMessage
    }
    @combinator object Productive {
      def apply: String = "Inhabitation makes developers super productive!"
      val semanticType: Type = 'TrueMessage
    }
  }

  trait HeadlineRepository {
    @combinator object InhabitationNews {
      def apply: String = "Inhabitation News"
      val semanticType: Type = 'Headline
    }
  }

  trait UtilRepository {
    @combinator object append {
      def apply(message: String, messages: List[String]): List[String] = message +: messages
      val semanticType: Type  = alpha =>: 'List(alpha) =>: 'List(alpha) :&: 'NonEmpty
    }
    @combinator object emptyList {
      def apply: List[String] = List.empty
      val semanticType: Type = 'List(alpha) :&: 'Empty
    }
  }

  trait Repository extends MessageRepository with HeadlineRepository with UtilRepository {
    @combinator object newsticker {
      def apply(theTitle: String, messages: List[String]): String = (theTitle +: messages).mkString("\n")
      val semanticType: Type = 'Headline =>: 'List('Message) :&: beta =>: 'Newsticker :&: beta
    }
  }

  lazy val semanticTaxonomy =
    Taxonomy("Message")
      .addSubtype("TrueMessage")
      .merge(
        Taxonomy("ListCharacteristic")
          .addSubtype("Empty")
          .addSubtype("NonEmpty")
      )
  lazy val kinding =
    Kinding(alpha)
      .addOption('Message)
      .merge(
        Kinding(beta)
          .addOption('Empty)
          .addOption('NonEmpty)
      )

  lazy val Gamma = new Repository {}
  lazy val reflectedGamma = ReflectedRepository(Gamma, semanticTaxonomy, kinding)
  lazy val inhabitationResult = reflectedGamma.inhabit[String]('Newsticker, 'NonEmpty)

  lazy val item =
    if (args.length > 0) args(0).toInt
    else new java.util.Random().ints(0, 2000).findFirst().getAsInt
  println(inhabitationResult.interpretedTerms.index(item))

}