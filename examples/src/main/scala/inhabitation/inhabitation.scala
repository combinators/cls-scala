package inhabitation

import de.tu_dortmund.cs.ls14.cls.inhabitation._
import de.tu_dortmund.cs.ls14.cls.interpreter.ReflectedRepository._
import de.tu_dortmund.cs.ls14.cls.interpreter._
import de.tu_dortmund.cs.ls14.cls.types._
import de.tu_dortmund.cs.ls14.cls.types.syntax._
import play.twirl.api.Html

object NewstickerInhabitation extends App {

  lazy val alpha = Variable("alpha")
  lazy val beta = Variable("beta")

  trait MessageRepository {
    @combinator object InhabitationRocks {
      def apply: Html = Html("<h1>Inhabitation rocks!</h1>")
      val semanticType = 'TrueMessage
    }
    @combinator object Productive {
      def apply: Html = Html("<h1>Inhabitation makes developers super productive!</h1>")
      val semanticType = 'TrueMessage
    }
  }

  trait HeadlineRepository {
    @combinator object InhabitationNews {
      def apply: String = "Inhabitation News"
      val semanticType = 'Headline
    }
  }

  trait UtilRepository {
    @combinator object append {
      def apply(message: Html, messages: List[Html]): List[Html] = message +: messages
      val semanticType = alpha =>: 'List(alpha) =>: 'List(alpha) :&: 'NonEmpty
    }
    @combinator object emptyList {
      def apply: List[Html] = List.empty
      val semanticType = 'List(alpha) :&: 'Empty
    }
  }

  trait Repository extends MessageRepository with HeadlineRepository with UtilRepository {
    @combinator object newsticker {
      def apply(theTitle: String, messages: List[Html]): Html = views.html.newsticker(theTitle, messages)
      val semanticType = 'Headline =>: 'List('Message) :&: beta =>: 'Newsticker :&: beta
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
  lazy val inhabitationResult = reflectedGamma.inhabit[Html]('Newsticker, 'NonEmpty)
}