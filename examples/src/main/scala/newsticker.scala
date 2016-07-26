import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

import de.tu_dortmund.cs.ls14.cls.types._
import de.tu_dortmund.cs.ls14.cls.inhabitation._
import de.tu_dortmund.cs.ls14.cls.interpreter._
import play.twirl.api.Html
import syntax._
import ReflectedRepository._

object Newsticker extends App {

  val alpha = Variable("alpha")
  val beta = Variable("beta")

  trait MessageRepository {
    @combinator object InhabitationRocks {
      def apply: Html = Html("<h1>Inhabitation Rocks</h1>")
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
      def apply(theTitle: String, messages: List[Html]): Html = Html("") //templates.newsticker(theTitle, messages)
      val semanticType = 'Headline =>: 'List('Message) :&: beta =>: 'Newsticker :&: beta
    }
  }

  val semanticTaxonomy =
    Taxonomy("Message")
      .addSubtype("TrueMessage")
      .merge(
        Taxonomy("ListCharacteristic")
          .addSubtype("Empty")
          .addSubtype("NonEmpty")
      )
  val kinding =
    Kinding(alpha)
      .addOption('Message)
      .merge(
        Kinding(beta)
          .addOption('Empty)
          .addOption('NonEmpty)
      )

  val Gamma = new Repository {}
  val reflectedGamma = ReflectedRepository(Gamma)
  val fullTaxonomy = reflectedGamma.nativeTypeTaxonomy.merge(semanticTaxonomy)
  val algorithm = new BoundedCombinatoryLogic(kinding, new SubtypeEnvironment(fullTaxonomy), reflectedGamma.combinators)

  val target = nativeTypeOf[Html] :&: 'Newsticker :&: 'NonEmpty
  val solutions =
    TreeGrammarEnumeration(algorithm.inhabit(target), target)
    .map(reflectedGamma.evalInhabitant[Html](_))

  for (i <- 1 to 10) {
    val file = Paths.get("newsticker_$i.html")
    val content = new java.util.ArrayList[String]()
    content.add(solutions.index(i).toString)
    Files.write(file, content, Charset.forName("UTF-8"))
  }
}