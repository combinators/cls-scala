/*
 * Copyright 2017 Jan Bessai
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.combinators.cls.examples

import org.combinators.cls.inhabitation._
import org.combinators.cls.interpreter.ReflectedRepository._
import org.combinators.cls.interpreter._
import org.combinators.cls.types._
import org.combinators.cls.types.syntax._

object Newsticker extends App {

  lazy val alpha = Variable("alpha")
  lazy val beta = Variable("beta")

  object SemanticTypes {
    val Message: Type = Constructor("Message")
    val TrueMessage: Type = Constructor("TrueMessage")
    val Headline: Type = Constructor("Headline")
    val NonEmpty: Type = Constructor("NonEmpty")
    val Empty: Type = Constructor("Empty")
    val NewsTicker: Type = Constructor("NewsTicker")
    def NewsList(elemTpe: Type): Type = Constructor("NewsList", elemTpe)
  }
  import SemanticTypes._

  trait MessageRepository {
    @combinator object InhabitationRocks {
      def apply: String = "Inhabitation rocks!"
      val semanticType: Type = TrueMessage
    }
    @combinator object Productive {
      def apply: String = "Inhabitation makes developers super productive!"
      val semanticType: Type = TrueMessage
    }
  }

  trait HeadlineRepository {
    @combinator object InhabitationNews {
      def apply: String = "Inhabitation News"
      val semanticType: Type = Headline
    }
  }

  trait UtilRepository {
    @combinator object append {
      def apply(message: String, messages: List[String]): List[String] =
        message +: messages
      val semanticType: Type =
        alpha =>: NewsList(alpha) =>: NewsList(alpha) :&: NonEmpty
    }
    @combinator object emptyList {
      def apply: List[String] = List.empty
      val semanticType: Type = NewsList(alpha) :&: Empty
    }
  }

  trait Repository
      extends MessageRepository
      with HeadlineRepository
      with UtilRepository {
    @combinator object newsticker {
      def apply(theTitle: String, messages: List[String]): String =
        (theTitle +: messages).mkString("\n")
      val semanticType: Type =
        Headline =>: NewsList(Message) :&: beta =>: NewsTicker :&: beta
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
      .addOption(Message)
      .merge(
        Kinding(beta)
          .addOption(Empty)
          .addOption(NonEmpty)
      )

  lazy val Gamma = new Repository {}
  lazy val reflectedGamma =
    ReflectedRepository(Gamma, semanticTaxonomy, kinding)
  lazy val inhabitationResult =
    reflectedGamma.inhabit[String](NewsTicker, NonEmpty)

  lazy val item =
    if (args.length > 0) args(0).toInt
    else new java.util.Random().ints(0, 2000).findFirst().getAsInt
  println(inhabitationResult.interpretedTerms.index(item))

}
