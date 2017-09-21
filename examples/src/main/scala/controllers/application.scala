package controllers

import inhabitation.NewstickerInhabitation
import play.api.mvc._

class Application extends InjectedController {

  def index = Action {
    Redirect(s"/news/${new java.util.Random().ints(0, 2000).findFirst().getAsInt}");
  }

  def news(item: Int) = Action {
    Ok(NewstickerInhabitation.inhabitationResult.interpretedTerms.index(item))
  }

}