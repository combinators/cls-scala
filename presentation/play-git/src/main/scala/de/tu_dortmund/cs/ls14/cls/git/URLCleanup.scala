package de.tu_dortmund.cs.ls14.cls.git

import play.api.mvc.InjectedController

/** Cleans leading double slashes of urls.
  * Turns `http://foo.bar//baz/` into `http://foo.bar/baz/`.
  */
class URLCleanup extends InjectedController {
  def untrail(path: String) = Action {
    MovedPermanently("/" + path)
  }
}
