package org.openpandora.box.dispatch

import net.liftweb.common.Box
import net.liftweb.http.LiftResponse
import net.liftweb.http.Req
import scala.PartialFunction

trait Dispatcher {
  def dispatch: PartialFunction[Req, () => Box[LiftResponse]]
}
