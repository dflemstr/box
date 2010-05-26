package org.openpandora.box.comet

import net.liftweb.common.Logger
import net.liftweb.http.CometActor
import net.liftweb.http.js.JE._
import org.openpandora.box.util.notifications.Poster
import org.openpandora.box.util.notifications.Poster._
import scala.xml.NodeSeq

class NotificationDispatcher extends CometActor with Logger {
  var userId: Long = -1l

  def render = NodeSeq.Empty

  override def localSetup()    = {
    userId = name map (_.toLong) openOr -1l
    Poster! AddListener(this)
  }
  override def localShutdown() = Poster! RemoveListener(this)

  override def lowPriority = {
    case PostMessage(kind, title, body) => partialUpdate(Call("displayNotice", Str(kind), Str(title.toString), Str(body.toString), JsNull).cmd)
  }
}
