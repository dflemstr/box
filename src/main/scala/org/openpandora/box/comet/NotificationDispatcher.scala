package org.openpandora.box.comet

import net.liftweb.common.Logger
import net.liftweb.http.CometActor
import net.liftweb.http.js.JE._
import org.openpandora.box.util.notifications.Poster
import org.openpandora.box.util.notifications.Poster._
import scala.xml.NodeSeq

trait NotificationDispatcher {
  def userId: Long
  def postMessage(kind: String, title: NodeSeq, body: NodeSeq)
}

class NotificationDispatcherComet extends NotificationDispatcher
                                     with CometActor
                                     with Logger {
  var userId: Long = -1l

  def render = NodeSeq.Empty
  def postMessage(kind: String, title: NodeSeq, body: NodeSeq) =
    this! PostMessage(kind, title, body)

  case class PostMessage(kind: String, title: NodeSeq, body: NodeSeq)

  override def localSetup()    = {
    userId = name map (_.toLong) openOr -1l
    Poster.default.registerNotificationDispatcher(this)
  }
  override def localShutdown() = Poster.default.unregisterNotificationDispatcher(this)

  override def lowPriority = {
    case PostMessage(kind, title, body) => partialUpdate(Call("displayNotice", Str(kind), Str(title.toString), Str(body.toString), JsNull).cmd)
  }
}
