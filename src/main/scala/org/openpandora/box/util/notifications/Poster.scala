package org.openpandora.box.util.notifications

import net.liftweb.common.Logger
import org.openpandora.box.comet.NotificationDispatcher
import scala.actors.Actor
import scala.xml.NodeSeq

object Poster {
  val default: Poster = new PosterImpl
}

trait Poster {
  def sendMessage(to: Long, kind: String, title: NodeSeq, message: NodeSeq)
  def registerNotificationDispatcher(dispatcher: NotificationDispatcher)
  def unregisterNotificationDispatcher(dispatcher: NotificationDispatcher)
}

private[notifications] class PosterImpl extends Poster
                                           with Actor
                                           with Logger {
  start()

  def sendMessage(to: Long, kind: String, title: NodeSeq, message: NodeSeq) =
    this! SendMessage(to, kind, title, message)

  def registerNotificationDispatcher(dispatcher: NotificationDispatcher) =
    this! AddListener(dispatcher)

  def unregisterNotificationDispatcher(dispatcher: NotificationDispatcher) =
    this! RemoveListener(dispatcher)

  case class SendMessage(to: Long, kind: String, title: NodeSeq, message: NodeSeq)
  case class AddListener(dispatcher: NotificationDispatcher)
  case class RemoveListener(dispatcher: NotificationDispatcher)

  @volatile var listeners: Map[Long, NotificationDispatcher] = Map.empty

  def act = Actor.loop {
    Actor.react {
      case SendMessage(to, kind, title, message) => listeners.get(to).foreach(_.postMessage(kind, title, message))
      case AddListener(indicator) => listeners += (indicator.userId -> indicator)
      case RemoveListener(indicator) => listeners -= indicator.userId
    }
  }
}
