package org.openpandora.box.util.notifications

import net.liftweb.common.Logger
import org.openpandora.box.comet.NotificationDispatcher
import scala.actors.Actor
import scala.xml.NodeSeq

object Poster extends Actor
                 with Logger {
  case class SendMessage(to: Long, kind: String, title: NodeSeq, message: NodeSeq)
  case class AddListener(dispatcher: NotificationDispatcher)
  case class RemoveListener(dispatcher: NotificationDispatcher)
  
  case class PostMessage(kind: String, title: NodeSeq, message: NodeSeq)

  @volatile private var listeners: Map[Long, NotificationDispatcher] = Map.empty

  def act = Actor.loop {
    Actor.react {
      case SendMessage(to, kind, title, message) => listeners.get(to).foreach(_! PostMessage(kind, title, message))
      case AddListener(indicator) => listeners += (indicator.userId -> indicator)
      case RemoveListener(indicator) => listeners -= indicator.userId
    }
  }
}
