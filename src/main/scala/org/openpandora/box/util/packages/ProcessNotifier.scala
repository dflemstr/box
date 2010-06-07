package org.openpandora.box.util.packages

import java.util.Locale
import java.util.ResourceBundle
import net.liftweb.common.Logger
import net.liftweb.http.LiftRules
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import net.liftweb.util.NamedPF
import org.openpandora.box.model.User
import org.openpandora.box.util.Localization._
import org.openpandora.box.util.notifications.Poster
import scala.actors.Actor
import scala.xml._

object ProcessNotifier extends Actor
                          with Logger {
  trait Message {
    val kind: String
    val replacements: Map[String, String] = Map.empty
    def localization = "validation." + kind
    def bodyLocalization = localization + ".body"
    def titleLocalization = localization + ".title"
  }

  trait WarningMessage extends AnyRef
                          with Message

  trait ErrorMessage   extends Throwable
                          with Message

  case object PxmlNotFoundError extends ErrorMessage {
    val kind = "pxmlnotfounderror"
  }

  case object PxmlTruncatedError extends ErrorMessage {
    val kind = "pxmltruncatederror"
  }

  case class XmlSyntaxError(error: String) extends ErrorMessage {
    val kind = "xmlsyntaxerror"
    override val replacements = Map("error" -> error)
  }

  case class PxmlSyntaxError(error: String) extends ErrorMessage {
    val kind = "pxmlsyntaxerror"
    override val replacements = Map("error" -> error)
  }

  case object PngInvalidError extends ErrorMessage {
    val kind = "pnginvaliderror"
  }

  case class PxmlSyntaxWarning(error: String) extends WarningMessage {
    val kind = "pxmlsyntaxwarning"
    override val replacements = Map("error" -> error)
  }

  case object PackageAdded extends Message {
    val kind = "added"
  }

  case class SendResolvedMessage(message: Message, user: User, filename: String)

  def act = Actor.loop {
    Actor.react {
      case SendResolvedMessage(message, user, filename) =>
        import Poster._
        val transformedTitle = loc(message.titleLocalization, user.language).replace("%filename%", Text(filename).text)
        val transformedBody = message.replacements.foldLeft(loc(message.bodyLocalization, user.language))((res, n) => res.replace("%" + n._1 + "%", Text(n._2).text))
        val kind = message match {
          case err: ErrorMessage => "error"
          case warn: WarningMessage => "warning"
          case _ => "notice"
        }
        Poster! SendMessage(user.id, kind, LiftRules.localizeStringToXml(transformedTitle), LiftRules.localizeStringToXml(transformedBody))
        ProcessNotifier.this.info("Sent process message, user=" + user.id + " filename=" + filename)
      case x => warn("Unhandled message in ProcessNotifier: " + x)
    }
  }
}
