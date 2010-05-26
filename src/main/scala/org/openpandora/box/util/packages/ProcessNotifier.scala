package org.openpandora.box.util.packages

import net.liftweb.common.Logger
import net.liftweb.util.Helpers._
import org.openpandora.box.model.User
import org.openpandora.box.util.notifications.Poster
import scala.actors.Actor
import scala.xml.NodeSeq

object ProcessNotifier extends Actor
                          with Logger {
  trait Message {
    val title: NodeSeq
    val body: NodeSeq
  }

  trait WarningMessage extends AnyRef
                          with Message

  trait ErrorMessage   extends Throwable
                          with Message

  private val pxmlStuff = <p>Please read <a href="http://pandorawiki.org/PXML_specification">the PXML specification</a> for more information.</p>

  case object PxmlNotFoundError extends ErrorMessage {
    val title = <p><title:filename/> didn't contain PXML data</p>
    val body = <p>The package didn't contain any <em>PXML</em> metadata.</p> ++ pxmlStuff
  }

  case object PxmlTruncatedError extends ErrorMessage {
    val title = <p><title:filename/> is missing a &lt;/PXML&gt; tag</p>
    val body = <p>The <em>PXML</em> ends without a closing &lt;/PXML&gt; tag, which makes it <em>impossible</em> for us to load it.</p>
  }

  case class XmlSyntaxError(error: String) extends ErrorMessage {
    val title = <p><title:filename/> contains invalid XML data</p>
    val body = <p>The error was:</p> ++ <code>{error}</code> ++ pxmlStuff
  }

  case class PxmlSyntaxError(error: String) extends ErrorMessage {
    val title = <p><title:filename/> contains invalid PXML markup</p>
    val body = <p>The error was:</p> ++ <code>{error}</code> ++ pxmlStuff
  }

  case object PngInvalidError extends ErrorMessage {
    val title = <p><title:filename/> has an invalid preview picture</p>
    val body = <p>There is a block of data with a <em>PNG</em> header after your <em>PXML</em> file. We tried to open it as an image, but weren't able to.</p>
  }

  case class PxmlSyntaxWarning(error: String) extends WarningMessage {
    val title = <p><title:filename/> has minor issues</p>
    val body = <p>The package has issues, but was accepted anyways. The system says:</p> ++ <code>{error}</code>
  }

  case class PackageAdded() extends Message {
    val title = <p><title:filename/> validated and added</p>
    val body = <p>You can find the applications it contained among all of the other applications.</p>
  }

  case class SendResolvedMessage(message: Message, user: User, filename: String)

  def act = Actor.loop {
    Actor.react {
      case m: SendResolvedMessage =>
        import Poster._
        val transformedTitle = bind("title", m.message.title, "filename" -> m.filename)
        val kind = m.message match {
          case err: ErrorMessage => "error"
          case warn: WarningMessage => "warning"
          case _ => "notice"
        }
        Poster! SendMessage(m.user.id, kind,  transformedTitle, m.message.body)
        ProcessNotifier.this.info("Sent process message, user=" + m.user.id + " filename=" + m.filename)
      case x => warn("Unhandled message in ProcessNotifier: " + x)
    }
  }
}
