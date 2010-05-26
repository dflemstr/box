package org.openpandora.box.snippet

import net.liftweb.http.DispatchSnippet
import net.liftweb.http.LiftRules
import net.liftweb.http.SHtml
import net.liftweb.http.js.JE
import net.liftweb.http.js.JsCmds
import org.openpandora.box.model.User
import scala.xml.NodeSeq

object Notifications {
  private val showIndicator =
    ((_: NodeSeq) => <lift:comet type="NotificationDispatcher" name={(User.currentUser.map(_.id) getOrElse 0).toString}/>)
}

class Notifications extends DispatchSnippet {
  def dispatch = {
    case "indicator" => Notifications.showIndicator
    case "script" => script
    case "test" =>((x: NodeSeq) =>
        SHtml.ajaxButton("Test notification",
                         () => JE.Call("displayNotice", JE.Str("notice"), JE.Str("Test"), JE.Str("This is a test message"), JE.JsNull).cmd)
      )
  }
  def script(template: NodeSeq) = {
    val cmd = LiftRules.noticesToJsCmd()
    if(cmd == JsCmds.Noop)
      NodeSeq.Empty
    else
      JsCmds.Script(JsCmds.OnLoad(cmd))
  }
}
