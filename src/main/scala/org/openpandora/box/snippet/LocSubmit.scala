package org.openpandora.box.snippet

import net.liftweb.http.DispatchSnippet
import net.liftweb.http.S
import scala.xml.NodeSeq
import scala.xml.Text

class LocSubmit extends DispatchSnippet {
  def dispatch = {
    case _ => render _
  }

  def render(what: NodeSeq) = (<input type="submit" value={S.attr("locid") map S.? getOrElse what.text}/>)
}
