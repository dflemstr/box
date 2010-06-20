package org.openpandora.box.snippet

import java.util.Date
import net.liftweb.common.Logger
import net.liftweb.http.DispatchSnippet
import net.liftweb.util.Helpers._
import org.openpandora.box.model.Database
import org.openpandora.box.model.User
import org.openpandora.box.util.DotDesktopCategories
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.ast._
import scala.xml.NodeSeq
import scala.xml.Text

class ApplicationStatistics extends DispatchSnippet
                               with Logger {
  def dispatch = {
    case "uploadHistory" => uploadHistory
  }

  def uploadHistory(downloadHistory: NodeSeq): NodeSeq = {
    val uploads: Seq[Long] =
      from(Database.packages){pkg => select(pkg.uploadTime) orderBy(pkg.uploadTime asc)}.toSeq

    val uploadsCurrent: Long =
      from(Database.packages){pkg =>compute(count)}

    val uploadsCurrentEntry = ((new Date).getTime.toLong, uploadsCurrent.toInt - 1)

    Text((uploads.zipWithIndex :+ uploadsCurrentEntry) map ({
        case (time, uploads) =>
        "[" + (time + 1).toString + "," + uploads.toString + "]"
      }) mkString("[", ",", "]"))
  }
}
