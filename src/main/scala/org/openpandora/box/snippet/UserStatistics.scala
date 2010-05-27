package org.openpandora.box.snippet

import java.util.Date
import net.liftweb.common.Logger
import net.liftweb.http.DispatchSnippet
import net.liftweb.util.Helpers._
import org.openpandora.box.model.Database
import org.openpandora.box.model.User
import org.openpandora.box.util.DotDesktopCategories
import org.squeryl.PrimitiveTypeMode._
import scala.xml.NodeSeq
import scala.xml.Text

class UserStatistics extends DispatchSnippet
                        with Logger {
  def dispatch = {
    case "downloadHistory" => downloadHistory
    case "categoryDownloads" => categoryDownloads
  }

  def downloadHistory(downloadHistory: NodeSeq): NodeSeq = {
    val userId = User.currentUserId getOrElse 0l
    val thirtyDaysAgo = new Date();
    thirtyDaysAgo.setTime(thirtyDaysAgo.getTime - 2678400000l)
    val downloadsLastMonth =
      from(Database.packageDownloads){download =>
        where(download.userId === userId and download.date > thirtyDaysAgo) groupBy(download.date) compute(count) orderBy(download.date asc)
      }
    Text(downloadsLastMonth.map { download =>
        "[" + download.key.getTime.toString + "," + download.measures.toString + "]"
      }.mkString("[", ",", "]"))
  }

  def categoryDownloads(categoryDownloads: NodeSeq): NodeSeq = {
    val userId = User.currentUserId getOrElse 0l
    val downloadsPerCategory =
      from(Database.applications, Database.packageDownloads, Database.categories) {(app, downloads, category) =>
        where(downloads.userId === userId and
              app.packageId === downloads.packageId and
              category.applicationId === app.id) groupBy(category.value) compute(count)
      }

    Text(downloadsPerCategory.map { download =>
        "'" + DotDesktopCategories(download.key) + "':" + download.measures + ""
      }.mkString("{", ",", "}"))
  }
}
