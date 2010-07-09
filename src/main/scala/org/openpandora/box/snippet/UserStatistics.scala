package org.openpandora.box.snippet

import java.util.Date
import net.liftweb.common.Logger
import net.liftweb.http.DispatchSnippet
import net.liftweb.http.S
import net.liftweb.util.Helpers._
import org.openpandora.box.model._
import org.openpandora.box.util.DotDesktopCategories
import org.squeryl.PrimitiveTypeMode._
import scala.xml.NodeSeq
import scala.xml.Text

class UserStatistics extends DispatchSnippet
                        with Logger {
  def dispatch = {
    case "downloadHistory" => downloadHistory
    case "categoryDownloads" => categoryDownloads
    case "upgradableApplications" => upgradable
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
  
  def upgradable(upgradable: NodeSeq): NodeSeq = {
    val user = User.currentUser.get
    val lang = Some(S.locale).filter(_.toString != "en_US").map(_.toString)
    val apps =
      from(Database.applications, Database.applications,
           Database.packages,
           Database.appMetas)((oldApp, newApp, pkg, meta) =>
        where(
          oldApp.id in (
            from(Database.applications, Database.packageDownloads){(app, download) =>
              where(app.packageId === download.packageId and download.userId === user.id) select(app.id)
            }
          ) and
          newApp.newest === true and
          oldApp.newest === false and
          newApp.pxmlId === oldApp.pxmlId and
          newApp.packageId === pkg.id and
          meta.applicationId === newApp.id and
          meta.languageName === "en_US").select(newApp, pkg, meta)).distinct

    def makeEntry(entry: NodeSeq): NodeSeq = apps.toSeq flatMap (x => Applications.makeLightweightAppEntry(x._1, x._2, user, x._3, "entry")(entry))

    bind("upgradableApplications", upgradable,
         "entry" -> makeEntry _)
  }
}
