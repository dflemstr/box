package org.openpandora.box.snippet

import java.util.Date
import net.liftweb.common.Logger
import net.liftweb.http.DispatchSnippet
import net.liftweb.http.S
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
    case "upgradableApplications" => upgradable
    case "recommendedApplications" => recommended
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
    val userId = User.currentUserId getOrElse 0l
    val lang = Some(S.locale).filter(_.toString != "en_US").map(_.toString)
    val apps =
      from(Database.applications, Database.applications,
           Database.packages,
           Database.appMetas, Database.appMetas)((oldApp, newApp, pkg, metaEng, metaLoc) =>
        where(
          (
            oldApp.id in (
              from(Database.applications, Database.packageDownloads){(app, download) =>
                where(app.packageId === download.packageId and download.userId === userId) select(app.id)
              }
            )
          ) and (
            (
              (newApp.versionMajor gt oldApp.versionMajor)
            ) or (
              (newApp.versionMajor   === oldApp.versionMajor) and
              (newApp.versionMinor   gt  oldApp.versionMinor)
            ) or (
              (newApp.versionMajor   === oldApp.versionMajor) and
              (newApp.versionMinor   === oldApp.versionMinor) and
              (newApp.versionRelease gt  oldApp.versionRelease)
            ) or (
              (newApp.versionMajor   === oldApp.versionMajor) and
              (newApp.versionMinor   === oldApp.versionMinor) and
              (newApp.versionRelease === oldApp.versionRelease) and
              (newApp.versionBuild   gt  oldApp.versionBuild)
            )
          ) and
          newApp.pxmlId === oldApp.pxmlId and
          newApp.packageId === pkg.id and
          metaEng.applicationId === newApp.id and
          metaEng.languageName === "en_US").
        select(newApp,
               pkg,
               metaEng,
               leftOuterJoin(metaLoc,
                             metaLoc.applicationId === newApp.id and
                             metaLoc.languageName === lang.orNull)))
    def makeEntry(entry: NodeSeq): NodeSeq = apps.toSeq flatMap (x => Applications.makeAppEntry(x._1, x._2, x._3, x._4, "entry", entry))

    bind("upgradableApplications", upgradable,
         "entry" -> makeEntry _)
  }

  def recommended(recommended: NodeSeq): NodeSeq = {
    NodeSeq.Empty
  }
}
