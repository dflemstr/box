package org.openpandora.box.rest

import net.liftweb.http.{Req, GetRequest, PostRequest, LiftRules, JsonResponse, PlainTextResponse}
import net.liftweb.common.{Full, Box}
import net.liftweb.http.js.JE
import net.liftweb.http.js.JE.{JsObj, JsArray, strToS, boolToJsExp}
import net.liftweb.http.js.JsExp
import net.liftweb.http.S
import org.openpandora.box.model._
import org.openpandora.box.util.DotDesktopCategories
import org.openpandora.box.util.Localization._
import org.squeryl.PrimitiveTypeMode._

object RestRepositoryJsonApi {
  def dispatch: LiftRules.DispatchPF = {
    case Req("repository" :: Nil, "json", GetRequest) =>
      () => Full(repomd)
  }
  private def repomd = JsonResponse(
    JsObj(
      "repository" -> JsObj(
        "name" -> loc("application"),
        "version" -> JE.numToJsExp(1.0)
      ),
      "applications" -> makeAppEntries
    )
  )

  private case class LocalizationEntry(lang: String, title: String, description: String)
  private case class VersionEntry(major: Int, minor: Int, release: Int, build: Int)
  private case class ApplicationEntry(id: String, version: VersionEntry, author: Option[String], vendor: Option[String], uri: String,
                                      localizations: Seq[LocalizationEntry], categories: Seq[String], image: Option[String])

  def makeAppEntries: JsArray = {
    val data =
      from(Database.applications, Database.users, Database.packages) { (app, user, pkg) =>
        where(app.packageId === pkg.id and user.id === pkg.userId) select(app, user, pkg)
      }

    val apps = data.map {
      case (app, user, pkg) =>
        ApplicationEntry(app.pxmlId, VersionEntry(app.versionMajor, app.versionMinor, app.versionRelease, app.versionBuild),
                         app.authorName, Some(user.username), S.hostAndPath + "/files/packages/" + pkg.fileId + ".pnd",
                         app.metas.toSeq.map(y => LocalizationEntry(y.languageName, y.title, y.description)),
                         app.categories.toSeq.map(y => DotDesktopCategories(y.value).toString),
                         if(pkg.hasImage) Some(S.hostAndPath + "/files/images/" + pkg.fileId + ".png") else None)
    }.toSeq

    JsArray(
      (for(app <- apps) yield JsObj(
          "id" -> app.id,
          "version" -> JsObj(
            "major"   -> JE.numToJsExp(app.version.major),
            "minor"   -> JE.numToJsExp(app.version.minor),
            "release" -> JE.numToJsExp(app.version.release),
            "build"   -> JE.numToJsExp(app.version.build)
          ),
          "author" -> app.author.orNull[String],
          "vendor" -> app.vendor.orNull[String],
          "uri" -> app.uri,
          "localizations" -> JsObj(
            (for(loc <- app.localizations) yield loc.lang -> JsObj(
                "title" -> loc.title,
                "description" -> loc.description
              )): _*
          ),
          "categories" -> JsArray(app.categories.map(x => x: JsExp): _*),
          "image" -> app.image.orNull[String]
        )): _*
    )
  }
}
