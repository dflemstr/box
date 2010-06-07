package org.openpandora.box.rest

import net.liftweb.http.{Req, GetRequest, PostRequest, LiftRules, JsonResponse, NotFoundResponse}
import net.liftweb.common.{Full, Box, Empty, Logger}
import net.liftweb.http.js.JE
import net.liftweb.http.js.JE.{JsObj, JsArray, strToS, JsNull}
import net.liftweb.http.js.JsExp
import net.liftweb.http.LiftResponse
import net.liftweb.http.S
import org.openpandora.box.model._
import org.openpandora.box.util.DotDesktopCategories
import org.openpandora.box.util.Localization._
import org.squeryl.PrimitiveTypeMode._
import scala.actors.Actor

object RestRepositoryJsonApi extends Logger {
  private[rest] var data: Box[String => JsExp] = Empty
  private[rest] val hostnameCache: collection.mutable.Map[String, JsExp] = collection.mutable.Map.empty

  def dispatch: LiftRules.DispatchPF = {
    case Req("repository" :: Nil, "json", GetRequest) =>
      () => Full(data.dmap[LiftResponse](NotFoundResponse("The repository file is still being generated..."))
                 (jsFunc => JsonResponse(hostnameCache.getOrElseUpdate(S.hostAndPath, jsFunc(S.hostAndPath)))))
  }
}

object RepositoryUpdater extends Actor {
  case object RefreshRepository

  private object Pinger extends Actor {
    def act = while(true) {
      RepositoryUpdater.this! RefreshRepository
      Thread.sleep(60000)
    }
  }

  override def start() = {
    Pinger.start()
    super.start()
  }

  def act = Actor.loop {
    Actor.react {
      case RefreshRepository =>
        RestRepositoryJsonApi.data = Full(repomd)
        RestRepositoryJsonApi.hostnameCache.clear()
    }
  }

  private def repomd = {
    val repository = JsObj(
      "name" -> loc("application"),
      "version" -> JE.numToJsExp(1.0)
    )
    val applications = makeAppEntries
    (host: String) => JsObj(
      "repository" -> repository,
      "applications" -> applications(host)
    )
  }

  private def makeAppEntries: (String => JsArray) = inTransaction {
    val appFuncs =
      from(Database.applications, Database.users, Database.packages) { (app, user, pkg) =>
        where(app.packageId === pkg.id and user.id === pkg.userId) select(app, user, pkg)
      }.toSeq.map {
        case (app, user, pkg) =>
          val id      = JE.strToS(app.pxmlId)
          val version = JsObj(
            "major"   -> JE.numToJsExp(app.versionMajor),
            "minor"   -> JE.numToJsExp(app.versionMinor),
            "release" -> JE.numToJsExp(app.versionRelease),
            "build"   -> JE.numToJsExp(app.versionBuild)
          )
          val author  = app.authorName.map(JE.strToS) getOrElse JsNull
          val vendor  = JE.strToS(user.username)
          val uri = (host: String) => (host + "/file/package/" + pkg.fileId + ".pnd")
          val localizations = JsObj(
            (for(loc <- app.metas.toSeq) yield loc.languageName -> JsObj(
                "title" -> loc.title,
                "description" -> loc.description
              )): _*
          )
          val categories = JsArray(app.categories.toSeq.map(y => DotDesktopCategories(y.value).toString: JsExp): _*)
          val image = (host: String) => (if(pkg.hasImage) JE.strToS(host + "/file/image/" + pkg.fileId + ".png") else JsNull)

          (host: String) => JsObj(
            "id" -> id,
            "version" -> version,
            "author" -> author,
            "vendor" -> vendor,
            "uri" -> uri(host),
            "localizations" -> localizations,
            "categories" -> categories,
            "icon" -> image(host)
          )
      }

    (host: String) => JsArray(appFuncs.map(_(host)): _*)
  }
}
