package org.openpandora.box.dispatch

import java.util.Date
import net.liftweb.http.{Req, GetRequest, HeadRequest, PostRequest, LiftRules, JsonResponse, NotFoundResponse, InMemoryResponse}
import net.liftweb.common.{Full, Box, Empty, Logger}
import net.liftweb.http.js.JE
import net.liftweb.http.js.JE.{JsObj, JsArray, strToS, JsNull}
import net.liftweb.http.js.JsExp
import net.liftweb.http.LiftResponse
import net.liftweb.http.S
import net.liftweb.util.Helpers
import org.openpandora.box.model._
import org.openpandora.box.util.DotDesktopCategories
import org.openpandora.box.util.Localization
import org.openpandora.box.util.packages.PackageManager
import org.squeryl.PrimitiveTypeMode._
import scala.actors.Actor

object RestRepositoryApi {
  val json: RestRepositoryApi = new RestRepositoryJsonApi
  val default = json
}

trait RestRepositoryApi extends Dispatcher

private[dispatch] class RestRepositoryJsonApi(localization: Localization = Localization.default) extends RestRepositoryApi
                                                                                                    with Logger {
  import localization._

  RepositoryUpdater.start()
  RepositoryUpdater! RepositoryUpdater.RefreshRepository
  
  var data: Box[String => JsExp] = Empty
  val responseCache: collection.mutable.Map[String, LiftResponse] = collection.mutable.Map.empty

  def newData(data: String => JsExp) = {
    this.data = Full(data)
    responseCache.clear()
  }

  def headers = {
    import Helpers._
    List("Last-Modified" -> toInternetDate(new Date),
         "Expires" -> toInternetDate(1.day.later.date),
         "Cache-Control" -> "public",
         "Pragma" -> "")
  }

  def respond =
    Full(
      data.dmap[LiftResponse](
        NotFoundResponse("The repository file is still being generated...")
      )(jsFunc => responseCache.getOrElseUpdate(S.hostAndPath,
                                                JsonResponse(jsFunc(S.hostAndPath),
                                                             headers,
                                                             Nil, 200)
        )
      )
    )

  def dispatch: LiftRules.DispatchPF = {
    case Req("repository" :: Nil, "json", GetRequest) => respond _
    case Req("repository" :: Nil, "json", HeadRequest) =>
      () => respond map (_.toResponse) map (resp => InMemoryResponse(Array(), resp.headers, resp.cookies, 204))
  }

  object RepositoryUpdater extends Actor {
    case object RefreshRepository
    
    val update = (pkg: Package) => {
      this! RefreshRepository
    }

    PackageManager.default.registerPackageAddedCallback(update)
    
    def act = Actor.loop {
      Actor.react {
        case RefreshRepository =>
          newData(repomd)
      }
    }

    def repomd = {
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

    def makeAppEntries: (String => JsArray) = inTransaction {
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
}
