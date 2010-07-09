package org.openpandora.box.snippet

import java.text.DateFormat
import java.util.Date
import java.util.Locale
import java.util.TimeZone
import net.liftweb.common.Logger
import net.liftweb.http.DispatchSnippet
import net.liftweb.http.FileParamHolder
import net.liftweb.http.RequestVar
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.http.js.JE
import net.liftweb.http.js.JsCmds
import net.liftweb.util.AltXML
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import org.openpandora.box.model.Application
import org.openpandora.box.model.AppMeta
import org.openpandora.box.model.Comment
import org.openpandora.box.model.Database
import org.openpandora.box.model.Package
import org.openpandora.box.model.Rating
import org.openpandora.box.model.SearchKeyword
import org.openpandora.box.model.User
import org.openpandora.box.util.ApplicationSearchRunner
import org.openpandora.box.util.ApplicationQueryParser
import org.openpandora.box.util.DotDesktopCategories
import org.openpandora.box.util.packages.PackageManager
import org.openpandora.box.util.filesystem.Filesystem
import org.openpandora.box.util.filesystem.PNDFile
import org.squeryl.PrimitiveTypeMode._
import scala.math._
import scala.xml.Group
import scala.xml.NodeSeq
import scala.xml.Text
import scala.xml.UnprefixedAttribute

object Applications {
  private object createCommentFunction extends RequestVar[Option[() => NodeSeq]](None)
  
  def makeDelete(user: Option[User], pkg: Package)(delete: NodeSeq) = user match {
    case Some(user) if user.id == pkg.userId || user.admin =>
      def doDelete() = {
        PackageManager.default.removePackage(pkg)
        JsCmds.RedirectTo(S.hostAndPath + "/applications/list")
      }
      SHtml.a(doDelete _, delete)
    case _ => NodeSeq.Empty
  }

  def makePerson(user: User, binding: String)(template: NodeSeq) = {
    val name = user.username
    bind(binding, template,
         "name" -> <a href={"/applications/list?search=uploader:" + name}>{name}</a>,
         "largeavatar" -> user.gravatarImage(70),
         "avatar"      -> user.gravatarImage(30),
         "smallavatar" -> user.gravatarImage(12))
  }

  def localizedDateFormat(locale: Locale, timeZone: TimeZone) = {
    val df = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT, locale)
    df.setTimeZone(timeZone)
    df
  }

  def makeComment(app: Application, df: DateFormat)(template: NodeSeq): NodeSeq =
    app.commentsWithUsersSeq.flatMap({
        case (comment, user) =>
          bind("comment", template,
               "author" -> makePerson(user, "author") _,
               "date"   -> df.format(comment.time),
               "body"   -> comment.body)
      }).toSeq

  def makeComments(app: Application, df: DateFormat)(template: NodeSeq): NodeSeq = if(app.commentsSeq.isEmpty)
    NodeSeq.Empty
  else
    bind("comments", template,
         "comment" -> makeComment(app, df) _)

  def makeCommentForm(user: Option[User], app: Application)(commentForm: NodeSeq): NodeSeq =
    createCommentFunction.is.map(_()) getOrElse (user match {
        case Some(user) =>
          var text = ""
          def doCreateComment(): Unit = text match {
            case "" =>
              S.error("body-field", <p>{S.?("comment.empty")}</p>)
            case something if something.length > 1024 =>
              S.error("body-field", <p>{S.?("comment.huge").replace("%length%", "1024")}</p>)
            case something =>
              Database.comments.insert(Comment(user, app, new Date, something))
              S.notice(<p>{S.?("comment.created")}</p>)
          }

          def createFunc() = bind("commentForm", commentForm,
                                  "body" -> SHtml.textarea(text, x => text = x, "id" -> "body-field", "cols" -> "64", "rows" -> "8"),
                                  "submit" -> SHtml.submit(S.?("comment.create"), doCreateComment))
          createFunc()
        case None => NodeSeq.Empty
      })

  def makeCategory(app: Application)(category: NodeSeq): NodeSeq = app.categoriesSeq.flatMap {cat =>
    val name = DotDesktopCategories(cat.value).toString
    bind("category", category,
         "name" -> <a href={"/applications/list?search=category:" + name}>{name}</a>)
  } toSeq

  def makeCategories(app: Application)(categories: NodeSeq): NodeSeq =
    bind("categories", categories,
         "category" -> makeCategory(app) _)

  def makeLanguage(langs: Seq[Locale], locale: Locale)(language: NodeSeq): NodeSeq =
    langs flatMap { lang =>
      bind("language", language, "name" -> lang.getDisplayName(locale))
    } toSeq

  def makeLanguages(app: Application, locale: Locale)(languages: NodeSeq): NodeSeq ={
    val langs = app.metasSeq.map(_.language)
    if(langs.size <= 1)
      NodeSeq.Empty
    else
      bind("languages", languages,
           "language" -> makeLanguage(langs, locale) _)
  }

  def makeRating(app: Application, mayEdit: Boolean = false)(rating: NodeSeq): NodeSeq = {
    val id = Helpers.nextFuncName
    def redraw() = JsCmds.SetHtml(id, render)

    def doAddRating(rating: Int)() = User.currentUser match {
      case Some(user) if !app.hasRated(user) =>
        try {
          require(rating > 0 && rating < 11)
          Database.ratings.insert(Rating(app, user, rating))
          val (avrg, cnt) = from(Database.ratings)(rating => where(rating.applicationId === app.id) compute(nvl(avg(rating.value), 0f), count)).single.measures
          update(Database.applications)(a => where(a.id === app.id) set(a.ratingCount := cnt, a.ratingAverage := avrg))
          app.ratingCount = cnt
          app.ratingAverage = avrg
          S.notice(<p>{S.?("rating.created")}</p>)
        } catch {
          case _ =>
            S.error(<p>{S.?("rating.invalid")}</p>)
        }
        redraw()
      case Some(user) =>
        S.error(<p>{S.?("rating.alreadyrated")}</p>)
        JsCmds.Noop
      case None =>
        S.error(<p>{S.?("rating.mustlogin")}</p>)
        JsCmds.Noop
    }

    def makeDisplay(average: Int, enabled: Boolean)(display: NodeSeq): NodeSeq =
      for {
        i <- 1 to 10
        kind = if(i <= average) "enabled" else "disabled"
        evenOrOddTemplate = Helpers.chooseTemplate("display", kind, rating)
        template = Helpers.chooseTemplate(kind, if(i % 2 == 1) "even" else "odd", evenOrOddTemplate) //Reverse even/odd, because firstEntry==1
        node <- if(enabled) SHtml.a(doAddRating(i) _, template) else template
      } yield node

    def render =
      bind("rating", rating,
           "display" -> makeDisplay(app.ratingAverage.toInt, mayEdit) _,
           "count" -> Text(app.ratingCount.toString))

    <div id={id}>{render}</div>
  }

  def makeFileSize(size: Long) =  {
    val stepThreshold = 0.9
    size match {
      case b if size < stepThreshold * 1024 => S.?("bytes").replace("%n%", b.toString)
      case kb if size < stepThreshold * 1024*1024 => S.?("kilobytes").replace("%n%", (round(10.0 * kb / 1024.0) / 10.0).toString)
      case mb if size < stepThreshold * 1024*1024*1024 => S.?("megabytes").replace("%n%", (round(mb * 10.0 / (1024.0 * 1024.0)) / 10.0).toString)
      case gb => S.?("gigabytes").replace("%n%", (round(gb * 10.0 / (1024.0 * 1024.0 * 1024.0)) / 10.0).toString)
    }
  }

  def makeGravatar(email: String) = Helpers.hexEncode(Helpers.md5(email.toLowerCase.getBytes("UTF-8")))

  def makeGravatarImage(email: String, gravatar: String, size: Int) = //We can expose the email here, since it's in the PXML anyways
    (<img src={"http://www.gravatar.com/avatar/" + gravatar + ".png?s=" + size + "&d=identicon"} alt={email} title={email} class="avatar" width={size.toString} height={size.toString}/>)

  def makeAuthor(app: Application)(author: NodeSeq) = (app.authorName, app.authorURL, app.authorEmail) match {
    case (Some(name), Some(url), Some(email)) =>
      val gravatar = makeGravatar(email)
      bind("author", author,
           "name"        -> <a href={url}>{name}</a>,
           "largeavatar" -> makeGravatarImage(email, gravatar, 70),
           "avatar"      -> makeGravatarImage(email, gravatar, 30),
           "smallavatar" -> makeGravatarImage(email, gravatar, 12))
    case (Some(name), Some(url), _) =>
      bind("author", author,
           "name"        -> <a href={url}>{name}</a>,
           "largeavatar" -> NodeSeq.Empty,
           "avatar"      -> NodeSeq.Empty,
           "smallavatar" -> NodeSeq.Empty)
    case (Some(name), _, _) =>
      bind("author", author,
           "name"        -> name,
           "largeavatar" -> NodeSeq.Empty,
           "avatar"      -> NodeSeq.Empty,
           "smallavatar" -> NodeSeq.Empty)
    case _ => NodeSeq.Empty
  }
 
  def makeImage16(pkg: Package) = if(pkg.hasImage)
    (<img src={"/file/image/" + pkg.fileId + "-16.png"} alt={pkg.fileName}/>)
  else
    NodeSeq.Empty

  def makeImage(pkg: Package) = if(pkg.hasImage)
    (<img src={"/file/image/" + pkg.fileId + ".png"} alt={pkg.fileName}/>)
  else
    NodeSeq.Empty

  def makeApplicationLink(app: Application)(n: NodeSeq) =
    (<a href={"/applications/" + app.id + "/show"}>{n}</a>)

  def makeDownloadLink(pkg: Package)(n: NodeSeq) =
    (<a href={"/file/package/" + pkg.fileId + ".pnd"}>{n}</a>)

  def makeVersionLink(app: Application)(n: NodeSeq) =
    (<a href={"/applications/list?search=version:" + app.version}>{n}</a>)

  def makeDownloadCount(pkg: Package)(n: NodeSeq) =
    Text(from(Database.packageDownloads)(dl => where(dl.packageId === pkg.id) compute(count)).single.measures.toString)

  def makeAppEntry(app: Application, bindName: String)(entry: NodeSeq) = {
    val pkg = app.pkg.single
    val user = pkg.user.single
    val meta = app.metas.where(_.languageName === "en_US").head
    val locale = S.locale
    val localeName = locale.toString.toLowerCase
    val metaLoc = app.metas.where(_.languageName like localeName).headOption

    val df = localizedDateFormat(S.locale, S.timeZone)

    bind(bindName, entry,
         "image" -> makeImage(pkg),
         "smallimage" -> makeImage16(pkg),
         "titleAndLink" -> makeApplicationLink(app)(Text(meta.title)),
         "title" -> meta.title,
         "pxmlid" -> app.pxmlId,
         "description" -> meta.description,
         "version" -> makeVersionLink(app)(Text(app.version)),
         "uploader" -> makePerson(user, "uploader") _,
         "time" -> df.format(pkg.uploadTime),
         "author" -> makeAuthor(app) _,
         "categories" -> makeCategories(app) _,
         "languages" -> makeLanguages(app, S.locale) _,
         "rating" -> makeRating(app, User.currentUser.map(app.hasRated) getOrElse false) _,
         "comments" -> makeComments(app, df) _,
         "commentForm" -> makeCommentForm(User.currentUser, app) _,
         "link" -> makeApplicationLink(app) _,
         "download" -> makeDownloadLink(pkg) _,
         "delete" -> makeDelete(User.currentUser, pkg) _,
         "fileSize" -> makeFileSize(Filesystem.default.getFile(pkg.fileId, PNDFile).length),
         "downloadCount" -> makeDownloadCount(pkg) _)
  }

  def makeLightweightAppEntry(app: Application, pkg: Package, user: User, meta: AppMeta, bindName: String)(entry: NodeSeq) = {
    val df = localizedDateFormat(S.locale, S.timeZone)
    bind(bindName, entry,
         "image" -> makeImage(pkg),
         "smallimage" -> makeImage16(pkg),
         "title" -> meta.title,
         "titleAndLink" -> makeApplicationLink(app)(Text(meta.title)),
         "description" -> meta.description,
         "time" -> df.format(pkg.uploadTime),
         "pxmlid" -> app.pxmlId,
         "version" -> makeVersionLink(app)(Text(app.version)),
         "rating" -> makeRating(app, User.currentUser.map(app.hasRated) getOrElse false) _,
         "author" -> makeAuthor(app) _,
         "link" -> makeApplicationLink(app) _,
         "uploader" -> makePerson(user, "uploader") _)
  }
}

class Applications extends DispatchSnippet with Logger {
  import Applications._
  def dispatch = {
    case "create" => create
    case "list" => list
    case "search" => search
    case "searchField" => searchField
    case "entry" => entry
  }

  private object upload extends RequestVar[Option[FileParamHolder]](None)

  def create(create: NodeSeq): NodeSeq = {
    import PackageManager._
    def doCreate() = {
      def success() =
        S.notice(<p>{S.?("package.uploaded")}</p> ++
                 <p>{S.?("package.processing")}</p>)

      upload.is match {
        case Some(h) =>
          PackageManager.default.makePackageFromStream(h.fileName, h.fileStream, User.currentUser getOrElse {
              S.error(<p>{S.?("package.abortlogout")}</p>)
              S.redirectTo(S.referer openOr "/")
            })
          success()
        case None =>
          S.error(<p>{S.?("package.void")}</p>)
      }
    }

    val u = SHtml.fileUpload(x => upload.set(Some(x)))
    val meta = new UnprefixedAttribute("id", "upload-field", u.attributes)

    Helpers.bind("create", create,
                 "upload" -> u % meta,
                 "submit" -> SHtml.submit(S.?("Create"), doCreate))
  }
  
  def fixHtml(uid: String, content: NodeSeq): String =
    AltXML.toXML(Group(S.session.map(s => s.fixHtml(s.processSurroundAndInclude("JS SetHTML id: " + uid, content))).openOr(content)),
                 false, true, S.ieMode).encJs

  def list(list: NodeSeq): NodeSeq = {
    import ApplicationSearchRunner._
    val search = (S.attr("search") or S.param("search") toOption) map (_.trim) getOrElse ""
    val lazyload = (S.attr("lazyload") or S.param("lazyload") toOption) map (_.toInt)

    if(S.param("search").isDefined)
      Database.searchKeywords.insert(search.split("""\s+""").filterNot(_ contains ':').map(keyw => SearchKeyword(keyw.take(256).toLowerCase)))

    def loadAppsOnPage(page: Option[(Int, Int)]) = ((search, page) match {
        case ("", None) =>
          default.runSearch(UseLocale(S.locale))
        case ("", Some((start, max))) =>
          default.runSearch(UseExplicitPagination(start, max),
                            UseLocale(S.locale))
        case (query, None) =>
          default.runSearch(RunQuery(query)(ApplicationQueryParser.default),
                            UseLocale(S.locale))
        case (query, Some((start, max))) =>
          default.runSearch(RunQuery(query)(ApplicationQueryParser.default),
                            UseExplicitPagination(start, max),
                            UseLocale(S.locale))
      }).toSeq
    def makeEntry(applications: Seq[(Application, Package, User, AppMeta)])(entry: NodeSeq): NodeSeq =
      applications flatMap (x => makeLightweightAppEntry(x._1, x._2, x._3, x._4, "entry")(entry))

    lazyload match {
      case None =>
        bind("list", list,
             "entry" -> makeEntry(loadAppsOnPage(None)) _,
             "loading" -> NodeSeq.Empty)
      case Some(pageSize) =>
        def makeId = Helpers.nextFuncName
        def loadPage(number: Int, id: String): NodeSeq = {
          val apps = loadAppsOnPage(Some((number * pageSize, pageSize)))

          def makeLoading(loading: NodeSeq) = if(apps.size < pageSize)
            NodeSeq.Empty
          else {
            val newId = makeId
            S.mapFunc(id,
                      () => {
                val newContent = fixHtml("inline", loadPage(number + 1, newId))
                (
                  (JE.JsRaw("jQuery('#" + id + "').replaceWith(" + newContent + ")")).cmd &
                  JE.Call("bindLoadEvent", JE.Str(newId), JE.AnonFunc(SHtml.makeAjaxCall(JE.Str(newId + "=true")).cmd)).cmd
                )
              })
            ((<div id={id}>{loading}</div>) ++ (if(number == 0) JsCmds.Script(JsCmds.OnLoad(JE.Call("bindLoadEvent", JE.Str(id), JE.AnonFunc(SHtml.makeAjaxCall(JE.Str(id + "=true")).cmd)).cmd)) else NodeSeq.Empty))
          }

          bind("list", list,
               "entry" -> makeEntry(apps) _,
               "loading" -> makeLoading _)
        }
        loadPage(0, makeId)
    }
  }

  def entry(entry: NodeSeq): NodeSeq = {
    val lang = Some(S.locale).filter(_.toString != "en_US").map(_.toString)
    val app = S.param("id") flatMap (x => tryo(x.toLong)) flatMap {x =>
      Database.applications.lookup(x)
    } getOrElse {
      S.error(<p>Invalid application id</p>)
      S.redirectTo(S.referer openOr "/")
    }
    makeAppEntry(app, "entry")(entry)
  }

  def search(search: NodeSeq) = S.param("search") map Text openOr NodeSeq.Empty

  def searchField(searchField: NodeSeq): NodeSeq =
    (<input type="text" name="search" id="search-field" value={S.param("search") openOr ""}/>)
}
