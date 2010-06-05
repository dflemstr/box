package org.openpandora.box.snippet

import java.text.DateFormat
import java.util.Date
import java.util.Locale
import net.liftweb.common.Full
import net.liftweb.common.Logger
import net.liftweb.http.DispatchSnippet
import net.liftweb.http.FileParamHolder
import net.liftweb.http.RequestVar
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.http.js.JE
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import org.openpandora.box.model._
import org.openpandora.box.util.ApplicationFilterRunner
import org.openpandora.box.util.DotDesktopCategories
import org.openpandora.box.util.packages.PackageManager
import org.openpandora.box.util.filesystem._
import org.squeryl.PrimitiveTypeMode._
import scala.math._
import scala.xml.NodeSeq
import scala.xml.Text
import scala.xml.UnprefixedAttribute

class Applications extends DispatchSnippet with Logger {
  def dispatch = {
    case "create" => create
    case "list" => list
    case "filter" => filter
    case "filterField" => filterField
    case "entry" => entry
  }

  private object upload extends RequestVar[Option[FileParamHolder]](None)
  private object createCommentFunction extends RequestVar[Option[() => NodeSeq]](None)

  private lazy val localizedAppMetas = {
    val locale = S.locale.toString
    val locales = locale :: (if(locale contains "_") List(locale.split("_").head) else Nil)
    Database.appMetas.where(metaEng => metaEng.languageName === "en_US") //TODO: wait for case support
  }

  def create(create: NodeSeq): NodeSeq = {
    import PackageManager._
    def doCreate() = {
      def success() =
        S.notice(<p>Your package was successfully uploaded!</p> ++
                 <p>The server will now process the uploaded file. We will notify you when it's done, stay tuned.</p>)

      upload.is match {
        case Some(h) =>
          PackageManager! MakePackageFromStream(h.fileName, h.fileStream, User.currentUser getOrElse {
              S.error(<p>User was logged out while the package was being uploaded.</p>)
              S.redirectTo(S.referer openOr "/")
            })
          success()
        case None =>
          S.error(<p>No file was uploaded!</p>)
      }
    }

    val u = SHtml.fileUpload(x => upload.set(Some(x)))
    val meta = new UnprefixedAttribute("id", "upload-field", u.attributes)

    Helpers.bind("create", create,
                 "upload" -> u % meta,
                 "submit" -> SHtml.submit("Create", doCreate))
  }

  private def makeAppEntry(app: Application, info: AppMeta, pkg: Package, bindName: String, entry: NodeSeq) = {
    lazy val categories = app.categories
    lazy val comments = app.comments
    val applicationLink = (n: NodeSeq) => (<a href={S.hostAndPath + "/applications/" + app.id + "/show"}>{n}</a>)
    val downloadLink = (n: NodeSeq) => (<a href={S.hostAndPath + "/file/package/" + pkg.fileId + ".pnd"}>{n}</a>)
    val image = if(pkg.hasImage)
      (<img src={S.hostAndPath + "/file/image/" + pkg.fileId + ".png"} alt={pkg.fileName}/>)
    else
      NodeSeq.Empty

    val df = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT, S.locale)
    df.setTimeZone(S.timeZone)

    def makeComment(comment: NodeSeq): NodeSeq = comments flatMap { c =>
      bind("comment", comment,
           "author" -> makePerson(c.userId, "author") _,
           "date"   -> df.format(c.time),
           "body"   -> c.body)
    } toSeq

    def makeComments(comments: NodeSeq): NodeSeq = if(comments.isEmpty)
      NodeSeq.Empty
    else
      bind("comments", comments,
           "comment" -> makeComment _)


    def makeCommentForm(commentForm: NodeSeq): NodeSeq = createCommentFunction.is.map(_()) getOrElse (User.currentUser match {
        case Some(user) =>
          var text = ""
          def doCreateComment(): Unit = text match {
            case "" =>
              S.error("body-field", <p>Comment field empty</p>)
            case something if something.length > 1024 =>
              S.error("body-field", <p>Comment too long (max accepted length: 1024 characters)</p>)
            case something =>
              Database.comments.insert(Comment(user, app, new Date, something))
              S.notice(<p>Comment created</p>)
          }

          def createFunc() = bind("commentForm", commentForm,
                                  "body" -> SHtml.textarea(text, x => text = x, "id" -> "body-field", "cols" -> "64", "rows" -> "8"),
                                  "submit" -> SHtml.submit("Create comment", doCreateComment))
          createFunc()
        case None => NodeSeq.Empty
      })

    def makeCategory(category: NodeSeq): NodeSeq = categories.flatMap {cat =>
      val name = DotDesktopCategories(cat.value).toString
      bind("category", category,
           "name" -> <a href={S.hostAndPath + "/applications/list?filter=category:" + name}>{name}</a>)
    } toSeq

    def makeCategories(categories: NodeSeq): NodeSeq =
      bind("categories", categories,
           "category" -> makeCategory _)

    def makeLanguage(language: NodeSeq): NodeSeq = {
      val langs = from(Database.appMetas)(meta => where(meta.applicationId === app.id) select(meta.language))
      if(langs.isEmpty || (langs.size == 1 && langs.head == Locale.ENGLISH))
        NodeSeq.Empty
      else
        langs flatMap { lang =>
          bind("language", language, "name" -> lang.getDisplayLanguage(S.locale))
        } toSeq
    }

    def makeLanguages(languages: NodeSeq): NodeSeq =
      bind("languages", languages,
           "language" -> makeLanguage _)

    def makeRating(rating: NodeSeq): NodeSeq = {
      def hasRated(user: User) =
        from(Database.ratings)(rating => where(rating.userId === user.id and rating.applicationId === app.id) compute(count)) > 0

      def doAddRating(r: String) = User.currentUser match {
        case Some(user) if !hasRated(user) =>
          try {
            val ratingVal = r.toInt
            require(ratingVal > 0 && ratingVal < 11)
            Database.ratings.insert(Rating(app, user, ratingVal))
            S.notice(<p>Rating submitted.</p>)
          } catch {
            case _ =>
              S.error(<p>Invalid rating.</p>)
          }
        case Some(user) =>
          S.error(<p>You have already rated this application.</p>)
        case None =>
          S.error(<p>Must be logged in to rate.</p>)
      }

      val strRange = (1 to 10) map (_.toString)
      def current: (Int, Int) = 
        from(Database.ratings)(rating => where(rating.applicationId === app.id) compute(avg(rating.value), count)).single.measures match {
          case (None, count) => (5, count.toInt)
          case (Some(avg), count) => (avg.toInt, count.toInt)
        }
      val id = Helpers.nextFuncName
      <div id={id}>{SHtml.ajaxSelect(strRange zip strRange, Full(current._1.toString), x => {
              doAddRating(x)
              val (rating, count) = current
              JE.Call("ratingAdded", JE.Str(id), JE.Num(rating), JE.Num(count)).cmd
            }, "class" -> "rating" :: (if(User.currentUser.isEmpty || hasRated(User.currentUser.get)) List("disabled" -> "disabled") else Nil): _*)}</div>
    }
    
    def makeAuthor(author: NodeSeq) = app.authorName match {
      case None => NodeSeq.Empty
      case Some(aname) =>
        bind("author", author,
             "name"-> aname)
    }
    
    def makePerson(who: Long, binding: String)(template: NodeSeq) = {
      val name = User.nameFor(who)
      val user = Database.users.lookup(who).get

      lazy val gravatar = Helpers.hexEncode(Helpers.md5(user.email.toLowerCase.getBytes("UTF-8")))

      def gravatarImage(size: Int) =
        <img src={"http://www.gravatar.com/avatar/" + gravatar + "?s=" + size + "&d=identicon"} alt={name} class="avatar"/>

      bind(binding, template,
           "name" -> <a href={S.hostAndPath + "/applications/list?filter=uploader:" + name}>{name}</a>,
           "largeavatar" -> gravatarImage(70),
           "avatar"      -> gravatarImage(30),
           "smallavatar" -> gravatarImage(16))
    }

    val stepThreshold = 0.9
    def makeFileSize(size: Long) = size match {
      case b if size < stepThreshold * 1024 => b + " B"
      case kb if size < stepThreshold * 1024*1024 => round(10.0 * kb / 1024.0) / 10.0 + " kB"
      case mb if size < stepThreshold * 1024*1024*1024 => round(mb * 10.0 / (1024.0 * 1024.0)) / 10.0 + " MB"
      case gb => round(gb * 10.0 / (1024.0 * 1024.0 * 1024.0)) / 10.0 + " GB"
    }

    def makeLazyNode(node: => NodeSeq) = (_: NodeSeq) => node
    def makeLazyString(text: => String) = (_: NodeSeq) => Text(text)

    bind(bindName, entry,
         "image" -> makeLazyNode(image),
         "title" -> makeLazyNode(applicationLink(Text(info.title))),
         "description" -> makeLazyString(info.description),
         "version" -> (makeLazyNode {
          val ver = Seq(app.versionMajor, app.versionMinor, app.versionRelease, app.versionBuild).mkString(".")
          (<a href={S.hostAndPath + "/applications/list?filter=version:" + ver}>{ver}</a>)
        }),
         "uploader" -> makePerson(pkg.user.single.id, "uploader") _,
         "time" -> makeLazyString(df.format(pkg.uploadTime)),
         "author" -> makeAuthor _,
         "categories" -> makeCategories _,
         "languages" -> makeLanguages _,
         "rating" -> makeRating _,
         "ratingCount" -> makeLazyString(from(Database.ratings)(rating => where(rating.applicationId === app.id) compute(count)).single.measures.toString),
         "comments" -> makeComments _,
         "commentForm" -> makeCommentForm _,
         "link" -> applicationLink,
         "download" -> downloadLink,
         "fileSize" -> makeFileSize(Filesystem.getFile(pkg.fileId, PNDFile).length),
         "downloadCount" -> makeLazyString(from(Database.packageDownloads)(dl => where(dl.packageId === pkg.id) compute(count)).single.measures.toString))
  }

  def list(list: NodeSeq): NodeSeq = {
    val filter = (S.attr("filter") or S.param("filter") openOr "").trim
    val apps = ApplicationFilterRunner.runFilter(localizedAppMetas, filter)

    def makeEntry(entry: NodeSeq): NodeSeq = apps.toSeq flatMap (x => makeAppEntry(x._1, x._2, x._3, "entry", entry))

    bind("list", list,
         "entry" -> makeEntry _)
  }

  def entry(entry: NodeSeq): NodeSeq = {
    val app = S.param("id") flatMap (x => tryo(x.toLong)) flatMap {x =>
      from(Database.applications, Database.appMetas, Database.packages)((app, meta, pkg) =>
        where(app.id === x and meta.applicationId === x and app.packageId === pkg.id) select(app, meta, pkg)).headOption
    } getOrElse {
      S.error(<p>Invalid application id</p>)
      S.redirectTo(S.referer openOr "/")
    }
    makeAppEntry(app._1, app._2, app._3, "entry", entry)
  }

  def filter(filter: NodeSeq) = S.param("filter") map Text openOr NodeSeq.Empty

  def filterField(filterField: NodeSeq): NodeSeq =
    (<input type="text" name="filter" id="filter-field" value={S.param("filter") openOr ""}/>)
}
