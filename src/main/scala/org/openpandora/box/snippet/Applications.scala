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
import net.liftweb.http.js.JsCmds
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import org.openpandora.box.model._
import org.openpandora.box.util.ApplicationFilterRunner
import org.openpandora.box.util.ApplicationFilterParser
import org.openpandora.box.util.DotDesktopCategories
import org.openpandora.box.util.Languages
import org.openpandora.box.util.packages.PackageManager
import org.openpandora.box.util.filesystem._
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.OneToMany
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

  private def makeAppEntry(app: Application, pkg: Package,
                           bindName: String, entry: NodeSeq) = {
    val locale = S.locale.toString.toLowerCase
    lazy val infos = app.metas
    lazy val categories = app.categories
    lazy val comments = app.comments
    lazy val concreteInfos = infos.toSeq
    lazy val info = concreteInfos.find(_.languageName.toLowerCase == locale) orElse
    concreteInfos.find(_.languageName.toLowerCase == locale.split("_")(0)) getOrElse
    concreteInfos.find(_.languageName.toLowerCase == "en_us").get
    val applicationLink = (n: NodeSeq) => (<a href={"/applications/" + app.id + "/show"}>{n}</a>)
    val downloadLink = (n: NodeSeq) => (<a href={"/file/package/" + pkg.fileId + ".pnd"}>{n}</a>)
    val image = if(pkg.hasImage)
      (<img src={"/file/image/" + pkg.fileId + ".png"} alt={pkg.fileName}/>)
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

    def makeComments(template: NodeSeq): NodeSeq = if(comments.isEmpty)
      NodeSeq.Empty
    else
      bind("comments", template,
           "comment" -> makeComment _)

    def makeCommentForm(commentForm: NodeSeq): NodeSeq = createCommentFunction.is.map(_()) getOrElse (User.currentUser match {
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

    def makeCategory(category: NodeSeq): NodeSeq = categories.flatMap {cat =>
      val name = DotDesktopCategories(cat.value).toString
      bind("category", category,
           "name" -> <a href={"/applications/list?filter=category:" + name}>{name}</a>)
    } toSeq

    def makeCategories(categories: NodeSeq): NodeSeq =
      bind("categories", categories,
           "category" -> makeCategory _)

    def makeLanguage(langs: Seq[Locale])(language: NodeSeq): NodeSeq =
      langs flatMap { lang =>
        bind("language", language, "name" -> lang.getDisplayName(S.locale))
      } toSeq

    def makeLanguages(languages: NodeSeq): NodeSeq ={
      val langs = concreteInfos.map(_.language)
      if(langs.size <= 1)
        NodeSeq.Empty
      else
        bind("languages", languages,
             "language" -> makeLanguage(langs) _)
    }

    def makeRating(rating: NodeSeq): NodeSeq = {
      val id = Helpers.nextFuncName
      def redraw() = JsCmds.SetHtml(id, render)

      def hasRated(user: User) =
        from(Database.ratings)(rating => where(rating.userId === user.id and rating.applicationId === app.id) compute(count)) > 0

      def doAddRating(rating: Int)() = User.currentUser match {
        case Some(user) if !hasRated(user) =>
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
             "display" -> makeDisplay(app.ratingAverage.toInt, User.currentUser.map(u => !hasRated(u)) getOrElse false) _,
             "count" -> Text(app.ratingCount.toString))

      <div id={id}>{render}</div>
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
           "name" -> <a href={"/applications/list?filter=uploader:" + name}>{name}</a>,
           "largeavatar" -> gravatarImage(70),
           "avatar"      -> gravatarImage(30),
           "smallavatar" -> gravatarImage(16))
    }

    val stepThreshold = 0.9
    def makeFileSize(size: Long) = size match {
      case b if size < stepThreshold * 1024 => S.?("bytes").replace("%n%", b.toString)
      case kb if size < stepThreshold * 1024*1024 => S.?("kilobytes").replace("%n%", (round(10.0 * kb / 1024.0) / 10.0).toString)
      case mb if size < stepThreshold * 1024*1024*1024 => S.?("megabytes").replace("%n%", (round(mb * 10.0 / (1024.0 * 1024.0)) / 10.0).toString)
      case gb => S.?("gigabytes").replace("%n%", (round(gb * 10.0 / (1024.0 * 1024.0 * 1024.0)) / 10.0).toString)
    }

    def makeLazyNode(node: => NodeSeq) = (_: NodeSeq) => node
    def makeLazyString(text: => String) = (_: NodeSeq) => Text(text)

    bind(bindName, entry,
         "image" -> makeLazyNode(image),
         "title" -> makeLazyNode(applicationLink(Text(info.title))),
         "description" -> makeLazyString(info.description),
         "version" -> (makeLazyNode {
          val ver = Seq(app.versionMajor, app.versionMinor, app.versionRelease, app.versionBuild).mkString(".")
          (<a href={"/applications/list?filter=version:" + ver}>{ver}</a>)
        }),
         "uploader" -> makePerson(pkg.user.single.id, "uploader") _,
         "time" -> makeLazyString(df.format(pkg.uploadTime)),
         "author" -> makeAuthor _,
         "categories" -> makeCategories _,
         "languages" -> makeLanguages _,
         "rating" -> makeRating _,
         "comments" -> makeComments _,
         "commentForm" -> makeCommentForm _,
         "link" -> applicationLink,
         "download" -> downloadLink,
         "fileSize" -> makeFileSize(Filesystem.default.getFile(pkg.fileId, PNDFile).length),
         "downloadCount" -> makeLazyString(from(Database.packageDownloads)(dl => where(dl.packageId === pkg.id) compute(count)).single.measures.toString))
  }

  def list(list: NodeSeq): NodeSeq = {
    val filter = (S.attr("filter") or S.param("filter") openOr "").trim
    val apps = ApplicationFilterRunner.default.runFilter(filter, S.locale)(ApplicationFilterParser.default)

    def makeEntry(entry: NodeSeq): NodeSeq = apps.toSeq flatMap (x => makeAppEntry(x._1, x._2, "entry", entry))

    bind("list", list,
         "entry" -> makeEntry _)
  }

  def entry(entry: NodeSeq): NodeSeq = {
    val app = S.param("id") flatMap (x => tryo(x.toLong)) flatMap {x =>
      from(Database.applications, Database.packages)((app, pkg) =>
        where(app.id === x and app.packageId === pkg.id) select(app, pkg)).toSeq.headOption
    } getOrElse {
      S.error(<p>Invalid application id</p>)
      S.redirectTo(S.referer openOr "/")
    }
    makeAppEntry(app._1, app._2, "entry", entry)
  }

  def filter(filter: NodeSeq) = S.param("filter") map Text openOr NodeSeq.Empty

  def filterField(filterField: NodeSeq): NodeSeq =
    (<input type="text" name="filter" id="filter-field" value={S.param("filter") openOr ""}/>)
}
