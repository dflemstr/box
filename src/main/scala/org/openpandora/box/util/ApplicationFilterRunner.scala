package org.openpandora.box.util

import java.util.Locale
import net.liftweb.common.Logger
import net.liftweb.http.S
import org.squeryl.PrimitiveTypeMode._
import org.openpandora.box.model._
import org.squeryl.Query
import org.squeryl.Queryable
import org.squeryl.dsl.ast.LogicalBoolean
import org.squeryl.dsl.ast.TypedExpressionNode
import org.squeryl.dsl.OneToMany

object ApplicationSearchRunner {
  val default: ApplicationSearchRunner = new ApplicationSearchRunnerImpl
  sealed trait SearchParam
  case class RunQuery(query: String)(implicit val parser: ApplicationQueryParser) extends SearchParam
  case class UseLocale(locale: Locale) extends SearchParam
  case class UseExplicitPagination(start: Int, count: Int) extends SearchParam
}

trait ApplicationSearchRunner {
  def runSearch(params: ApplicationSearchRunner.SearchParam*): Query[(Application, Package, AppMeta, Option[AppMeta])]
}

private[util] class ApplicationSearchRunnerImpl extends ApplicationSearchRunner
                                                   with Logger {
  import ApplicationSearchRunner._

  case class VersionRestriction(major: Option[Int] = None, minor: Option[Int] = None,
                                release: Option[Int] = None, build: Option[Int] = None)

  object OrderingColumn extends Enumeration {
    val Title, Time, Rating, PxmlId, Author = Value
  }

  object OrderingDirection extends Enumeration {
    val Ascending, Descending = Value
  }

  case class Search(titleRestr:       List[String]            = Nil,
                    descriptionRestr: List[String]            = Nil,
                    keywordRestr:     List[String]            = Nil,
                    uploaderRestr:    List[String]            = Nil,
                    authorRestr:      List[String]            = Nil,
                    categoryRestr:    List[String]            = Nil,
                    versionRestr:     VersionRestriction      = VersionRestriction(),
                    orderCol:         OrderingColumn.Value    = OrderingColumn.Title,
                    orderDir:         OrderingDirection.Value = OrderingDirection.Ascending,
                    start:            Int                     = 0,
                    count:            Option[Int]             = None,
                    locale:           Option[Locale]          = None,
                    showOldVersions:  Boolean                 = false)

  def runQuery(query: String, parser: ApplicationQueryParser, search: Search) = {
    import ApplicationQueryParser._
    val expressions = parser.parseAll(parser.exprs, query) match {
      case parser.Success(expressions, _) =>
        expressions
      case parser.NoSuccess(err, _) =>
        S.warning(<p>{S.?("search.invalid")}</p> ++ <code>{err}</code>)
    Seq.empty
    }
    @inline def warnVersion(field: String) = S.warning(<p>{S.?("search.version.conflict").replace("%versionfield%", field)}</p>)
    expressions.foldLeft(search) { (previous, expr) =>
      expr match {
        case SearchTitle(title)       => previous.copy(titleRestr       = title    :: previous.titleRestr)
        case SearchDescription(descr) => previous.copy(descriptionRestr = descr    :: previous.descriptionRestr)
        case SearchKeyword(keyword)   => previous.copy(keywordRestr     = keyword  :: previous.keywordRestr)
        case SearchAuthor(author)     => previous.copy(authorRestr      = author   :: previous.authorRestr)
        case SearchUploader(uploader) => previous.copy(uploaderRestr    = uploader :: previous.uploaderRestr)
        case SearchCategory(category) => previous.copy(categoryRestr    = category :: previous.categoryRestr)
        case OrderByTitle(true)       => previous.copy(orderCol = OrderingColumn.Title,  orderDir = OrderingDirection.Ascending)
        case OrderByTitle(false)      => previous.copy(orderCol = OrderingColumn.Title,  orderDir = OrderingDirection.Descending)
        case OrderByTime(true)        => previous.copy(orderCol = OrderingColumn.Time,   orderDir = OrderingDirection.Ascending)
        case OrderByTime(false)       => previous.copy(orderCol = OrderingColumn.Time,   orderDir = OrderingDirection.Descending)
        case OrderByRating(true)      => previous.copy(orderCol = OrderingColumn.Rating, orderDir = OrderingDirection.Ascending)
        case OrderByRating(false)     => previous.copy(orderCol = OrderingColumn.Rating, orderDir = OrderingDirection.Descending)
        case OrderByPxmlId(true)      => previous.copy(orderCol = OrderingColumn.PxmlId, orderDir = OrderingDirection.Ascending)
        case OrderByPxmlId(false)     => previous.copy(orderCol = OrderingColumn.PxmlId, orderDir = OrderingDirection.Descending)
        case OrderByAuthor(true)      => previous.copy(orderCol = OrderingColumn.Author, orderDir = OrderingDirection.Ascending)
        case OrderByAuthor(false)     => previous.copy(orderCol = OrderingColumn.Author, orderDir = OrderingDirection.Descending)
        case MaxResults(n)            => previous.copy(count = Some(n))
        case ShowOlderVersions(value)   => previous.copy(showOldVersions = value)
        case SearchVersion(major, minor, release, build) =>
          if(previous.versionRestr.major.isDefined)
            warnVersion("major")
          if(previous.versionRestr.minor.isDefined)
            warnVersion("minor")
          if(previous.versionRestr.release.isDefined)
            warnVersion("release")
          if(previous.versionRestr.build.isDefined)
            warnVersion("build")
          previous.copy(versionRestr = VersionRestriction(Some(major), Some(minor), Some(release), Some(build)))
        case SearchVersionMajor(major)     =>
          if(previous.versionRestr.major.isDefined)
            warnVersion("major")
          previous.copy(versionRestr = previous.versionRestr.copy(major = Some(major)))
        case SearchVersionMinor(minor)     =>
          if(previous.versionRestr.minor.isDefined)
            warnVersion("minor")
          previous.copy(versionRestr = previous.versionRestr.copy(minor = Some(minor)))
        case SearchVersionRelease(release) =>
          if(previous.versionRestr.release.isDefined)
            warnVersion("release")
          previous.copy(versionRestr = previous.versionRestr.copy(release = Some(release)))
        case SearchVersionBuild(build)     =>
          if(previous.versionRestr.build.isDefined)
            warnVersion("build")
          previous.copy(versionRestr = previous.versionRestr.copy(build = Some(build)))
      }
    }
  }

  def runSearch(params: SearchParam*): Query[(Application, Package, AppMeta, Option[AppMeta])] = {

    //Generate a Search construct out of the params
    val search = params.foldLeft(Search()) {
      case (search, query @ RunQuery(string)) => runQuery(string, query.parser, search)
      case (search, UseLocale(loc)) => search.copy(locale = Some(loc))
      case (search, UseExplicitPagination(s, c)) => search.copy(start = s, count = Some(c))
    }

    //Then, actually create a database query out of it
    runAssembledSearch(search)
  }

  def runAssembledSearch(search: Search) = {
    val lang = search.locale.filter(_.toString != "en_US").map(_.toString)

    val categoryAlternatives = if(search.categoryRestr.isEmpty)
      Nil
    else
      DotDesktopCategories.values.filter {x =>
        search.categoryRestr.forall(x.toString.toLowerCase contains _.toLowerCase)
      }.map(_.id).toList

    @inline def likeness(what: => String)(to: String) = what like "%" + to + "%"
    @inline def maybeLikeness(what: => Option[String])(to: String) = what like "%" + to + "%"
    @inline def maybeOmit[A](condition: Boolean, value: A) = if(condition) Seq() else Seq(value)

    val inhibitUsers      = search.uploaderRestr.isEmpty
    val inhibitCategories = categoryAlternatives.isEmpty

    //This is the most epic query ever constructed
    val query = from(Database.applications,
                     Database.appMetas,
                     Database.appMetas,
                     Database.packages,
                     Database.users     .inhibitWhen(inhibitUsers),
                     Database.categories.inhibitWhen(inhibitCategories)) {
      (app, metaEng, metaLoc, pkg, user, category) =>
      where (
        {
          (search.titleRestr map {restr =>
              likeness(metaEng.title)(restr) or
              likeness(metaLoc.title)(restr)
            }) ++
          (search.descriptionRestr map {restr =>
              likeness(metaEng.description)(restr) or
              likeness(metaLoc.title)(restr)
            }) ++
          (search.keywordRestr map {restr =>
              likeness(metaEng.title)(restr) or
              likeness(metaLoc.title)(restr) or
              likeness(metaEng.description)(restr) or
              likeness(metaLoc.description)(restr)
            }) ++
          (search.uploaderRestr map likeness(user.get.username)) ++
          (search.authorRestr map maybeLikeness(app.authorName)) ++
          (search.versionRestr.major map (app.versionMajor === _)) ++
          (search.versionRestr.minor map (app.versionMinor === _)) ++
          (search.versionRestr.release map (app.versionRelease === _)) ++
          (search.versionRestr.build map (app.versionBuild === _)) ++
          maybeOmit(inhibitCategories, app.id === category.get.applicationId) ++
          maybeOmit(inhibitUsers, user.get.id === pkg.userId) ++
          (maybeOmit(search.showOldVersions, app.newest === true)) ++
          Seq(app.packageId === pkg.id,
              category.get.value in categoryAlternatives,
              metaEng.applicationId === app.id,
              metaEng.languageName === "en_US")
        }.reduceLeft[LogicalBoolean](_ and _)
      ).select(app,
               pkg,
               metaEng,
               leftOuterJoin(metaLoc,
                             metaLoc.applicationId === app.id and
                             metaLoc.languageName === lang.orNull)).orderBy {
        val o: TypedExpressionNode[_] = (search.orderCol match {
            case OrderingColumn.Title  => metaEng.title
            case OrderingColumn.Rating => app.ratingAverage
            case OrderingColumn.Time   => pkg.uploadTime
            case OrderingColumn.PxmlId => app.pxmlId
            case OrderingColumn.Author => app.authorName
          })
        search.orderDir match {
          case OrderingDirection.Ascending  => o.asc
          case OrderingDirection.Descending => o.desc
        }
      }
    }.distinct

    search.count match {
      case Some(count) => query.page(search.start, count)
      case None => query
    }
  }
}
