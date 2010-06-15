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
}

trait ApplicationSearchRunner {
  def runSearch(search: String, locale: Locale)(implicit p: ApplicationSearchParser): Query[(Application, Package)]
}

private[util] class ApplicationSearchRunnerImpl extends ApplicationSearchRunner
                                                   with Logger {

  case class VersionRestriction(major: Option[Int], minor: Option[Int], release: Option[Int], build: Option[Int])

  def runSearch(search: String, locale: Locale)(implicit p: ApplicationSearchParser): Query[(Application, Package)] = {
    import p._
    import ApplicationSearchParser._

    def all = from(Database.applications, Database.packages)((app, pkg) =>
      where(app.packageId === pkg.id) select(app, pkg) orderBy(pkg.uploadTime))
    
    val query = if(search.isEmpty)
      runExpressions(Seq.empty, locale)
    else
      parseAll(exprs, search) match {
        case Success(expressions, _) =>
          runExpressions(expressions, locale)
        case ns @ NoSuccess(error, _) =>
          S.warning(<p>{S.?("search.invalid")}</p> ++ <code>{error}</code>)
          runExpressions(Seq.empty, locale)
      }
    query
  }

  def runExpressions(expressions: Seq[ApplicationSearchParser.Expression], locale: Locale) = {
    import ApplicationSearchParser._
    val lang = locale.toString

    //Split the search expressions into separate sequences of strings
    val titleRestrictionsBuilder       = Seq.newBuilder[String]
    val descriptionRestrictionsBuilder = Seq.newBuilder[String]
    val keywordRestrictionsBuilder     = Seq.newBuilder[String]
    val uploaderRestrictionsBuilder    = Seq.newBuilder[String]
    val authorRestrictionsBuilder      = Seq.newBuilder[String]
    val categoryRestrictionsBuilder    = Seq.newBuilder[String]

    var ordering: OrderingExpression   = OrderByTitle(true)
    var versionRestriction             = VersionRestriction(None, None, None, None)
    var max = 0

    @inline def warnVersion(field: String) = S.warning(<p>{S.?("search.version.conflict").replace("%versionfield%", field)}</p>)
    expressions foreach {
      case SearchTitle(title)       => titleRestrictionsBuilder       += title
      case SearchDescription(descr) => descriptionRestrictionsBuilder += descr
      case SearchKeyword(keyword)   => keywordRestrictionsBuilder     += keyword
      case SearchAuthor(author)     => authorRestrictionsBuilder      += author
      case SearchUploader(uploader) => uploaderRestrictionsBuilder    += uploader
      case SearchCategory(category) => categoryRestrictionsBuilder    += category
      case o: OrderingExpression    => ordering = o
      case MaxResults(n)            => max = n
      case SearchVersion(major, minor, release, build) =>
        if(versionRestriction.major.isDefined)
          warnVersion("major")
        if(versionRestriction.minor.isDefined)
          warnVersion("minor")
        if(versionRestriction.release.isDefined)
          warnVersion("release")
        if(versionRestriction.build.isDefined)
          warnVersion("build")
        versionRestriction = VersionRestriction(Some(major), Some(minor), Some(release), Some(build))
      case SearchVersionMajor(major)     =>
        if(versionRestriction.major.isDefined)
          warnVersion("major")
        versionRestriction = versionRestriction.copy(major = Some(major))
      case SearchVersionMinor(minor)     =>
        if(versionRestriction.minor.isDefined)
          warnVersion("minor")
        versionRestriction = versionRestriction.copy(minor = Some(minor))
      case SearchVersionRelease(release) =>
        if(versionRestriction.release.isDefined)
          warnVersion("release")
        versionRestriction = versionRestriction.copy(release = Some(release))
      case SearchVersionBuild(build)     =>
        if(versionRestriction.build.isDefined)
          warnVersion("build")
        versionRestriction = versionRestriction.copy(build = Some(build))
    }

    val titleRestrictions       = titleRestrictionsBuilder.result()
    val descriptionRestrictions = descriptionRestrictionsBuilder.result()
    val keywordRestrictions     = keywordRestrictionsBuilder.result()
    val uploaderRestrictions    = uploaderRestrictionsBuilder.result()
    val authorRestrictions      = authorRestrictionsBuilder.result()
    val categoryRestrictions    = categoryRestrictionsBuilder.result()

    val categoryAlternatives    = if(categoryRestrictions.isEmpty)
      Nil
    else
      DotDesktopCategories.values.filter {x =>
        categoryRestrictions.forall(x.toString.toLowerCase contains _.toLowerCase)
      }.map(_.id).toList

    @inline def likeness(what: => String)(to: String) = what like "%" + to + "%"
    @inline def maybeLikeness(what: => Option[String])(to: String) = what like "%" + to + "%"
    @inline def maybeOmit[A](condition: Boolean, value: A) = if(condition) Seq() else Seq(value)

    val inhibitUsers      = uploaderRestrictions.isEmpty
    val inhibitCategories = categoryAlternatives.isEmpty
    val inhibitAppMetas   = titleRestrictions.isEmpty && descriptionRestrictions.isEmpty &&
    keywordRestrictions.isEmpty && !ordering.isInstanceOf[OrderByTitle]

    //This is the most epic query ever constructed
    val query = from(Database.applications,
                     Database.appMetas  .inhibitWhen(inhibitAppMetas),
                     Database.packages,
                     Database.users     .inhibitWhen(inhibitUsers),
                     Database.categories.inhibitWhen(inhibitCategories)) {
      (app, metaEng, pkg, user, category) =>
      where (
        {
          (titleRestrictions map {likeness(metaEng.get.title)}) ++
          (descriptionRestrictions map {likeness(metaEng.get.description)}) ++
          keywordRestrictions.map {restr =>
            likeness(metaEng.get.title)(restr) or
            likeness(metaEng.get.description)(restr)
          } ++
          (uploaderRestrictions map likeness(user.get.username)) ++
          (authorRestrictions map maybeLikeness(app.authorName)) ++
          (versionRestriction.major map (app.versionMajor === _)) ++
          (versionRestriction.minor map (app.versionMinor === _)) ++
          (versionRestriction.release map (app.versionRelease === _)) ++
          (versionRestriction.build map (app.versionBuild === _)) ++
          maybeOmit(inhibitCategories, app.id === category.get.applicationId) ++
          maybeOmit(inhibitUsers, user.get.id === pkg.userId) ++
          Seq(app.packageId === pkg.id,
              category.get.value in categoryAlternatives,
              metaEng.get.applicationId === app.id,
              metaEng.get.languageName === "en_US")
        }.reduceLeft[LogicalBoolean](_ and _)
      ).select(app, pkg).orderBy {
        val o: TypedExpressionNode[_] = (ordering match {
            case _: OrderByTitle => metaEng.get.title
            case _: OrderByRating => app.ratingAverage
            case _: OrderByTime => pkg.uploadTime
          })
        if(ordering.ascending)
          o.asc
        else
          o.desc
      }
    }.distinct

    if(max == 0) query else query.page(0, max)
  }
}
