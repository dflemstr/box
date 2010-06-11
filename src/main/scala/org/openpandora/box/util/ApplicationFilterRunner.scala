package org.openpandora.box.util

import net.liftweb.common.Logger
import net.liftweb.http.S
import org.squeryl.PrimitiveTypeMode._
import org.openpandora.box.model._
import org.squeryl.Query
import org.squeryl.Queryable
import org.squeryl.dsl.ast.LogicalBoolean
import org.squeryl.dsl.ast.TypedExpressionNode
import org.squeryl.dsl.OneToMany

object ApplicationFilterRunner {
  val default: ApplicationFilterRunner = new ApplicationFilterRunnerImpl
}

trait ApplicationFilterRunner {
  def runFilter(filter: String)(implicit p: ApplicationFilterParser): Query[(Application, Package)]
}

private[util] class ApplicationFilterRunnerImpl extends ApplicationFilterRunner
                                                   with Logger {

  case class VersionRestriction(major: Option[Int], minor: Option[Int], release: Option[Int], build: Option[Int])

  def runFilter(filter: String)(implicit p: ApplicationFilterParser): Query[(Application, Package)] = {
    import p._
    import ApplicationFilterParser._
    def all = from(Database.applications, Database.packages)((app, pkg) =>
      where(app.packageId === pkg.id) select(app, pkg) orderBy(pkg.uploadTime))
    
    val query = if(filter.isEmpty)
      all
    else parseAll(exprs, filter) match {
      case Success(expressions, _) =>

        //Split the filter expressions into separate sequences of strings
        val titleRestrictionsBuilder       = Seq.newBuilder[String]
        val descriptionRestrictionsBuilder = Seq.newBuilder[String]
        val keywordRestrictionsBuilder     = Seq.newBuilder[String]
        val uploaderRestrictionsBuilder    = Seq.newBuilder[String]
        val authorRestrictionsBuilder      = Seq.newBuilder[String]
        val categoryRestrictionsBuilder    = Seq.newBuilder[String]
        var ordering: OrderingExpression   = OrderByTitle(false)
        var versionRestriction             = VersionRestriction(None, None, None, None)
        var max = 0
        expressions foreach {
          case FilterTitle(title)       => titleRestrictionsBuilder       += title
          case FilterDescription(descr) => descriptionRestrictionsBuilder += descr
          case FilterKeyword(keyword)   => keywordRestrictionsBuilder     += keyword
          case FilterAuthor(author)     => authorRestrictionsBuilder      += author
          case FilterUploader(uploader) => uploaderRestrictionsBuilder    += uploader
          case FilterCategory(category) => categoryRestrictionsBuilder    += category
          case o: OrderingExpression    => ordering = o
          case MaxResults(n)            => max = n
          case FilterVersion(major, minor, release, build) =>
            if(versionRestriction.major.isDefined)
              S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "major")}</p>)
            if(versionRestriction.minor.isDefined)
              S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "minor")}</p>)
            if(versionRestriction.release.isDefined)
              S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "release")}</p>)
            if(versionRestriction.build.isDefined)
              S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "build")}</p>)
            versionRestriction = VersionRestriction(Some(major), Some(minor), Some(release), Some(build))
          case FilterVersionMajor(major)     =>
            if(versionRestriction.major.isDefined)
              S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "major")}</p>)
            versionRestriction = versionRestriction.copy(major = Some(major))
          case FilterVersionMinor(minor)     =>
            if(versionRestriction.minor.isDefined)
              S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "minor")}</p>)
            versionRestriction = versionRestriction.copy(minor = Some(minor))
          case FilterVersionRelease(release) =>
            if(versionRestriction.release.isDefined)
              S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "release")}</p>)
            versionRestriction = versionRestriction.copy(release = Some(release))
          case FilterVersionBuild(build)     =>
            if(versionRestriction.build.isDefined)
              S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "build")}</p>)
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

        val inhibitAppMetas   = titleRestrictions.isEmpty && descriptionRestrictions.isEmpty && !ordering.isInstanceOf[OrderByTitle]
        val inhibitRatings    = !ordering.isInstanceOf[OrderByRating]
        val inhibitUsers      = uploaderRestrictions.isEmpty
        val inhibitCategories = categoryAlternatives.isEmpty

        //This is the most epic query ever constructed
        val query = from(Database.applications,
                         Database.appMetas  .inhibitWhen(inhibitAppMetas),
                         Database.packages,
                         Database.ratings   .inhibitWhen(inhibitRatings),
                         Database.users     .inhibitWhen(inhibitUsers),
                         Database.categories.inhibitWhen(inhibitCategories)) {
          (app, meta, pkg, rating, user, category) =>
          where (
            {
              (titleRestrictions map likeness(meta.get.title)) ++
              (descriptionRestrictions map likeness(meta.get.description)) ++
              keywordRestrictions.map {restr =>
                likeness(meta.get.title)(restr) or
                likeness(meta.get.description)(restr)
              } ++
              (uploaderRestrictions map likeness(user.get.username)) ++
              (authorRestrictions map maybeLikeness(app.authorName)) ++
              (versionRestriction.major map (app.versionMajor === _)) ++
              (versionRestriction.minor map (app.versionMinor === _)) ++
              (versionRestriction.release map (app.versionRelease === _)) ++
              (versionRestriction.build map (app.versionBuild === _)) ++
              maybeOmit(inhibitAppMetas, app.id === meta.get.applicationId) ++
              maybeOmit(inhibitRatings, app.id === rating.get.applicationId) ++
              maybeOmit(inhibitCategories, app.id === category.get.applicationId) ++
              maybeOmit(inhibitUsers, user.get.id === pkg.userId) ++
              Seq(app.packageId === pkg.id, category.get.value in categoryAlternatives)
            }.reduceLeft[LogicalBoolean](_ and _)
          ).select(app, pkg).orderBy {
            val o: TypedExpressionNode[_] = (ordering match {
                case _: OrderByTitle => meta.get.title
                case _: OrderByRating => avg(rating.get.value)
                case _: OrderByTime => pkg.uploadTime
              })
            if(ordering.ascending)
              o.asc
            else
              o.desc
          }
        }.distinct

        if(max == 0) query else query.page(0, max)
      case ns @ NoSuccess(error, _) =>
        S.warning(<p>{S.?("filters.invalid")}</p> ++ <code>{error}</code>)
        all
    }
    query
  }
}
