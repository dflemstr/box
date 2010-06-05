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

object ApplicationFilterRunner extends Logger {

  private case class VersionRestriction(major: Option[Int], minor: Option[Int], release: Option[Int], build: Option[Int])

  def runFilter(filter: String): Query[(Application, Package,
                                        OneToMany[org.openpandora.box.model.AppMeta],
                                        OneToMany[org.openpandora.box.model.Category],
                                        OneToMany[org.openpandora.box.model.Comment])] = {
    import ApplicationFilterParser._
    def all = from(Database.applications, Database.packages)((app, pkg) =>
      where(app.packageId === pkg.id) select(app, pkg, app.metas, app.categories, app.comments) orderBy(pkg.uploadTime))
    
    val query = if(filter.isEmpty)
      all
    else parseAll(exprs, filter) match {
      case Success(expressions, _) =>

        //Split the filter expressions into separate sequences of strings
        val titleRestrictions       = expressions collect {case FilterTitle(title)       => title}
        val descriptionRestrictions = expressions collect {case FilterDescription(descr) => descr}
        val keywordRestrictions     = expressions collect {case FilterKeyword(keyword)   => keyword}
        val uploaderRestrictions    = expressions collect {case FilterUploader(uploader) => uploader}
        val authorRestrictions      = expressions collect {case FilterAuthor(uploader)   => uploader}
        val categoryRestrictions    = expressions collect {case FilterCategory(category) => category}

        //Determine ordering: use the first expression found, or else sort by title
        val ordering                = expressions.collect {case expr: OrderingExpression => expr}.headOption.getOrElse(OrderByTitle(false))

        //Accumulate all version filter expressions
        val versionRestriction      = expressions.foldLeft(VersionRestriction(None, None, None, None)) {(prev, expr) =>
          expr match {
            case FilterVersion(major, minor, release, build) =>
              if(prev.major.isDefined)
                S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "major")}</p>)
              if(prev.minor.isDefined)
                S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "minor")}</p>)
              if(prev.release.isDefined)
                S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "release")}</p>)
              if(prev.build.isDefined)
                S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "build")}</p>)
              VersionRestriction(Some(major), Some(minor), Some(release), Some(build))
            case FilterVersionMajor(major)     =>
              if(prev.major.isDefined)
                S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "major")}</p>)
              prev.copy(major = Some(major))
            case FilterVersionMinor(minor)     =>
              if(prev.minor.isDefined)
                S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "minor")}</p>)
              prev.copy(minor = Some(minor))
            case FilterVersionRelease(release) =>
              if(prev.release.isDefined)
                S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "release")}</p>)
              prev.copy(release = Some(release))
            case FilterVersionBuild(build)     =>
              if(prev.build.isDefined)
                S.warning(<p>{S.?("filters.version.conflict").replace("%versionfield%", "build")}</p>)
              prev.copy(build = Some(build))
            case _ => prev
          }
        }

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
        from(Database.applications,
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
          ).select(app, pkg, app.metas, app.categories, app.comments).orderBy {
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
      case ns @ NoSuccess(error, _) =>
        S.warning(<p>{S.?("filters.invalid")}</p> ++ <code>{error}</code>)
        all
    }
    query
  }
}
