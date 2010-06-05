package org.openpandora.box.util

import net.liftweb.http.S
import org.squeryl.PrimitiveTypeMode._
import org.openpandora.box.model._
import org.squeryl.Query
import org.squeryl.Queryable
import org.squeryl.dsl.ast.LogicalBoolean
import org.squeryl.dsl.ast.TypedExpressionNode

object ApplicationFilterRunner {

  private case class VersionRestriction(major: Option[Int], minor: Option[Int], release: Option[Int], build: Option[Int])

  def runFilter(appMetaSource: Queryable[AppMeta], filter: String): Query[(Application, AppMeta, Package)] = {
    import ApplicationFilterParser._
    def all =
      from(Database.applications, appMetaSource, Database.packages)((app, meta, pkg) =>
        where(app.packageId === pkg.id and meta.applicationId === app.id) select(app, meta, pkg) orderBy(pkg.uploadTime))
    if(filter.isEmpty)
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
                S.warning(<p>Multiple "major" version restrictions specified</p>)
              if(prev.minor.isDefined)
                S.warning(<p>Multiple "minor" version restrictions specified</p>)
              if(prev.release.isDefined)
                S.warning(<p>Multiple "release" version restrictions specified</p>)
              if(prev.build.isDefined)
                S.warning(<p>Multiple "build" version restrictions specified</p>)
              VersionRestriction(Some(major), Some(minor), Some(release), Some(build))
            case FilterVersionMajor(major)     =>
              if(prev.major.isDefined)
                S.warning(<p>Multiple "major" version restrictions specified</p>)
              prev.copy(major = Some(major))
            case FilterVersionMinor(minor)     =>
              if(prev.minor.isDefined)
                S.warning(<p>Multiple "minor" version restrictions specified</p>)
              prev.copy(minor = Some(minor))
            case FilterVersionRelease(release) =>
              if(prev.release.isDefined)
                S.warning(<p>Multiple "release" version restrictions specified</p>)
              prev.copy(release = Some(release))
            case FilterVersionBuild(build)     =>
              if(prev.build.isDefined)
                S.warning(<p>Multiple "build" version restrictions specified</p>)
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

        //This is the most epic query ever constructed
        from(Database.applications,
             appMetaSource,
             Database.packages,
             Database.ratings,
             Database.users,
             Database.categories.inhibitWhen(categoryAlternatives.isEmpty)) {
          (app, meta, pkg, rating, user, category) =>
          where (
            {
              Seq(app.id === meta.applicationId,
                  app.id === rating.applicationId,
                  app.id === category.get.applicationId,
                  app.packageId === pkg.id,
                  user.id === pkg.userId,
                  category.get.value in categoryAlternatives) ++
              (titleRestrictions map likeness(meta.title)) ++
              (descriptionRestrictions map likeness(meta.description)) ++
              keywordRestrictions.map {restr =>
                likeness(meta.title)(restr) or
                likeness(meta.description)(restr)
              } ++
              (uploaderRestrictions map likeness(user.username)) ++
              (authorRestrictions map maybeLikeness(app.authorName)) ++
              (versionRestriction.major map (app.versionMajor === _)) ++
              (versionRestriction.minor map (app.versionMinor === _)) ++
              (versionRestriction.release map (app.versionRelease === _)) ++
              (versionRestriction.build map (app.versionBuild === _)) ++ Seq.empty
            }.reduceLeft[LogicalBoolean](_ and _)
          ).select(app, meta, pkg).orderBy {
            val o: TypedExpressionNode[_] = (ordering match {
                case _: OrderByTitle => meta.title
                case _: OrderByRating => avg(rating.value)
                case _: OrderByTime => pkg.uploadTime
              })
            if(ordering.ascending)
              o.asc
            else
              o.desc
          }
        }.distinct
      case ns @ NoSuccess(error, _) =>
        S.warning(<p>Invalid filter:</p> ++ <code>{error}</code>)
        all
    }
  }
}
