package org.openpandora.box.model

import org.squeryl.dsl.ManyToOne
import org.squeryl.dsl.OneToMany
import org.squeryl.annotations._
import org.squeryl.PrimitiveTypeMode._
import scala.annotation.target.field


case class Application(packageId:       Long, //id
                       @(Column @field)(length = 256)
                       pxmlId:          String,
                       versionMajor:    Int,
                       versionMinor:    Int,
                       versionRelease:  Int,
                       versionBuild:    Int,
                       @(Column @field)(length = 64)
                       authorName:      Option[String],
                       var ratingCount:     Long,
                       var ratingAverage:   Float,
                       val newest:       Boolean) extends LongKeyedEntity {
  lazy val `package`:  ManyToOne[Package]  = Database.packagesToApplications.right(this)

  lazy val categories: OneToMany[Category] = Database.applicationsToCategories.left(this)
  lazy val comments:   OneToMany[Comment]  = Database.applicationsToComments.left(this)
  lazy val metas:      OneToMany[AppMeta]  = Database.applicationsToAppMetas.left(this)
  lazy val ratings:    OneToMany[Rating]   = Database.applicationsToRatings.left(this)

  final def pkg = `package` //alias

  def this() = this(0l, "", 0, 0, 0, 0, Some(""), 0, 0f, false)
}

object Application {
  def apply(`package`: Package, pxmlId: String, versionMajor: Int, versionMinor: Int, versionRelease: Int, versionBuild: Int, authorName: Option[String]): Application = {
    val countQuery = from(Database.applications)(app => where(
        app.pxmlId === pxmlId and (
          (
            (app.versionMajor   gt  versionMajor)
          ) or (
            (app.versionMajor   === versionMajor) and
            (app.versionMinor   gt  versionMinor)
          ) or (
            (app.versionMajor   === versionMajor) and
            (app.versionMinor   === versionMinor) and
            (app.versionRelease gt  versionRelease)
          ) or (
            (app.versionMajor   === versionMajor) and
            (app.versionMinor   === versionMinor) and
            (app.versionRelease === versionRelease) and
            (app.versionBuild   gt  versionBuild)
          )
        )) compute(count))
    val isNewest = {
      val count = (countQuery: Long)
      info("count: " + count)
      count == 0l
    }

    if(isNewest) update(Database.applications)(app => where(app.pxmlId === pxmlId) set(app.newest := false))

    Application(`package`.id, pxmlId: String, versionMajor, versionMinor, versionRelease, versionBuild, authorName, 0, 0f, isNewest)
  }
}