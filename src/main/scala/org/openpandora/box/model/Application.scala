package org.openpandora.box.model

import org.squeryl.dsl.ManyToOne
import org.squeryl.dsl.OneToMany
import org.squeryl.annotations._


case class Application(packageId:       Long, //id
                       @Column(length = 256)
                       pxmlId:          String,
                       versionMajor:    Int,
                       versionMinor:    Int,
                       versionRelease:  Int,
                       versionBuild:    Int,
                       @Column(length = 64)
                       authorName:      Option[String]) extends LongKeyedEntity {
  lazy val `package`:  ManyToOne[Package]  = Database.packagesToApplications.right(this)

  lazy val categories: OneToMany[Category] = Database.applicationsToCategories.left(this)
  lazy val comments:   OneToMany[Comment]  = Database.applicationsToComments.left(this)
  lazy val metas:      OneToMany[AppMeta]  = Database.applicationsToAppMetas.left(this)
  lazy val ratings:    OneToMany[Rating]   = Database.applicationsToRatings.left(this)

  final def pkg = `package` //alias

  def this() = this(0l, "", 0, 0, 0, 0, Some(""))
}

object Application {
  def apply(`package`: Package, pxmlId: String, versionMajor: Int, versionMinor: Int, versionRelease: Int, versionBuild: Int, authorName: Option[String]): Application = {
    authorName.foreach(an => require(an.length <= 64, "Author name too long"))
    require(versionMajor >= 0,   "Version major value is negative")
    require(versionMinor >= 0,   "Version minor value is negative")
    require(versionRelease >= 0, "Version release value is negative")
    require(versionBuild >= 0,   "Version build value is negative")
    Application(`package`.id, pxmlId: String, versionMajor, versionMinor, versionRelease, versionBuild, authorName)
  }
}