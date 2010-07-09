package org.openpandora.box.model

import org.squeryl.Query
import org.squeryl.annotations.Column
import org.squeryl.dsl.ManyToOne
import org.squeryl.dsl.OneToMany
import org.squeryl.PrimitiveTypeMode._


case class Application(packageId:       Long, //id
                       @Column(length = 512)
                       pxmlId:          String,
                       versionMajor:    Int,
                       versionMinor:    Int,
                       versionRelease:  Int,
                       versionBuild:    Int,
                       @Column(length = 512)
                       authorName:      Option[String],
                       @Column(length = 512)
                       authorURL:       Option[String],
                       @Column(length = 512)
                       authorEmail:       Option[String],
                       var ratingCount:     Long,
                       var ratingAverage:   Float,
                       val newest:       Boolean) extends LongKeyedEntity {
  lazy val `package`:  ManyToOne[Package]  = Database.packagesToApplications.right(this)

  lazy val categories: OneToMany[Category] = Database.applicationsToCategories.left(this)
  lazy val comments:   OneToMany[Comment]  = Database.applicationsToComments.left(this)
  lazy val metas:      OneToMany[AppMeta]  = Database.applicationsToAppMetas.left(this)
  lazy val ratings:    OneToMany[Rating]   = Database.applicationsToRatings.left(this)

  lazy val commentsWithUsers: Query[(Comment, User)] =
    from(Database.comments, Database.users)((comment, user) => where(comment.applicationId === id and comment.userId === user.id) select(comment, user))

  lazy val categoriesSeq = categories.toSeq
  lazy val commentsSeq   = comments.toSeq
  lazy val metasSeq      = metas.toSeq

  lazy val commentsWithUsersSeq = commentsWithUsers.toSeq

  final def pkg = `package` //alias

  def version = versionMajor + "." + versionMinor + "." + versionBuild + "." + versionRelease

  def hasRated(user: User) =
    from(Database.ratings)(rating => where(rating.userId === user.id and rating.applicationId === id) compute(count)) > 0

  def this() = this(0l, "", 0, 0, 0, 0, Some(""), Some(""), Some(""), 0, 0f, false)
}

object Application {
  def updatesCount(pxmlId: String, major: Int, minor: Int, release: Int, build: Int): Long =
    from(Database.applications)(app => where(
        app.pxmlId === pxmlId and (
          (
            (app.versionMajor   gt  major)
          ) or (
            (app.versionMajor   === major) and
            (app.versionMinor   gt  minor)
          ) or (
            (app.versionMajor   === major) and
            (app.versionMinor   === minor) and
            (app.versionRelease gt  release)
          ) or (
            (app.versionMajor   === major) and
            (app.versionMinor   === minor) and
            (app.versionRelease === release) and
            (app.versionBuild   gt  build)
          )
        )) compute(count))

  def apply(`package`: Package, pxmlId: String,
            versionMajor: Int, versionMinor: Int, versionRelease: Int, versionBuild: Int,
            authorName: Option[String], authorURL: Option[String], authorEmail: Option[String]): Application = {
    val isNewest = updatesCount(pxmlId, versionMajor, versionMinor, versionRelease, versionBuild) == 0l

    if(isNewest) update(Database.applications)(app => where(app.pxmlId === pxmlId) set(app.newest := false))

    Application(`package`.id, pxmlId: String,
                versionMajor, versionMinor, versionRelease, versionBuild,
                authorName, authorURL, authorEmail, 0, 0f, isNewest)
  }
}