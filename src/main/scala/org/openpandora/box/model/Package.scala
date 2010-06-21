package org.openpandora.box.model

import java.util.Date
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.ManyToOne
import org.squeryl.dsl.OneToMany
import org.squeryl.annotations._
import scala.annotation.target.field

case class Package(userId:     Long,   //id
                   @Column(length = 16)
                   fileId:     String, //unique
                   @Column(length = 512)
                   fileName:   String,
                   uploadTime: Long,
                   hasImage:   Boolean) extends LongKeyedEntity {
  lazy val user:         ManyToOne[User]            = Database.usersToPackages.right(this)

  lazy val applications: OneToMany[Application]     = Database.packagesToApplications.left(this)
  lazy val downloads:    OneToMany[PackageDownload] = Database.packagesToPackageDownloads.left(this)

  def delete() = {
    Database.ratings.deleteWhere(_.applicationId in from(Database.applications)(app => where(app.packageId === id) select(app.id)))
    Database.appMetas.deleteWhere(_.applicationId in from(Database.applications)(app => where(app.packageId === id) select(app.id)))
    Database.categories.deleteWhere(_.applicationId in from(Database.applications)(app => where(app.packageId === id) select(app.id)))
    Database.comments.deleteWhere(_.applicationId in from(Database.applications)(app => where(app.packageId === id) select(app.id)))
    Database.applications.deleteWhere(_.packageId === id)
    Database.packageDownloads.deleteWhere(_.packageId === id)
    Database.packages.delete(id)
  }
}

object Package {
  def apply(user: User, fileName: String, uploadTime: Date, hasImage: Boolean): Package = {
    import net.liftweb.util.Helpers
    Package(user.id, Helpers.randomString(16), fileName, uploadTime.getTime, hasImage)
  }
  def apply(user: User, fileId: String, fileName: String, uploadTime: Date, hasImage: Boolean): Package = {
    Package(user.id, fileId, fileName, uploadTime.getTime, hasImage)
  }
}
