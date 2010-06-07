package org.openpandora.box.model

import java.util.Date
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.ManyToOne
import org.squeryl.dsl.OneToMany
import org.squeryl.annotations._
import scala.annotation.target.field

case class Package(userId:     Long,   //id
                   @(Column @field)(length = 16) val
                   fileId:     String, //unique
                   @(Column @field)(length = 64) val
                   fileName:   String,
                   uploadTime: Long,
                   hasImage:   Boolean) extends LongKeyedEntity {
  lazy val user:         ManyToOne[User]            = Database.usersToPackages.right(this)

  lazy val applications: OneToMany[Application]     = Database.packagesToApplications.left(this)
  lazy val downloads:    OneToMany[PackageDownload] = Database.packagesToPackageDownloads.left(this)
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
