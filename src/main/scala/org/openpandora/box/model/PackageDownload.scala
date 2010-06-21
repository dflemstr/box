package org.openpandora.box.model

import java.util.Date
import org.squeryl.dsl.ManyToOne

case class PackageDownload(packageId: Long,        //id
                           userId:    Option[Long], /*id*/
                           date:      Date) extends LongKeyedEntity {
  lazy val `package`: ManyToOne[Package] = Database.packagesToPackageDownloads.right(this)
  lazy val user:      ManyToOne[User]    = Database.usersToPackageDownloads.right(this)

  final def pkg = `package` //alias

  def this() = this(0, Some(0), new Date())
}

object PackageDownload {
  def apply(`package`: Package, user: User): PackageDownload =
    PackageDownload(`package`.id, Some(user.id), new Date)
  def apply(`package`: Package): PackageDownload =
    PackageDownload(`package`.id, None, new Date)
}
