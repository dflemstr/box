package org.openpandora.box.model

import java.util.Date
import org.squeryl.dsl.ManyToOne
import org.squeryl.KeyedEntity
import org.squeryl.dsl.CompositeKey3
import org.squeryl.PrimitiveTypeMode._

case class PackageDownload(packageId: Long,        //id
                           userId:    Option[Long], /*id*/
                           date:      Date) extends KeyedEntity[CompositeKey3[Long, Option[Long], Date]] {
  def id = compositeKey(packageId, userId, date)
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
