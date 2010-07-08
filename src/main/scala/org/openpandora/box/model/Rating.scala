package org.openpandora.box.model

import org.squeryl.dsl.ManyToOne
import org.squeryl.KeyedEntity
import org.squeryl.dsl.CompositeKey2
import org.squeryl.PrimitiveTypeMode._

case class Rating(applicationId: Long, //id
                  userId:        Long, //id
                  value:         Int) extends KeyedEntity[CompositeKey2[Long, Long]] {
  def id = compositeKey(applicationId, userId)
  lazy val application: ManyToOne[Application] = Database.applicationsToRatings.right(this)
  lazy val user:        ManyToOne[User]        = Database.usersToRatings.right(this)
}

object Rating {
  def apply(application: Application, user: User, value: Int): Rating = {
    Rating(application.id, user.id, value)
  }
}