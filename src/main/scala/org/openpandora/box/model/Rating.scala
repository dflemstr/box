package org.openpandora.box.model

import org.squeryl.dsl.ManyToOne

case class Rating(applicationId: Long, //id
                  userId:        Long, //id
                  value:         Int) extends LongKeyedEntity {
  lazy val application: ManyToOne[Application] = Database.applicationsToRatings.right(this)
  lazy val user:        ManyToOne[User]        = Database.usersToRatings.right(this)
}

object Rating {
  def apply(application: Application, user: User, value: Int): Rating = {
    Rating(application.id, user.id, value)
  }
}