package org.openpandora.box.model

import java.util.Date
import org.squeryl.annotations.Column
import org.squeryl.dsl.CompositeKey3
import org.squeryl.dsl.ManyToOne
import org.squeryl.KeyedEntity
import org.squeryl.PrimitiveTypeMode._

case class Comment(userId:        Long, //id
                   applicationId: Long, //id
                   time:          Long,
                   @Column(length = 1024)
                   body:          String) extends KeyedEntity[CompositeKey3[Long, Long, Long]] {
  def id = compositeKey(userId, applicationId, time)
  def date = new Date(time)
  lazy val application: ManyToOne[Application] = Database.applicationsToComments.right(this)
  lazy val user:        ManyToOne[User]        = Database.usersToComments.right(this)
}

object Comment {
  def apply(user: User, application: Application, date: Date, body: String): Comment = {
    Comment(user.id, application.id, date.getTime, body)
  }
}
