package org.openpandora.box.model

import java.util.Date
import org.squeryl.dsl.ManyToOne
import org.squeryl.annotations._
import scala.annotation.target.field

case class Comment(userId:        Long, //id
                   applicationId: Long, //id
                   time:          Long,
                   @(Column @field)(length = 1024)
                   body:          String) extends LongKeyedEntity {
  def date = new Date(time)
  lazy val application: ManyToOne[Application] = Database.applicationsToComments.right(this)
  lazy val user:        ManyToOne[User]        = Database.usersToComments.right(this)
}

object Comment {
  def apply(user: User, application: Application, date: Date, body: String): Comment = {
    Comment(user.id, application.id, date.getTime, body)
  }
}
