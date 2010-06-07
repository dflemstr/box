package org.openpandora.box.model

import java.util.Locale
import java.util.TimeZone
import net.liftweb.http.CleanRequestVarOnSessionTransition
import net.liftweb.http.RequestVar
import net.liftweb.http.S
import net.liftweb.http.SessionVar
import org.squeryl.annotations._
import net.liftweb.util.Helpers
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.OneToMany
import scala.annotation.target.field

case class User(@(Column @field)(length = 64)
                username:     String,
                @(Column @field)(length = 128)
                email:        String, //email
                @(Column @field)(length = 16)
                passwordSalt: String, //salt
                @(Column @field)(length = 30)
                passwordHash: String, //hash
                @(Column @field)(length = 6)
                languageName: String, //lang
                @(Column @field)(length = 32)
                timeZoneName: String, //tz
                admin:        Boolean,
                @(Column @field)(length = 16)
                emailUid:     String, //unique
                validated:    Boolean) extends LongKeyedEntity {
  lazy val comments:         OneToMany[Comment]         = Database.usersToComments.left(this)
  lazy val packageDownloads: OneToMany[PackageDownload] = Database.usersToPackageDownloads.left(this)
  lazy val packages:         OneToMany[Package]         = Database.usersToPackages.left(this)
  lazy val ratings:          OneToMany[Rating]          = Database.usersToRatings.left(this)

  def timeZone = TimeZone.getTimeZone(timeZoneName)
  def language = Locale.getAvailableLocales.find(_.toString == languageName) getOrElse Locale.getDefault

  def checkPassword(password: String) = (Helpers.hash(passwordSalt + password) == passwordHash)

  def login() {
    User._currentUser.remove()
    User._currentUserId.set(Some(id))
  }

  def logout() {
    User._currentUser.remove()
    User._currentUserId.remove()
    S.request.foreach(_.request.session.terminate)
  }
}

object User {
  def apply(username: String, email: String, password: String, language: Locale, timeZone: TimeZone, admin: Boolean, validated: Boolean): User = {
    val salt = Helpers.randomString(16)
    User(username, email, salt, Helpers.hash(salt + password), language.toString, timeZone.getID, admin, Helpers.randomString(16),  validated)
  }

  private object _currentUserId extends SessionVar[Option[Long]](None)

  private object _currentUser extends RequestVar[Option[User]](currentUserId.flatMap(Database.users.lookup(_)))
                                 with CleanRequestVarOnSessionTransition

  def currentUserId = _currentUserId.is
  def currentUser = _currentUser.is

  def refresh(): Unit = _currentUser.remove()

  def loggedIn = currentUserId.isDefined

  def nameFor(id: Long) = Database.users.lookup(id).map(_.username) getOrElse "Unknown"
}