package org.openpandora.box.snippet

import net.liftweb.http.js.JsCmds
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import net.liftweb.util.Mailer
import net.liftweb.util.Mailer.From
import net.liftweb.util.Mailer.Subject
import net.liftweb.util.Mailer.To
import java.util.Locale
import java.util.TimeZone
import net.liftweb.common.Full
import net.liftweb.common.Logger
import net.liftweb.http.DispatchSnippet
import net.liftweb.http.RequestVar
import net.liftweb.http.S
import net.liftweb.http.SHtml
import net.liftweb.util.Props
import org.openpandora.box.model.Database
import org.openpandora.box.model.User
import org.openpandora.box.util.Languages
import org.openpandora.box.util.TimeZones
import org.openpandora.box.util.notifications.Poster
import org.squeryl.PrimitiveTypeMode._
import scala.xml.NodeSeq
import scala.xml.Text

class Users extends DispatchSnippet with Logger {
  def dispatch = {
    case "isLoggedIn" => isLoggedIn
    case "isLoggedOut" => isLoggedOut
    case "login" => login
    case "logout" => logout
    case "create" => create
    case "edit" => edit
    case "lostPassword" => lostPassword
    case "resetPassword" => resetPassword
    case "changePassword" => changePassword
    case "info" => info
    case "sendMessage" => sendMessage
  }

  private object username extends RequestVar("")
  private object oldPassword extends RequestVar[String]("")
  private object passwords extends RequestVar[Seq[String]](Nil)
  private object email extends RequestVar("")
  private object language extends RequestVar[Locale](S.locale)
  private object timeZone extends RequestVar[TimeZone](S.timeZone)

  private object createFunction extends RequestVar[Option[() => NodeSeq]](None)
  private object editFunction extends RequestVar[Option[() => NodeSeq]](None)

  private lazy val useEmail = Props.get("mail.enable").map(_.toBoolean) getOrElse false
  private val emailRegex = """[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=""" +
  """?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+(?:[A-Z]{2}|com|org|net|edu""" +
  """|gov|mil|biz|info|mobi|name|aero|asia|jobs|museum)\b"""

  def isLoggedIn(seq: NodeSeq) = if(User.currentUser.isDefined) seq else NodeSeq.Empty
  def isLoggedOut(seq: NodeSeq) = if(User.currentUser.isDefined) NodeSeq.Empty else seq

  def login(login: NodeSeq): NodeSeq = {
    var password = ""
    def doLogin() = {
      transaction(from(Database.users)(user => where(user.username === username.is) select(user)).headOption) match {
        case Some(user) if user.validated && (user.checkPassword(password)) =>
          user.login()
          S.notice(<p>{S.?("user.loggedin")}</p>)
          S.redirectTo("/")
        case Some(user) if !user.validated =>
          S.error(<p>{S.?("user.email.validationPending")}</p>)
        case _ =>
          S.error(<p>{S.?("user.invalid")}</p>)
      }
    }
    bind("login", login,
         "username" -> SHtml.text(username, username.set, "id" -> "username-field"),
         //We use "" because the password should always be cleared:
         "password" -> SHtml.password("", x => password = x, "id" -> "password-field"),
         "submit" -> SHtml.submit("Log in", doLogin))
  }

  def logout(foo: NodeSeq): NodeSeq = {
    User.currentUser.foreach(_.logout())
    S.redirectTo(S.referer openOr "/")
  }

  private def validateUser(body: Boolean, alsoPassword: Boolean) = inTransaction {
    def mkValidation(test: Boolean, id: String, message: String) = if(test) Seq(id -> message) else Seq.empty
    mkValidation(body && from(Database.users)(user => where(user.username === username.is) compute(count)) > 0, "username", "Username does already exist")
    mkValidation(body && username.is.length > 64, "username", "User name too long (max: 64)") ++
    mkValidation(body && username.is.length < 4,"username", "User name too short (min: 4)") ++
    mkValidation(body && !(username.is matches """[\w_-]+"""),"username", """Invalid username (only letters, underscores and dashes allowed)""") ++
    mkValidation(body && !(email.is matches emailRegex),"email", "Invalid email address.") ++
    mkValidation(alsoPassword && passwords.toSet.size > 1,"password", "Passwords don't match") ++
    mkValidation(alsoPassword && passwords.exists(_.length < 4),"password", "Password too short (min: 4)")
  }
  
  def create(create: NodeSeq): NodeSeq = createFunction.is.map(_()) getOrElse {
    def doCreate(): Unit = {
      info("Create callback called for registration attempt, username=" + username.is + " email=" + email.is)
      validateUser(true, true) match {
        case Seq() =>
          val user = inTransaction(Database.users.insert(User(username.is, email.is, passwords.is.head, language.is, timeZone.is, false, !useEmail)))
          if(useEmail) {
            sendValidationEmail(user)
            S.notice(S.?("user.email.validationSent"))
          } else {
            user.login()
            S.notice(S.?("user.welcome"))
          }
          S.redirectTo("/")
        case errors =>
          info("Errors while registering, username=" + username.is + " errors=" + errors)
          errors.foreach(err => S.error(err._1 + "-field", <p>{err._2}</p>))
          createFunction.set(Some(createFunc))
      }
    }
    def createFunc() =
      bind("create", create,
           "username" -> SHtml.text(username.is, username.set, "size" -> "64", "id" -> "username-field"),
           "email"    -> SHtml.text(email.is, email.set, "size" -> "128", "id" -> "email-field"),
           "password" -> SHtml.password_*("", S.LFuncHolder(passwords.set), "class" -> "password-field"),
           "language" -> SHtml.selectObj(Languages.localesAndNames(S.locale), Full(language.is), language.set, "id" -> "language-field"),
           "timezone" -> SHtml.selectObj(TimeZones.timeZonesAndCodes, Full(timeZone.is), timeZone.set, "id" -> "timezone-field"),
           "submit"   -> SHtml.submit("Create user", doCreate _))
    createFunc()
  }

  private def sendValidationEmail(user: User) = {
    val validationLink = S.hostAndPath + "/user/validate?id=" + user.emailUid
    S.runTemplate("mail-templates-hidden" :: "validate-email" :: Nil) match {
      case Full(template) =>
        val email = bind("validate", template,
                         "name" -> user.username,
                         "link" -> ((x: NodeSeq) => <a href={validationLink}>{x}</a>))
        Mailer.sendMail(From("noreply@" + S.hostName),
                        Subject(S.hostName + " account validation request"),
                        To(user.email),
                        Mailer.xmlToMailBodyType(email))
      case _ =>
        S.error(Seq(<em>INTERNAL ERROR:</em>, <br/>,
                    Text("No email template found for 'validate-email'! " +
                         "Please report this error to the site administrator."), <br/>,
                    Text("The email was not sent")))
    }
  }
  
  def edit(edit: NodeSeq): NodeSeq = editFunction.map(_()) getOrElse {
    User.currentUser match {
      case Some(user) =>
        username.set(user.username)
        email.set(user.email)
        language.set(user.language)
        timeZone.set(user.timeZone)
        def doEdit(): Unit = validateUser(true, false) match {
          case Seq() =>
            transaction{
              update(Database.users){u =>
                where(u.id === user.id)
                set(u.username := username.is,
                    u.email := email.is,
                    u.languageName := language.is.toString,
                    u.timeZoneName := timeZone.is.getID)
              }
            }
            User.refresh()
            S.notice(<p>{S.?("user.profile.updated")}</p>)
            editFunction.set(Some(editFunc))
          case errors =>
            errors.foreach(err => S.error(err._1 + "-field", <p>{err._2}</p>))
            editFunction.set(Some(editFunc))
        }
        def editFunc() =
          bind("edit", edit,
               "username" -> SHtml.text(username.is, username.set, "size" -> "64", "id" -> "username-field"),
               "email"    -> SHtml.text(email.is, email.set, "size" -> "128", "id" -> "email-field"),
               "language" -> SHtml.selectObj(Languages.localesAndNames(S.locale), Full(language.is), language.set, "id" -> "language-field"),
               "timezone" -> SHtml.selectObj(TimeZones.timeZonesAndCodes, Full(timeZone.is), timeZone.set, "id" -> "timezone-field"),
               "submit"   -> SHtml.submit("Save changes", doEdit _))
        editFunc()
      case None =>
        S.error(<p>{S.?("user.mustlogin")}</p>)
        S.redirectTo(S.referer openOr "/")
    }
  }

  def passwordReset(resetPassword: NodeSeq, id: String): NodeSeq =
    from(Database.users)(user => where(user.emailUid === id) select(user)).headOption match {
      case Some(user) =>
        def doSet() = validateUser(false, true) match {
          case Seq() =>
            val passwordSalt = Helpers.randomString(16)
            val passwordHash = Helpers.hash(passwordSalt + passwords.is.head)
            inTransaction(update(Database.users){u =>
                where(u.id === user.id)
                set(u.passwordSalt := passwordSalt,
                    u.passwordHash := passwordHash,
                    u.emailUid := Helpers.randomString(16))
              })
            User.refresh()
            S.notice(<p>{S.?("user.password.changed")}</p>)

            user.login()
            S.redirectTo(S.referer openOr "/")
          case errors =>
            errors.foreach(err => S.error(err._1 + "-field", <p>{err._2}</p>))
        }
        bind("resetPassword", resetPassword,
             "password" -> SHtml.password_*("", S.LFuncHolder(passwords.set), "class" -> "password-field"),
             "submit" -> SHtml.submit("Reset password", doSet), "id" -> "submit-field")
      case _ =>
        S.error(Seq(Text("Invalid password link. "), <a href="/user/lost-password">Get a valid one</a>))
        S.redirectTo(S.referer openOr "/")
    }

  def resetPassword(resetPassword: NodeSeq): NodeSeq = S.param("id") match {
    case Full(id) =>
      passwordReset(resetPassword, id)
    case _ =>
      S.error(<p>How did you get in there? Don't try to fiddle around!</p>)
      S.redirectTo(S.referer openOr "/")
  }

  def lostPassword(lostPassword: NodeSeq): NodeSeq = {
    def doResetPassword(email: String) = inTransaction(from(Database.users)(user => where(user.email === email) select(user))).headOption match {
      case Some(user) if user.validated =>
        val resetLink = S.hostAndPath + "/user/reset-password?id=" + user.emailUid

        S.runTemplate("mail-templates-hidden" :: "reset-password-email" :: Nil)match {
          case Full(template) =>
            val email = bind("reset", template,
                             "name" -> user.username,
                             "link" -> ((x: NodeSeq) => <a href={resetLink}>{x}</a>))
            Mailer.sendMail(From("noreply@" + S.hostName),
                            Subject(S.hostName + " password reset request"),
                            To(user.email),
                            Mailer.xmlToMailBodyType(email))
            S.notice(<p>{"Password reset email sent"}</p>)
            S.redirectTo(S.referer openOr "/")
          case _ =>
            S.error(Seq(<em>INTERNAL ERROR:</em>, <br/>,
                        Text("No email template found for 'reset-password-email'! " +
                             "Please report this error to the site administrator."), <br/>,
                        Text("The email was not sent")))
        }
      case Some(user) if !user.validated =>
        sendValidationEmail(user)
        S.notice(<p>Resent account confirmation email</p>)
        S.redirectTo(S.referer openOr "/")
      case _ =>
        S.error(<p>Email address couldn't be found</p>)
    }
    bind("lostPassword", lostPassword,
         "email" -> SHtml.text("", email.set, "id" -> "email-field"),
         "submit" -> SHtml.submit("Send password link", () => doResetPassword(email)))
  }

  def changePassword(changePassword: NodeSeq): NodeSeq = {
    val user = User.currentUser.get

    def doChangePassword() {
      if (!user.checkPassword(oldPassword.is))
        S.error(<p>Wrong old password</p>)
      else validateUser(false, true) match {
        case Seq() =>
          val passwordSalt = Helpers.randomString(16)
          val passwordHash = Helpers.hash(passwordSalt + passwords.is.head)
          inTransaction(update(Database.users){u =>
              where(u.id === user.id)
              set(u.passwordSalt := passwordSalt,
                  u.passwordHash := passwordHash)
            })
          User.refresh()
          S.notice(<p>{S.?("user.password.changed")}</p>)

          user.login()
          S.redirectTo(S.referer openOr "/")
        case errors =>
          errors.foreach(err => S.error(err._1 + "-field", <p>{err._2}</p>))
      }
    }

    bind("changePassword", changePassword,
         "oldPassword" -> SHtml.password("", oldPassword.set, "id" -> "oldpassword-field"),
         "newPassword" -> SHtml.password_*("", S.LFuncHolder(passwords.set), "class" -> "password-field"),
         "submit" -> SHtml.submit("Change password", doChangePassword))
  }

  def info(info: NodeSeq) = Text("TODO") ++ info

  def sendMessage(stuff: NodeSeq) =
    SHtml.ajaxButton("Send message", () => {
        Poster! Poster.SendMessage(User.currentUser.map(_.id) getOrElse 0l, "notice", <p>Hello World</p>, <p>Hello World</p>)
        JsCmds.Noop
      })
}
