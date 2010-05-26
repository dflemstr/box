package bootstrap.liftweb

import java.util.Locale
import java.util.TimeZone
import javax.mail.Authenticator
import javax.mail.PasswordAuthentication
import net.liftweb.common.Box
import net.liftweb.common.Full
import net.liftweb.common.Logger
import net.liftweb.http.DocType
import net.liftweb.http.LiftRules
import net.liftweb.http.OnDiskFileParamHolder
import net.liftweb.http.ParsePath
import net.liftweb.http.ResponseInfo
import net.liftweb.http.RewriteRequest
import net.liftweb.http.RewriteResponse
import net.liftweb.http.S
import net.liftweb.http.js.JE
import net.liftweb.http.js.JsCmds
import net.liftweb.http.provider.HTTPRequest
import net.liftweb.sitemap.Loc
import net.liftweb.sitemap.Menu
import net.liftweb.sitemap.SiteMap
import net.liftweb.util.Mailer
import net.liftweb.util.Props
import org.openpandora.box.dispatch.FileDispatcher
import org.openpandora.box.model._
import org.openpandora.box.util.packages.PackageManager
import org.openpandora.box.util.packages.ProcessNotifier
import org.openpandora.box.util.notifications.Poster

/**
 * A class that's instantiated early and run.
 */
class Boot extends Logger {
  def boot {
    configLift()
    createDatabase()
    setupEmail()
    buildSitemap()
    startDaemons()
  }

  private def createDatabase() {
    import org.squeryl.adapters._
    import org.squeryl.Session
    import org.squeryl.SessionFactory
    import org.squeryl.PrimitiveTypeMode._

    val driver = Props.get("db.driver") openOr "org.h2.Driver"
    val url = Props.get("db.url")       openOr "jdbc:h2:mem:box;AUTO_SERVER=TRUE"
    val user = Props.get("db.user")
    val password = Props.get("db.password")
    val userPass = for(u <- user.toOption; p <- password.toOption) yield (u, p)
    info("Connecting to database, driver=" + driver + " url=" + url + " user=" + user + " password=" + password)
    
    //Check whether the driver class exists
    Class.forName(driver)

    val adapter = driver match {
      case "org.h2.Driver" => new H2Adapter
      case "com.mysql.jdbc.Driver" => new MySQLAdapter
      case "org.postgresql.Driver" => new PostgreSqlAdapter
      case "oracle.jdbc.driver.OracleDriver" => new OracleAdapter
      case _ =>
        error("Unsupported database driver: no adapter, url=" + url)
        Predef.error("Unsupported database driver: no adapter, url=" + url)
    }
    
    userPass match {
      case Some((user, pass)) =>
        SessionFactory.concreteFactory = Some(() =>
          Session.create(java.sql.DriverManager.getConnection(url, user, pass), adapter)
        )
      case None =>
        SessionFactory.concreteFactory = Some(() =>
          Session.create(java.sql.DriverManager.getConnection(url), adapter)
        )
    }

    try transaction{Database.create; info("Database created, url=" + url)} catch {case _ =>}
  }

  private def startDaemons() {
    PackageManager.start()
    ProcessNotifier.start()
    Poster.start()
  }

  private lazy val useEmail = Props.get("mail.enable").map(_.toBoolean) openOr false

  private def setupEmail() = if(useEmail) {
    Props.get("mail.smtp.host").foreach(System.setProperty("mail.smtp.host", _))
    Props.get("mail.smtp.auth").foreach(System.setProperty("mail.smtp.auth", _))
    for {
      user <- Props.get("mail.smtp.user")
      password <- Props.get("mail.smtp.password")
    } Mailer.authenticator = Full(new Authenticator {
        override def getPasswordAuthentication = new PasswordAuthentication(user, password)
      })
  }

  private def buildSitemap() {
    import net.liftweb.sitemap.Loc._

    val isLoggedIn = If(User.currentUser.isDefined _, "Must be logged in")
    val isLoggedOut = If(User.currentUser.isEmpty _, "Please log out first")

    val userMenu = Menu(Loc("User", List("user", "view"), "User overview"),
                        Menu(Loc("UserLogin", List("user", "login"), "Log in", If(User.currentUser.isEmpty _, "Already logged in"))),
                        Menu(Loc("UserLogout", List("user", "logout"), "Log out", isLoggedIn)),
                        Menu(Loc("UserCreate", List("user", "create"), "Register", isLoggedOut)),
                        Menu(Loc("UserLostPassword", List("user", "lost-password"), "Lost password", If(useEmail _, "E-Mail system disabled"), isLoggedOut)),
                        Menu(Loc("UserResetPassword", List("user", "reset-password"), "Reset password", Hidden, isLoggedOut)),
                        Menu(Loc("UserEdit", List("user", "edit"), "Account details", isLoggedIn)),
                        Menu(Loc("UserChangePassword", List("user", "change-password"), "Change password", isLoggedIn)),
                        Menu(Loc("UserValidate", List("user", "validate"), "Validate user", Hidden, isLoggedOut)))

    val applicationMenu = Menu(Loc("Applications", List("applications", "list"), "Applications"),
                               Menu(Loc("ShowApplication", List("applications", "show"), "Show application", Hidden)),
                               Menu(Loc("AddApplications", List("applications", "add"), "Add applications", isLoggedIn)),
                               Menu(Loc("ConstructAppFilter", List("applications", "filter"), "Construct filter")))

    val entries = Menu(Loc("Home", List("index"), "Home")) :: applicationMenu :: userMenu :: Nil
    LiftRules.setSiteMap(SiteMap(entries: _*))
  }

  private def configLift() {
    LiftRules.addToPackages("org.openpandora.box")

    LiftRules.loggedInTest = Full(User.currentUser.isDefined _)

    LiftRules.timeZoneCalculator = (request: Box[HTTPRequest]) =>
    (User.currentUser map (_.timeZone) getOrElse TimeZone.getDefault)

    LiftRules.localeCalculator = (request: Box[HTTPRequest]) =>
    (User.currentUser map (_.language) getOrElse (request flatMap (_.locale) openOr Locale.getDefault))

    LiftRules.ajaxStart =
      Full(JE.Call("startAjax").cmd _)

    LiftRules.ajaxEnd =
      Full(JE.Call("stopAjax").cmd _)

    LiftRules.noticesToJsCmd = noticesJsCmd

    LiftRules.handleMimeFile = OnDiskFileParamHolder(_, _, _, _)

    LiftRules.early.append(makeUtf8)

    LiftRules.resourceNames ::= "translations/core"

    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath(List("user", "messages", id, "show"), _, _, _), _, _) =>
        RewriteResponse(List("user", "messages", "show"), Map("id" -> id))
      case RewriteRequest(ParsePath(List("applications", id, "show"), _, _, _), _, _) =>
        RewriteResponse(List("applications", "show"), Map("id" -> id))
    }

    LiftRules.dispatch.append(FileDispatcher.dispatch)

    LiftRules.explicitlyParsedSuffixes += "pnd"

    ResponseInfo.docType = {
      case _ if S.getDocType._1 => S.getDocType._2
      case _ => Full(DocType.xhtml11)
    }
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) = {
    req.setCharacterEncoding("UTF-8")
  }

  private val noticesJsCmd = () => {
    import net.liftweb.builtin.snippet._
    import scala.xml._

    def messageCall(kind: String, title: String, body: NodeSeq, assocId: Option[String] = None) = assocId match {
      case Some(id) =>
        JE.Call("displayNotice", JE.Str(kind), JE.Str(title), JE.Str(body.toString), JE.Str(id)).cmd
      case None =>
        JE.Call("displayNotice", JE.Str(kind), JE.Str(title), JE.Str(body.toString), JE.JsNull).cmd
    }

    val messages = S.errors.map(("error", "Error", _)) ++ S.warnings.map(("warning", "Warning", _)) ++ S.notices.map(("notice", "Information", _))

    val commands = for {
      (kind, title, data) <- messages
      body = data._1
      assocId = data._2 toOption
    } yield messageCall(kind, title, body, assocId)

    if(commands.isEmpty)
      JsCmds.Noop
    else
      commands.reduceLeft(_ & _)
  }
}
