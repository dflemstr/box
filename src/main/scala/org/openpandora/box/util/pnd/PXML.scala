package org.openpandora.box.util.pnd

import java.util.Locale
import java.util.ResourceBundle
import net.liftweb.common.Logger
import net.liftweb.http.LiftRules
import net.liftweb.util.Helpers
import net.liftweb.util.NamedPF
import org.openpandora.box.util.Localization._
import scala.xml.NodeSeq

case class RequirementException(msg: String) extends Exception(msg)

class DOM(val raw: NodeSeq, val locale: Locale) extends Logger {
  def require(body: Boolean, message: String) = if(!body) throw new RequirementException(message)

  protected def ?(key: String): String = loc(key, locale)
}

/**
 * Provides a DOM for a PXML file. It only supports a subset of the standard.
 */
class PXML(xml: NodeSeq, locale: Locale) extends DOM(xml, locale) {
  private def potentialApplicationNodes: NodeSeq = {
    //The old standard says one application per PXML. The new standard says multiple <application> tags.
    val appNodes = xml\"application"
    val title = xml\"title"
    val result = (if(title.length > 0) xml else Seq()) ++ (if(appNodes.length > 0) appNodes else Seq())
    require(result.length > 0, ?("validation.application.missing"))
    result
  }
  
  val applications = potentialApplicationNodes map (new Application(_, locale))
}

class Application(xml: NodeSeq, locale: Locale) extends DOM(xml, locale) {
  val titles = xml\"title" map (new LocalizedString(_, locale))
  require(titles.length > 0, ?("validation.title.missing"))
  require(titles exists (_.lang.toString.toLowerCase == Locale.US.toString.toLowerCase), ?("validation.title.noenus"))
  
  val descriptions = xml\"description" map (new LocalizedString(_, locale))
  require(descriptions.length > 0, ?("validation.description.missing"))
  require(descriptions exists (_.lang.toString.toLowerCase == Locale.US.toString.toLowerCase), ?("validation.description.noenus"))
  
  val categories = xml\"categories"\"category" map (new Category(_, locale))
  require(categories.length > 0, ?("validation.category.missing"))
  
  require((xml\"version").length == 1, ?("validation.version.onlyone"))
  val version = xml\"version" map (new Version(_, locale)) head

  require((xml\"osversion").length < 2, ?("validation.osversion.toomany"))
  val osversion = xml\"osversion" map (new Version(_, locale)) headOption

  require((xml\"author").length < 2, ?("validation.author.toomany"))
  val author = xml\"author" map (new Author(_, locale)) headOption

  val id = (xml\"@id").text
}

class LocalizedString(xml: NodeSeq, locale: Locale) extends DOM(xml, locale) {
  val lang = new Locale((xml\"@lang").text.toLowerCase)
  require((xml\"@lang").length == 1, ?("validation.string.lang.missing"))
  val text = xml.text.trim
  require(text.length > 0, ?("validation.string.empty"))
}

class Version(xml: NodeSeq, locale: Locale) extends DOM(xml, locale) {
  private def toInt(label: String) = {
    val text = (xml \ ("@" + label)).text
    require(text matches """\d{1,9}""", ?("validation.version.noint").replace("%versionfield%", label))
    val int = text.toInt
    require(int > -1,   ?("validation.version.invalid").replace("%versionfield%", label))
    int
  }
  val major = toInt("major")

  val minor = toInt("minor")
  
  val release = toInt("release")

  val build = toInt("build")
}

class Author(xml: NodeSeq, locale: Locale) extends DOM(xml, locale) {
  val name = (xml\"@name").text
  val website = (xml\"@website").text
}

class Category(xml: NodeSeq, locale: Locale) extends DOM(xml, locale) {
  val name = (xml\"@name").text
  require(name.length > 0, ?("validation.category.nameless"))
}
