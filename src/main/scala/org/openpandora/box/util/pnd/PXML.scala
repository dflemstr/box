package org.openpandora.box.util.pnd

import java.util.Locale
import net.liftweb.common.Logger
import org.openpandora.box.util.Localization
import org.openpandora.box.util.Languages
import scala.xml.NodeSeq

case class RequirementException(msg: String) extends Exception(msg)

class DOM(val raw: NodeSeq)(implicit localization: Localization, locale: Locale) extends Logger {
  import localization._
  def require(body: Boolean, message: String) = if(!body) throw new RequirementException(message)
  def error(message: String) = throw new RequirementException(message)

  protected def ?(key: String): String = loc(key, locale)
}

/**
 * Provides a DOM for a PXML file. It only supports a subset of the standard.
 */
class PXML(xml: NodeSeq)(implicit localization: Localization, locale: Locale) extends DOM(xml) {
  private def potentialApplicationNodes: NodeSeq = {
    //The old standard says one application per PXML. The new standard says multiple <application> tags.
    val appNodes = xml\"application"
    val title = xml\"title"
    val result = (if(title.length > 0) xml else Seq()) ++ (if(appNodes.length > 0) appNodes else Seq())
    require(result.length > 0, ?("validation.application.missing"))
    result
  }
  
  val applications = potentialApplicationNodes map (new Application(_))
}

class Application(xml: NodeSeq)(implicit localization: Localization, locale: Locale) extends DOM(xml) {
  val titles = xml\"title" map (new LocalizedString(_))
  require(titles.length > 0, ?("validation.title.missing"))
  require(titles exists (_.lang.toString.toLowerCase == Locale.US.toString.toLowerCase), ?("validation.title.noenus"))
  
  val descriptions = xml\"description" map (new LocalizedString(_))
  require(descriptions.length > 0, ?("validation.description.missing"))
  require(descriptions exists (_.lang.toString.toLowerCase == Locale.US.toString.toLowerCase), ?("validation.description.noenus"))
  
  val categories = xml\"categories"\"category" map (new Category(_))
  require(categories.length > 0, ?("validation.category.missing"))
  
  require((xml\"version").length == 1, ?("validation.version.onlyone"))
  val version = xml\"version" map (new Version(_)) head

  require((xml\"osversion").length < 2, ?("validation.osversion.toomany"))
  val osversion = xml\"osversion" map (new Version(_)) headOption

  require((xml\"author").length < 2, ?("validation.author.toomany"))
  val author = xml\"author" map (new Author(_)) headOption

  val id = (xml\"@id").text
}

class LocalizedString(xml: NodeSeq)(implicit localization: Localization, locale: Locale) extends DOM(xml) {
  private val langName = (xml\"@lang").text
  private val lowLang = langName.toLowerCase
  val lang = Languages.locales.find(_.toString.toLowerCase == lowLang) getOrElse error(?("validation.string.lang.invalid").replace("%lang%", langName))
  require((xml\"@lang").length == 1, ?("validation.string.lang.missing"))
  val text = xml.text.trim
  require(text.length > 0, ?("validation.string.empty"))
}

class Version(xml: NodeSeq)(implicit localization: Localization, locale: Locale) extends DOM(xml) {
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

class Author(xml: NodeSeq)(implicit localization: Localization, locale: Locale) extends DOM(xml) {
  private val tmpName = (xml\"@name").text
  private val tmpWebsite = (xml\"@website").text
  private val tmpEmail = (xml\"@email").text
  val name = if(tmpName.isEmpty) None else Some(tmpName)
  val website = if(tmpWebsite.isEmpty) None else Some(tmpWebsite)
  val email = if(tmpEmail.isEmpty) None else Some(tmpEmail)
}

class Category(xml: NodeSeq)(implicit localization: Localization, locale: Locale) extends DOM(xml) {
  val name = (xml\"@name").text
  require(name.length > 0, ?("validation.category.nameless"))
}
