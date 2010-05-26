package org.openpandora.box.util.pnd

import java.util.Locale
import net.liftweb.common.Logger
import scala.xml.NodeSeq

case class RequirementException(msg: String) extends Exception(msg)

class DOM(val raw: NodeSeq) extends Logger {
  def require(body: Boolean, message: String) = if(!body) throw new RequirementException(message)
}

/**
 * Provides a DOM for a PXML file. It only supports a subset of the standard.
 */
class PXML(xml: NodeSeq) extends DOM(xml) {
  private def potentialApplicationNodes: NodeSeq = {
    //The old standard says one application per PXML. The new standard says multiple <application> tags.
    val appNodes = xml\"application"
    val title = xml\"title"
    val result = (if(title.length > 0) xml else Seq()) ++ (if(appNodes.length > 0) appNodes else Seq())
    require(result.length > 0, "The PXML does not contain any application information!")
    result
  }
  
  val applications = potentialApplicationNodes map (new Application(_))
}

class Application(xml: NodeSeq) extends DOM(xml) {
  val titles = xml\"title" map (new LocalizedString(_))
  require(titles.length > 0, "There is no title information in the PXML!")
  require(titles exists (_.lang.toString.toLowerCase == Locale.US.toString.toLowerCase), "There is no American English (en_US) title!")
  
  val descriptions = xml\"description" map (new LocalizedString(_))
  require(descriptions.length > 0, "There is no description information in the PXML!")
  require(descriptions exists (_.lang.toString.toLowerCase == Locale.US.toString.toLowerCase), "There is no American English (en_US) description!")
  
  val categories = xml\"categories"\"category" map (new Category(_))
  require(categories.length > 0, "The PXML doesn't contain any categories!")
  
  require((xml\"version").length == 1, "There must be one version tag in the PXML file!")
  val version = xml\"version" map (new Version(_)) apply 0

  require((xml\"osversion").length < 2, "There are too many osversion tags in the PXML file!")
  val osversion = xml\"osversion" map (new Version(_)) headOption

  require((xml\"author").length < 2, "There are too many author tags in the PXML file!")
  val author = xml\"author" map (new Author(_)) headOption
}

class LocalizedString(xml: NodeSeq) extends DOM(xml) {
  val lang = new Locale((xml\"@lang").text)
  require((xml\"@lang").length == 1, "A localized string is missing a lang attribute!")
  val text = xml.text.trim
  require(text.length > 0, "A localized string is lacking text in the PXML!")
}

class Version(xml: NodeSeq) extends DOM(xml) {
  val major = (xml\"@major").text.toInt
  require(major > -1,   "Invalid major version number in the PXML")

  val minor = (xml\"@minor").text.toInt
  require(minor > -1,   "Invalid minor version number in the PXML")
  
  val release = (xml\"@release").text.toInt
  require(release > -1, "Invalid release version number in the PXML")

  val build = (xml\"@build").text.toInt
  require(build > -1,   "Invalid build version number in the PXML")
}

class Author(xml: NodeSeq) extends DOM(xml) {
  val name = (xml\"@name").text
  val website = (xml\"@website").text
}

class Category(xml: NodeSeq) extends DOM(xml) {
  val name = (xml\"@name").text
  require(name.length > 0, "There is a nameless category in the PXML")
}
