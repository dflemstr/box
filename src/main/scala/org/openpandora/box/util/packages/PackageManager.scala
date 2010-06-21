package org.openpandora.box.util.packages

import java.awt.RenderingHints
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.io.File
import java.io.FileOutputStream
import java.io.InputStream
import java.nio.channels.Channels
import java.util.Locale
import javax.imageio.ImageIO
import net.liftweb.common.Logger
import org.openpandora.box.model.Application
import org.openpandora.box.model.Category
import net.liftweb.util.Helpers
import org.openpandora.box.model.AppMeta
import org.openpandora.box.model.Database
import org.openpandora.box.model.Package
import org.openpandora.box.model.User
import org.openpandora.box.util.DotDesktopCategories
import org.openpandora.box.util.Localization
import org.openpandora.box.util.filesystem.FileType
import org.openpandora.box.util.filesystem.Filesystem
import org.openpandora.box.util.filesystem.PNDFile
import org.openpandora.box.util.filesystem.PNGImage
import org.openpandora.box.util.filesystem.PXMLFile
import org.openpandora.box.util.pnd
import org.openpandora.box.util.pnd.PNDBinaryInfo
import org.openpandora.box.util.pnd.PXML
import org.openpandora.box.util.pnd.RequirementException
import org.squeryl.PrimitiveTypeMode._
import org.xml.sax.SAXException
import scala.actors.Actor
import scala.actors.Actor._
import scala.math.{min => minNumber}
import scala.xml._

object PackageManager {
  val default: PackageManager = new PackageManagerImpl
}

trait PackageManager {
  def makePackageFromStream(filename: String, input: InputStream, user: User)
  def registerPackageAddedCallback(callback: Package => Unit)
  def unregisterPackageAddedCallback(callback: Package => Unit)
}

private[packages] class PackageManagerImpl(fs: Filesystem = Filesystem.default,
                                           pn: ProcessNotifier = ProcessNotifier.default,
                                           loc: Localization = Localization.default) extends PackageManager
                                                                                        with Actor
                                                                                        with Logger {
  start()

  def registerPackageAddedCallback(callback: Package => Unit) = packageAddedCallbacks +:= callback

  def unregisterPackageAddedCallback(callback: Package => Unit) = packageAddedCallbacks = packageAddedCallbacks filterNot (_ == callback)

  def makePackageFromStream(filename: String, input: InputStream, user: User) =
    this! MakePackageFromStream(filename, input, user)

  var packageAddedCallbacks: Seq[Package => Unit] = Seq.empty

  case class MakePackageFromStream(filename: String, input: InputStream, user: User)

  def act = loop {
    react {
      case MakePackageFromStream(filename, inputStream, user) =>
        createPackage(filename, inputStream, user)(fs, pn, loc)
      case x => info("PackageManager received an unknown message, message=" + x)
    }
  }

  def createPackage(filename: String, inputStream: InputStream, user: User)(implicit fs: Filesystem, pn: ProcessNotifier, loc: Localization) = {
    implicit val u = user
    implicit val l = user.language
    implicit val f = filename

    val id = createUID()

    try {
      //Try to save the file
      val file = savePNDFile(id, inputStream)
      val (pxmlFile, maybePngFile) = PNDBinaryInfo.default.findPxmlAndPng(id, file)

      maybePngFile.map(loadAndResizePngFile(_, 64, 64))

      //Then, try to parse the PXML file, and if it succeeds, repack it
      val xml = cleanXMLFileAndLoad(pxmlFile)

      //Now load the metadata in the PXML
      val pxml = makePxml(xml)

      //If it loaded successfully, we can add it to the database
      val namePart = if(filename contains ".") filename.split('.').toSeq.dropRight(1).mkString else filename
      val fileName = ((namePart take 508) + ".pnd")

      transaction {
        val pkg = Database.packages.insert(
          Package(
            user = user,
            fileId = id,
            fileName = fileName,
            uploadTime = new java.util.Date,
            hasImage = maybePngFile.isDefined
          )
        )

        for(application <- pxml.applications) addApplicationToPackage(pkg, application)
        PackageManagerImpl.this.info("A package was successfully added, id=" + pkg.id)

        packageAddedCallbacks.foreach(_(pkg))
      }
      sendMessage(ProcessNotifier.PackageAdded)
    } catch {
      case err: ProcessNotifier.ErrorMessage =>
        deleteAllFiles(id)
        sendMessage(err)
      case err =>
        deleteAllFiles(id)
        PackageManagerImpl.this.info("Unknown error, error="+ err.toString + " stacktrace=" + err.getStackTraceString)
        throw err
    }
  }

  def addCategoryToApplication(app: Application, category: pnd.Category) = {
    val name = category.name.toLowerCase
    Database.categories.insert(
      Category(
        application = app,
        category = DotDesktopCategories.values.find(_.toString.toLowerCase == name).get
      )
    )
  }

  def addApplicationToPackage(pkg: Package, application: pnd.Application)(implicit pn: ProcessNotifier, user: User, filename: String) =
    try {
      val an = application.author.flatMap(_.name.map(_ take 512))
      val app = Database.applications.insert(
        Application(
          `package` = pkg,
          pxmlId = application.id,
          versionMajor = application.version.major,
          versionMinor = application.version.minor,
          versionRelease = application.version.release,
          versionBuild = application.version.build,
          authorName = an
        )
      )

      for(category <- application.categories) quell(addCategoryToApplication(app, category))

      val titles = application.titles.groupBy(_.lang).toMap
      val descriptions = application.descriptions.groupBy(_.lang).toMap
      val pairs = (for{
          lang <- titles.keySet
          if descriptions contains lang
        } yield lang -> (titles(lang).head, descriptions(lang).head)).toMap

      for(lang <- pairs.keySet; locs = pairs(lang)) try {
        Database.appMetas.insert(
          AppMeta(
            application = app,
            language = lang,
            title = (locs._1.text take 512),
            description = (locs._2.text take 2048)
          )
        )
      } catch {
        case err =>
          sendMessage(ProcessNotifier.PxmlSyntaxWarning(err.getMessage))
          err.printStackTrace
      }
    } catch { case err => sendMessage(ProcessNotifier.PxmlSyntaxWarning(err.getMessage))}


  def createUID() = Helpers.randomString(16)

  def quell[T](action: => T): Unit = try action catch {
    case x: scala.util.control.ControlThrowable => throw x
    case _ =>
  }

  def cleanXMLFileAndLoad(xmlFile: File): Elem = {
    //Load the XML file
    val messedUpXml = loadXml(xmlFile)
    //Reformat the XML so that it's laid out properly
    val xml = cleanXml(messedUpXml)
    //Overwrite the old XML file
    saveXml(xml, xmlFile)
    xml
  }

  def loadXml(file: File) = try XML loadFile file catch {
    case ex: SAXException =>
      throw ProcessNotifier.XmlSyntaxError(Option(ex.getMessage) getOrElse "")
  }

  def saveXml(xml: Elem, file: File) =
    XML.save(file.getAbsolutePath, xml, "UTF-8", true)

  def cleanXml(xml: Elem) =
    XML.loadString(new PrettyPrinter(80, 4).format(xml))

  def deleteFile(id: String, kind: FileType)(implicit fs: Filesystem) =
    quell(fs.getFile(id, kind).delete())

  def deleteAllFiles(id: String)(implicit fs: Filesystem) = {
    deleteFile(id, PNDFile )
    deleteFile(id, PXMLFile)
    deleteFile(id, PNGImage)
  }

  def sendMessage(message: ProcessNotifier.Message)(implicit pn: ProcessNotifier, user: User, filename: String) =
    pn.sendResolvedMessage(message, user, filename)

  def savePNDFile(id: String, stream: InputStream)(implicit fs: Filesystem) = {
    val file = fs.getFile(id, PNDFile)
    saveFile(file, stream)
  }

  def saveFile(file: File, stream: InputStream): File = {
    require(!file.exists, "We tried to overwrite a file!")

    file.createNewFile()

    val inChannel = Channels.newChannel(stream)
    val outChannel = new FileOutputStream(file).getChannel
    try outChannel.transferFrom(inChannel, 0, Int.MaxValue) finally {
      outChannel.close()
      inChannel.close()
    }
    file
  }

  def makePxml(xml: Elem)(implicit locale: Locale, localization: Localization): PXML = try new PXML(xml) catch {
    case err: RequirementException => throw ProcessNotifier.PxmlSyntaxError(Option(err.getMessage) getOrElse "")
  }

  def loadAndResizePngFile(pngFile: File, maxWidth: Int, maxHeight: Int) =
    try {
      val image = ImageIO.read(pngFile)
      val resizedImage = resizeImage(image, maxWidth, maxHeight)
      ImageIO.write(resizedImage, "png", pngFile)
    } catch {
      case _ => throw ProcessNotifier.PngInvalidError
    }

  def resizeImage(image: BufferedImage, maxWidth: Int, maxHeight: Int) = {
    val factor = minNumber(maxWidth / image.getWidth.toFloat, maxHeight / image.getHeight.toFloat)
    val width = (image.getWidth * factor).toInt
    val height = (image.getHeight * factor).toInt
    val result = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val transform = AffineTransform.getScaleInstance(factor, factor)
    val g = result.createGraphics
    g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
    g.drawRenderedImage(image, transform)
    g.dispose()
    result
  }
}
