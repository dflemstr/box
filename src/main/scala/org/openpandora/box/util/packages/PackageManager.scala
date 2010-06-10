package org.openpandora.box.util.packages

import java.awt.RenderingHints
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.io.File
import java.io.FileOutputStream
import java.io.InputStream
import java.io.RandomAccessFile
import java.nio.ByteBuffer
import java.nio.channels.Channels
import java.nio.channels.FileChannel
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
      val (pxmlFile, maybePngFile) = findPxmlAndPng(id, file)

      //Then, try to parse the PXML file, and if it succeeds, repack it
      val xml = cleanXMLFileAndLoad(pxmlFile)

      //Now load the metadata in the PXML
      val pxml = makePxml(xml)

      //If it loaded successfully, we can add it to the database
      val namePart = filename.split('.').toSeq.dropRight(1).mkString
      val fileName = (namePart.substring(0, minNumber(namePart.length, 60)) + ".pnd")

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
      val an = application.author.map {a =>
        val name = a.name
        name.substring(0, minNumber(60, name.length))
      }
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
            title = (if(locs._1.text.length > 64) locs._1.text.substring(0, 60) + "..." else locs._1.text),
            description = (if(locs._2.text.length > 2048) locs._2.text.substring(0, 2044) + "..." else locs._2.text)
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

  def findPxmlAndPng(id: String, file: File)(implicit fs: Filesystem) = {
    val channel = makeChannel(file)
    try {
      val pxmlBeginning = findPxmlBeginning(channel)
      extractPxmlAndPng(id, channel, pxmlBeginning)
    } finally channel.close()
  }

  def makePxml(xml: Elem)(implicit locale: Locale, localization: Localization): PXML = try new PXML(xml) catch {
    case err: RequirementException => throw ProcessNotifier.PxmlSyntaxError(Option(err.getMessage) getOrElse "")
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

  val PndPxmlWindowSize = 65536
  val MaxChunks = 16

  @inline private def indexOf(data: ByteBuffer, start: Int, length: Int,
                              pattern: Array[Byte], failure: Array[Int]) = {
    //Use Knuth-Morris-Pratt to scan the data for a pattern
    //This is a bit overkill but blazingly fast
    var j = 0
    var i = start
    var result = -1
    while(i < start + length && result < 0) {
      while(j > 0 && pattern(j) != data.get(i))
        j = failure(j - 1)

      if(pattern(j) == data.get(i))
        j += 1

      if(j == pattern.length)
        result = i - pattern.length + 1

      i += 1
    }
    result
  }

  def makeChannel(file: File): FileChannel = new RandomAccessFile(file, "r").getChannel

  def findPxmlBeginning(channel: FileChannel): Long = {
    //Do some precomutations:
    val buffer = ByteBuffer.allocateDirect(PndPxmlWindowSize)
    val pxmlPattern = PNDBinaryInfo.PxmlHeader.toArray
    val pxmlFailure = PNDBinaryInfo.PxmlHeaderKmpFailure.toArray
    var i = 1

    val pxmlBegin: Long = if(channel.size < PndPxmlWindowSize) {
      //The whole file fits in the window, act accordingly
      channel.position(0)
      channel.read(buffer)
      indexOf(buffer, 0, channel.size.toInt, pxmlPattern, pxmlFailure)
    } else {
      var result: Long = -1
      while(result < 0 && i < MaxChunks) {
        val offsetFromEnd = (PndPxmlWindowSize - PNDBinaryInfo.PxmlHeader.length) * i + PNDBinaryInfo.PxmlHeader.length
        if(channel.size > offsetFromEnd) {
          val pos = channel.size - offsetFromEnd
          channel.position(pos)
          channel.read(buffer)
          val tmp = indexOf(buffer, 0, PndPxmlWindowSize, pxmlPattern, pxmlFailure)
          if(tmp > 0)
            result = pos + tmp
          i += 1
        }
      }

      if(result < 0)
        throw ProcessNotifier.PxmlNotFoundError
      result
    }

    pxmlBegin
  }

  def extractPxmlAndPng(id: String, channel: FileChannel, start: Long)(implicit fs: Filesystem) = {
    channel.position(start)
    val len = (channel.size - start).toInt
    //TODO: don't load everything at once!
    val buffer = ByteBuffer.allocateDirect(len)

    channel.read(buffer)

    //Try to find the end of the PXML file
    val pxmlLength = indexOf(buffer, 0, len,
                             PNDBinaryInfo.PxmlFooter.toArray,
                             PNDBinaryInfo.PxmlFooterKmpFailure.toArray) + PNDBinaryInfo.PxmlFooter.length

    if(pxmlLength < PNDBinaryInfo.PxmlFooter.length)
      throw ProcessNotifier.PxmlTruncatedError

    val pngStart = indexOf(buffer, pxmlLength, len - pxmlLength,
                           PNDBinaryInfo.PngHeader.toArray,
                           PNDBinaryInfo.PngHeaderKmpFailure.toArray)

    //Write PXML file
    val pxmlFile = fs.getFile(id, PXMLFile)
    val pxmlChannel = new FileOutputStream(pxmlFile).getChannel
    try {
      channel.position(start)
      pxmlChannel.transferFrom(channel, 0, pxmlLength)
    } finally pxmlChannel.close()

    //If we have a PNG image, write it too
    val maybePngFile = if(pngStart > 0) {
      val pngFile = fs.getFile(id, PNGImage)
      val pngChannel = new FileOutputStream(pngFile).getChannel
      try {
        channel.position(start + pngStart)
        pngChannel.transferFrom(channel, 0, len - pngStart)
      } finally pngChannel.close()
      try {
        val image = ImageIO.read(pngFile)
        val resizedImage = resizeImage(image, 64, 64)
        ImageIO.write(resizedImage, "png", pngFile)
      } catch {
        case _ => throw ProcessNotifier.PngInvalidError
      }
      Some(pngFile)
    } else None
    (pxmlFile, maybePngFile)
  }
}
