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
import org.openpandora.box.util.filesystem.FileType
import org.openpandora.box.util.filesystem.Filesystem
import org.openpandora.box.util.filesystem.PNDFile
import org.openpandora.box.util.filesystem.PNGImage
import org.openpandora.box.util.filesystem.PXMLFile
import org.openpandora.box.util.pnd.PNDBinaryInfo
import org.openpandora.box.util.pnd.PXML
import org.openpandora.box.util.pnd.RequirementException
import org.squeryl.PrimitiveTypeMode._
import org.xml.sax.SAXException
import scala.actors.Actor
import scala.actors.Actor._
import scala.xml.PrettyPrinter
import scala.xml.XML

object PackageManager extends Actor
                         with Logger {
  case class MakePackageFromStream(filename: String, input: InputStream, user: User)

  def act = loop {
    react {
      case MakePackageFromStream(filename, stream, user) =>
        new PackageAdder(filename, stream, user).start()
    }
  }

  private class PackageAdder(filename: String, inputStream: InputStream, user: User) extends Actor {
    def act: Unit = transaction {
      import ProcessNotifier._

      val name = Helpers.randomString(16)

      def silent(action: => Unit) = try action catch {case _ =>}

      def deleteAll() = {
        def delFile(name: String, kind: FileType) = silent(Filesystem.getFile(name, kind).delete())
        delFile(name, PNDFile )
        delFile(name, PXMLFile)
        delFile(name, PNGImage)
      }

      def sendMessage(message: ProcessNotifier.Message) =
        ProcessNotifier! SendResolvedMessage(message, user, filename)

      try {
        //Try to save the file
        val file = Filesystem.getFile(name, PNDFile)
        val channel = makeChannel(makeFile(file, inputStream))
        val (pxmlFile, maybePngFile) = try {
          val pxmlBeginning = findPxmlBeginning(channel)
          extractPxmlAndPng(name, channel, pxmlBeginning)
        } finally channel.close()

        //Then, try to parse the PXML file, and if it succeeds, repack it
        val messedUpXml = try XML.loadFile(pxmlFile) catch {
          case ex: SAXException =>
            throw XmlSyntaxError(Option(ex.getMessage) getOrElse "")
        }
        val xml = XML.loadString(new PrettyPrinter(80, 4).format(messedUpXml))
        XML.save(pxmlFile.getAbsolutePath, xml, "UTF-8", true)

        //Now load the metadata in the PXML
        val pxml = try new PXML(xml) catch {
          case err: RequirementException => throw PxmlSyntaxError(Option(err.getMessage) getOrElse "")
        }

        //If it loaded successfully, we can add it to the database
        val namePart = filename.split('.').toSeq.dropRight(1).mkString
        val fileName = (namePart.substring(0, Math.min(namePart.length, 60)) + ".pnd")

        val pkg = Database.packages.insert(
          Package(
            user = user,
            fileId = name,
            fileName = fileName,
            uploadTime = new java.util.Date,
            hasImage = maybePngFile.isDefined
          )
        )

        for(application <- pxml.applications) try {
          val an = application.author.map {a =>
            val name = a.name
            name.substring(0, Math.min(60, name.length))
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

          for {
            category <- application.categories
            name = category.name.toLowerCase
            if DotDesktopCategories.values.exists(_.toString.toLowerCase == name)
          } silent {
            Database.categories.insert(
              Category(
                application = app,
                category = DotDesktopCategories.values.find(_.toString.toLowerCase == name).get
              )
            )
          }

          val titles = application.titles.groupBy(_.lang).toMap
          val descriptions = application.descriptions.groupBy(_.lang).toMap
          val pairs = (for{
              lang <- titles.keySet
              if descriptions contains lang
            } yield lang -> (titles(lang).head, descriptions(lang).head)).toMap

          for(lang <- pairs.keySet; locs = pairs(lang)) try {
            PackageManager.this.info("Adding an appMeta, application=" + app + " language=" + lang)
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
              sendMessage(PxmlSyntaxWarning(err.getMessage))
              err.printStackTrace
          }
        } catch { case err => sendMessage(PxmlSyntaxWarning(err.getMessage))}

        PackageManager.this.info("A package was successfully added, id=" + pkg.id)
        sendMessage(PackageAdded())
      } catch {
        case err: ErrorMessage =>
          deleteAll()
          sendMessage(err)
        case err =>
          deleteAll()
          PackageManager.this.info("Unknown error, error="+ err.toString + " stacktrace=" + err.getStackTraceString)
          throw err
      }
    }

    private def resizeImage(image: BufferedImage, maxWidth: Int, maxHeight: Int) = {
      val factor = Math.min(maxWidth / image.getWidth.toFloat, maxHeight / image.getHeight.toFloat)
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

    private def makeFile(file: File, input: InputStream): File = {
      require(!file.exists, "We tried to overwrite a PND file!")

      file.createNewFile()

      val inChannel = Channels.newChannel(input)
      val outChannel = new FileOutputStream(file).getChannel
      try outChannel.transferFrom(inChannel, 0, Int.MaxValue) finally {
        outChannel.close()
        inChannel.close()
      }

      file
    }

    private val PndPxmlWindowSize = 65536
    private val MaxChunks = 16

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

    private def makeChannel(file: File): FileChannel = new RandomAccessFile(file, "r").getChannel

    private def findPxmlBeginning(channel: FileChannel): Long = {
      //Do some precomutations:
      val buffer = ByteBuffer.allocateDirect(PndPxmlWindowSize)
      val pxmlPattern = PNDBinaryInfo.PxmlHeader.toArray
      val pxmlFailure = PNDBinaryInfo.PxmlHeaderKmpFailure.toArray
      var i = 1

      val pxmlBegin: Long = if(channel.size < PndPxmlWindowSize) {
        //The whole file fits in the window, act accordingly
        channel.position(0)
        channel.read(buffer)
        val tmp = indexOf(buffer, 0, channel.size.toInt, pxmlPattern, pxmlFailure)
        if(tmp < 0)
          throw ProcessNotifier.PxmlNotFoundError
        tmp
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

    private def extractPxmlAndPng(name: String, channel: FileChannel, start: Long) = {
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
      val pxmlFile = Filesystem.getFile(name, PXMLFile)
      val pxmlChannel = new FileOutputStream(pxmlFile).getChannel
      try {
        channel.position(start)
        pxmlChannel.transferFrom(channel, 0, pxmlLength)
      } finally pxmlChannel.close()

      //If we have a PNG image, write it too
      val maybePngFile = if(pngStart > 0) {
        val pngFile = Filesystem.getFile(name, PNGImage)
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
}
