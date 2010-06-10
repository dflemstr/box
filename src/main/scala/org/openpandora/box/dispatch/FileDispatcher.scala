package org.openpandora.box.dispatch

import java.io._
import java.util.Date
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import org.openpandora.box.model._
import org.openpandora.box.util.filesystem._
import org.squeryl.PrimitiveTypeMode._

object FileDispatcher {
  val default: FileDispatcher = new FileDispatcherImpl
}

trait FileDispatcher extends Dispatcher

class FileDispatcherImpl(fs: Filesystem = Filesystem.default) extends FileDispatcher {
  private def serve(file: File, mime: String, name: Option[String] = None) = () => {
    val stream = new BufferedInputStream(new FileInputStream(file))
    val maybeDisposition = name map (x => "Content-Disposition" -> ("attachment; filename=" + x))
    val timeInAYear = 365.days.later.getTime
    Full(new StreamingResponse(stream, stream.close, file.length,
                               List("Content-Type" -> mime,
                                    "Cache-Control" -> ("max-age=" + 365*24*60*60),
                                    "Expires" -> toInternetDate(timeInAYear),
                                    "Pragma" -> "")
                               ++ maybeDisposition, Nil, 200))
  }

  def servePackage(id: String)(implicit fs: Filesystem) = {
    from(Database.packages)(pkg => where(pkg.fileId === id) select(pkg)).headOption match {
      case Some(pkg) =>
        val name = pkg.fileName
        User.currentUser match {
          case Some(user) =>
            Database.packageDownloads.insert(PackageDownload(`package` = pkg, user = user))
          case None =>
            Database.packageDownloads.insert(PackageDownload(`package` = pkg))
        }

        val file = fs.getFile(id, PNDFile)
        serve(file, PNDFile.mimeType, Some(name))
      case _ =>
        () => Full(NotFoundResponse(S.?("package.notfound")))
    }
  }

  def dispatch = {
    case Req("file" :: PNDFile.folder :: id :: Nil, PNDFile.extension, _) if fs.getFile(id, PNDFile).exists =>
      servePackage(id)(fs)
    case Req("file" :: PNGImage.folder :: id :: Nil, PNGImage.extension, _) if fs.getFile(id, PNGImage).exists =>
      val file = fs.getFile(id, PNGImage)
      serve(file, PNGImage.mimeType)
    case Req("file" :: PXMLFile.folder :: id :: Nil, PXMLFile.extension, _) if fs.getFile(id, PXMLFile).exists =>
      val file = fs.getFile(id, PXMLFile)
      serve(file, PXMLFile.mimeType)
    case Req("file" :: t :: id :: Nil, ext, slash) =>
      () => Full(NotFoundResponse(S.?("file.notfound").replace("%id%", id).replace("%extension%", ext).replace("%datastorage%", t)))
  }
}
