package org.openpandora.box.dispatch

import java.io._
import net.liftweb.common._
import net.liftweb.http._
import org.openpandora.box.model._
import org.openpandora.box.util.filesystem._
import org.squeryl.PrimitiveTypeMode._

object FileDispatcher {
  private def serve(file: File, mime: String, name: Option[String] = None) = () => {
    val stream = new BufferedInputStream(new FileInputStream(file))
    val maybeDisposition = name map (x => "Content-Disposition" -> ("attachment; filename=" + x))
    Full(new StreamingResponse(stream, stream.close, file.length, List("Content-Type" -> mime,
                                                                       "Cache-Control" -> ("max-age=" + Int.MaxValue)) ++ maybeDisposition, Nil, 200))
  }

  def servePackage(id: String) = inTransaction{
    from(Database.packages)(pkg => where(pkg.fileId === id) select(pkg)).headOption match {
      case Some(pkg) =>
        val name = pkg.fileName
        User.currentUser match {
          case Some(user) =>
            Database.packageDownloads.insert(PackageDownload(`package` = pkg, user = user))
          case None =>
            Database.packageDownloads.insert(PackageDownload(`package` = pkg))
        }

        val file = Filesystem.getFile(id, PNDFile)
        serve(file, PNDFile.mimeType, Some(name))
      case _ =>
        () => Full(NotFoundResponse("The specified package has been deleted by the user."))
    }
  }

  val dispatch: PartialFunction[Req, () => Box[LiftResponse]] = {
    case Req("file" :: PNDFile.folder :: id :: Nil, PNDFile.extension, _) if Filesystem.getFile(id, PNDFile).exists =>
      servePackage(id)
    case Req("file" :: PNGImage.folder :: id :: Nil, PNGImage.extension, _) if Filesystem.getFile(id, PNGImage).exists =>
      val file = Filesystem.getFile(id, PNGImage)
      serve(file, PNGImage.mimeType)
    case Req("file" :: PXMLFile.folder :: id :: Nil, PXMLFile.extension, _) if Filesystem.getFile(id, PXMLFile).exists =>
      val file = Filesystem.getFile(id, PXMLFile)
      serve(file, PXMLFile.mimeType)
    case Req("file" :: t :: id :: Nil, ext, slash) =>
      () => Full(NotFoundResponse("Unknown file with id " + id + " and extension " + ext + " in data storage named " + t))
  }
}
