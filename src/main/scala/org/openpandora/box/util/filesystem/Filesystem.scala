package org.openpandora.box.util.filesystem

import java.io.File
import net.liftweb.util.Props

object Filesystem {
  val default: Filesystem = new FilesystemImpl
}

trait Filesystem {
  def getFile(id: String, kind: FileType): File
}

private[filesystem] class FilesystemImpl extends Filesystem {
  val directory = Props.get("filesystem.dir") map (x => new File(x)) getOrElse
    new File(System.getProperty("java.io.tmpdir"), "box")

  def getFile(id: String, kind: FileType) = {
    if(!directory.exists)
      directory.mkdir()

    val subdir = new File(directory, kind.folder)
    if(!subdir.exists)
      subdir.mkdir()
    
    new File(subdir, id + "." + kind.extension)
  }
}

trait FileType {
  val folder: String
  val extension: String
  val mimeType: String
}

object PNGImage extends FileType {
  val folder = "image"
  val extension = "png"
  val mimeType = "image/png"
}

object PNDFile extends FileType {
  val folder = "package"
  val extension = "pnd"
  val mimeType = "application/x-pandora-pnd"
}

object PXMLFile extends FileType {
  val folder = "metadata"
  val extension = "xml"
  val mimeType = "application/pxml+xml"
}
