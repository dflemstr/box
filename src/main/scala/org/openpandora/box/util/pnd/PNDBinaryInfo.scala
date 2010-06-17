package org.openpandora.box.util.pnd

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStream
import java.io.RandomAccessFile
import java.nio.ByteBuffer
import java.nio.channels.Channels
import java.nio.channels.FileChannel
import java.nio.channels.ReadableByteChannel
import java.nio.channels.WritableByteChannel
import org.openpandora.box.util.filesystem._
import org.openpandora.box.util.packages.ProcessNotifier
import scala.math._
import scala.xml.NodeSeq
import scala.xml.XML

object PNDBinaryInfo {
  val default: PNDBinaryInfo = new PNDBinaryInfoImpl
}

trait PNDBinaryInfo {
  def findPxmlAndPng(id: String, file: File)(implicit fs: Filesystem): (File, Option[File])
  def loadPxmlAndPng(file: File): (NodeSeq, Option[Array[Byte]])
}

private[pnd] class PNDBinaryInfoImpl extends PNDBinaryInfo {
  val PxmlHeader = "<PXML".getBytes("UTF-8")
  val PxmlFooter = "</PXML>".getBytes("UTF-8")
  val PngHeader  = Array(137, 80, 78, 71, 13, 10, 26, 10).map(_.toByte)
  val PxmlHeaderKmpFailure = computeFailure(PxmlHeader)
  val PxmlFooterKmpFailure = computeFailure(PxmlFooter)
  val PngHeaderKmpFailure  = computeFailure(PngHeader)

  private def computeFailure(pattern: Array[Byte]): Array[Int] = {
    val failure = new Array[Int](pattern.length)

    var j = 0
    var i = 1
    while(i < pattern.length) {
      while(j > 0 && pattern(j) != pattern(i)) {
        j = failure(j - 1)
      }
      if(pattern(j) == pattern(i))
        j += 1
      failure(i) = j
      i += 1
    }
    failure
  }

  def makeChannel(file: File): FileChannel = new RandomAccessFile(file, "r").getChannel

  def findPxmlAndPng(id: String, file: File)(implicit fs: Filesystem) = {
    val channel = makeChannel(file)
    try {
      val pxmlBeginning = findPxmlBeginning(channel)
      val pxml = fs.getFile(id, PXMLFile)
      val png = fs.getFile(id, PNGImage)
      val pxmlOutputStream = new FileOutputStream(pxml)
      val pngOutputStream = new FileOutputStream(png)
      extractPxmlAndPng(channel, pxmlBeginning, pxmlOutputStream, pngOutputStream) match {
        case true => (pxml, Some(png))
        case false => (pxml, None)
      }
    } finally channel.close()
  }

  def loadPxmlAndPng(file: File): (NodeSeq, Option[Array[Byte]]) = {
    val channel = makeChannel(file)
    try {
      val pxmlBeginning = findPxmlBeginning(channel)
      val pxmlStream = new ByteArrayOutputStream()
      val pngStream = new ByteArrayOutputStream()
      val png = if(extractPxmlAndPng(channel, pxmlBeginning, pxmlStream, pngStream)) Some(pngStream.toByteArray) else None
      val pxmlString = new String(pxmlStream.toByteArray, "UTF-8")
      val pxml = XML.loadString(pxmlString)
      (pxml, png)
    } finally channel.close()
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

  def findPxmlBeginning(channel: FileChannel): Long = {
    //Do some precomutations:
    val buffer = ByteBuffer.allocateDirect(PndPxmlWindowSize)
    val pxmlPattern = PxmlHeader.toArray
    val pxmlFailure = PxmlHeaderKmpFailure.toArray
    var i = 1

    val pxmlBegin: Long = if(channel.size < PndPxmlWindowSize) {
      //The whole file fits in the window, act accordingly
      channel.position(0)
      channel.read(buffer)
      indexOf(buffer, 0, channel.size.toInt, pxmlPattern, pxmlFailure)
    } else {
      var result: Long = -1
      while(result < 0 && i < MaxChunks) {
        val offsetFromEnd = (PndPxmlWindowSize - PxmlHeader.length) * i + PxmlHeader.length
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

  def transfer(from: ReadableByteChannel, to: WritableByteChannel, count: Long): Unit = {
    val bb = ByteBuffer.allocateDirect(4096)
    var remaining = count
    while(remaining > 0 && bb.hasRemaining) {
      bb.limit(min(remaining, bb.capacity).toInt)
      from.read(bb)
      bb.flip()
      remaining -= to.write(bb)
      bb.compact()
    }
  }

  def extractPxmlAndPng(channel: FileChannel, start: Long, pxmlStream: OutputStream, pngStream: OutputStream): Boolean = {
    channel.position(start)
    val len = (channel.size - start).toInt
    //TODO: don't load everything at once!
    val buffer = ByteBuffer.allocateDirect(len)

    channel.read(buffer)

    //Try to find the end of the PXML file
    val pxmlLength = indexOf(buffer, 0, len,
                             PxmlFooter,
                             PxmlFooterKmpFailure) + PxmlFooter.length

    if(pxmlLength < PxmlFooter.length)
      throw ProcessNotifier.PxmlTruncatedError

    val pngStart = indexOf(buffer, pxmlLength, len - pxmlLength,
                           PngHeader,
                           PngHeaderKmpFailure)

    //Write PXML file
    val pxmlChannel = Channels.newChannel(pxmlStream)
    try {
      channel.position(start)
      transfer(channel, pxmlChannel, pxmlLength)
    } finally pxmlChannel.close()

    //If we have a PNG image, write it too
    if(pngStart > 0) {
      val pngChannel = Channels.newChannel(pngStream)
      try {
        channel.position(start + pngStart)
        transfer(channel, pngChannel, len - pngStart)
      } finally pngChannel.close()
    }

    (pngStart > 0)
  }
}
