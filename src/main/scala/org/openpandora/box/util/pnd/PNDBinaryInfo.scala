package org.openpandora.box.util.pnd

object PNDBinaryInfo {
  val PxmlHeader: Seq[Byte] = "<PXML".getBytes("UTF-8")
  val PxmlFooter: Seq[Byte] = "</PXML>".getBytes("UTF-8")
  val PngHeader:  Seq[Byte] = Seq(137, 80, 78, 71, 13, 10, 26, 10).map(_.toByte)
  val PxmlHeaderKmpFailure: Seq[Int] = computeFailure(PxmlHeader)
  val PxmlFooterKmpFailure: Seq[Int] = computeFailure(PxmlFooter)
  val PngHeaderKmpFailure:  Seq[Int] = computeFailure(PngHeader)

  private def computeFailure(pattern: Seq[Byte]): Seq[Int] = {
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
}
