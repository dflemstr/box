package org.openpandora.box.util.pnd

import org.specs._

object PndBinaryInfoImplSpecification extends Specification("A specification for PND binary operations") {
  val pxmlArchetype =
    (<PXML xmlns="http://openpandora.org/namespaces/PXML">
        <application id="exaile" appdata="exaile">
          <title lang="en_US">Exaile music player</title>
          <title lang="sv">Exaile musikspelare</title>
          <title lang="de">Exaile musikspieler</title>
          <description lang="en_US">A versatile music player written in Python.</description>
          <description lang="sv">En kraftfull musikspelare skriven i Python.</description>
          <description lang="de">Ein fähiger Musikspieler, in Python geschrieben.</description>
          <version major="1" minor="0" release="0" build="2"/>
          <exec command="./bin/exaile" background="true" standalone="true"/>
          <author name="Bjornhild Andersson" website="http://some.website.with.author.info" email="a.b@c.de"/>
          <icon src="icon.png"/>
          <categories>
            <category name="Audio"/>
            <category name="Music"/>
            <category name="GTK"/>
          </categories>
        </application>
     </PXML>)
  val pxmlArchetypeBytes = pxmlArchetype.toString.getBytes("UTF-8").toSeq

  val pngArchetype = Array(
     137, 80, 78, 71, 13, 10, 26, 10,
     0, 0, 0, 13, 73, 72, 68, 82,
     0, 0, 0, 1, 0, 0, 0, 1,
     8, 4, 0, 0, 0, 181, 28, 12,
     2, 0, 0, 0, 1, 115, 82, 71,
     66, 0, 174, 206, 28, 233, 0, 0,
     0, 11, 73, 68, 65, 84, 8, 29,
     99, 96, 96, 0, 0, 0, 3, 0,
     1, 79, 72, 10, 175, 0, 0, 0,
     0, 73, 69, 78, 68, 174, 66, 96,
     130
  ).map(_.toByte).toSeq

  "The PND binary information implementation" should {
    val file = java.io.File.createTempFile("box-test", ".pnd")
    val binaryInfo: PNDBinaryInfo = new PNDBinaryInfoImpl
    
    doBefore {
      val pxmlData: Array[Byte] = pxmlArchetypeBytes.toArray
      val pngData: Array[Byte] = pngArchetype.toArray
      def randomData(count: Int) = Array.fill(count)((scala.util.Random.nextInt % 256).toByte)
      val pndData: Array[Byte] = randomData(scala.util.Random.nextInt(65536)) ++ pxmlData ++ pngData
      file.createNewFile()
      val fos = new java.io.FileOutputStream(file)
      try fos.write(pndData) finally fos.close()
    }

    "be able to" >> {
      "find PXML and PNG data in a binary file" in {
        val (pxml, png) = binaryInfo.loadPxmlAndPng(file)
        png must beSome[Array[Byte]]
      }

      "load PNG data losslessly" in {
        val (pxml, png) = binaryInfo.loadPxmlAndPng(file)
        png must beSome[Array[Byte]]
        png.get.toSeq must containInOrder(pngArchetype)
      }

      "load PXML data, not necessarily retaining attribute order" in {
        val (pxml, png) = binaryInfo.loadPxmlAndPng(file)
        pxml must ==/(pxmlArchetype)
      }
    }

    doAfter {
      file.delete()
    }
    ()
  }
}
