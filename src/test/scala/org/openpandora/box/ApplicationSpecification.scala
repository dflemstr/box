package org.openpandora.box

import org.specs._

object ApplicationSpecification extends Specification("Web application specification") {
  description = "A specification for a web application"

  "The web application" should {
    "only contain XHTML files which are valid" in {
      def isXHtmlFile(file: String) = file.endsWith(".html") || file.endsWith(".htm") || file.endsWith(".xhtml")

      def isReallyXHtmlFile(file: java.io.File) = {
        val parsedResult = net.liftweb.util.PCDataXmlParser(new java.io.FileInputStream(file.getAbsolutePath))
        parsedResult.toOption.isDefined
      }

      def wellFormed(file: java.io.File) {
        if(file.isDirectory)
          file.listFiles.map(wellFormed)

        if(file.isFile && isXHtmlFile(file.getName))
          file verifies (isReallyXHtmlFile)
      }

      wellFormed(new java.io.File("src/main/webapp"))
    }

    "only contain XML files which are valid" in {
      def isXmlFile(file: String) = file.endsWith(".xml")
      
      def isReallyXmlFile(file: java.io.File) =
        try {
          scala.xml.XML.loadFile(file) != null
        } catch {
          case ex: org.xml.sax.SAXParseException => println(ex.getMessage); false
        }
      
      def wellFormed(file: java.io.File) {
        if(file.isDirectory)
          file.listFiles.map(wellFormed)

        if(file.isFile && isXmlFile(file.getName))
          file verifies (isReallyXmlFile)
      }

      wellFormed(new java.io.File("src/main/webapp"))
    }
  }
}
