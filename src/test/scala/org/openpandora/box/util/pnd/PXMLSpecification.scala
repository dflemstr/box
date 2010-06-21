package org.openpandora.box.util.pnd

import org.specs._

object PXMLSpecification extends Specification("PXML-checks and PXML-DOM specification") {
  implicit val localization = new org.openpandora.box.util.Localization {
    def loc(key: String) = key
    def loc(key: String, locale: java.util.Locale) = "(" + locale + ")" + key
  }
  implicit val locale = java.util.Locale.ENGLISH

  def expose = addToSusVerb("expose")

  "The PXML DOM representation" should expose {
    "the <PXML> tag, taking care to" >> {
      "detect PXML application declarations of version" >> {
        "1.0" in {
          val pxml = new PXML(pxmlArchetypeV1ex1)
          pxml.applications must haveSize(1)
        }
        "2.0" in {
          val pxml1 = new PXML(pxmlArchetypeV2ex1)
          pxml1.applications must haveSize(1)
          val pxml2 = new PXML(pxmlArchetypeV2ex2)
          pxml2.applications must haveSize(2)
        }
      }
      "fail on empty input" in {
        (new PXML(<PXML></PXML>)) must throwA[RequirementException]
      }
    }
    "the <application> tag, taking care to" >> {
      "check whether it reproduces ids correctly" in {
        val ids = Seq.fill(100)("foobar" + scala.util.Random.nextLong)
        ids.foreach {id =>
          val appXML = <application id={id}>{applicationMinimalBody}</application>
          val application = new Application(appXML)
          application.id must_== id
        }
      }
    }
  }

  lazy val applicationMinimalBody: scala.xml.NodeSeq =
    ((<title lang="en_US">foo</title>) ++
     (<description lang="en_US">foo</description>) ++
     (<version major="0" minor="0" release="0" build="0"/>) ++
     (<categories><category name="Game"/></categories>))

  lazy val pxmlArchetypeV2ex1 =
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

  lazy val pxmlArchetypeV2ex2 =
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
        <application id="amarok" appdata="amarok">
          <title lang="en_US">Amarok music player</title>
          <title lang="sv">Amarok musikspelare</title>
          <title lang="de">Amarok musikspieler</title>
          <description lang="en_US">A versatile music player written in C++.</description>
          <description lang="sv">En kraftfull musikspelare skriven i C++.</description>
          <description lang="de">Ein fähiger Musikspieler, in C++ geschrieben.</description>
          <version major="4" minor="4" release="2" build="105"/>
          <exec command="./bin/amarok" background="true" standalone="true"/>
          <author name="Bjornhild Andersson"/>
          <icon src="icon.png"/>
          <categories>
            <category name="Audio"/>
            <category name="Music"/>
            <category name="Qt"/>
          </categories>
        </application>
     </PXML>)

  lazy val pxmlArchetypeV1ex1 =
    (<PXML xmlns="http://openpandora.org/namespaces/PXML" id="exaile">
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
     </PXML>)
}
