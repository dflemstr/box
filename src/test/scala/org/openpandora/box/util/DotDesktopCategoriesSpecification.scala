package org.openpandora.box.util

import org.specs._

object DotDesktopCategoriesSpecification extends Specification(".desktop category specification") {
  "The .desktop category enumeration" should {
    "only contain values with correct casing" in {
      val allCapsValues = Seq("GNOME", "GTK", "IDE", "KDE", "OCR", "P2P", "PDA", "TV")
      DotDesktopCategories.values.foreach { value =>
        value.toString must beMatching("^[A-Z0-9][a-zA-Z0-9]*[a-z0-9]$").unless(allCapsValues contains value.toString)
      }
    }
    "not contain duplicates" in {
      DotDesktopCategories.values.foreach { value =>
        val name = value.toString.toLowerCase
        DotDesktopCategories.values.count(_.toString.toLowerCase == name) must be(1)
      }
    }
  }
}
