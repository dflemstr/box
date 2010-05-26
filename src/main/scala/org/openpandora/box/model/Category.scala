package org.openpandora.box.model

import org.openpandora.box.util.DotDesktopCategories
import org.squeryl.dsl.ManyToOne


case class Category(applicationId: Long, //id
                    value:         Int   /*enum*/) extends LongKeyedEntity {
  lazy val application: ManyToOne[Application] = Database.applicationsToCategories.right(this)
}

object Category {
  def apply(application: Application, category: DotDesktopCategories.Value): Category =
    Category(application.id, category.id)
}