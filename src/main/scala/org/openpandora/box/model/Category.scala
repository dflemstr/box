package org.openpandora.box.model

import org.openpandora.box.util.DotDesktopCategories
import org.squeryl.dsl.CompositeKey2
import org.squeryl.dsl.ManyToOne
import org.squeryl.KeyedEntity
import org.squeryl.PrimitiveTypeMode._


case class Category(applicationId: Long, //id
                    value:         Int   /*enum*/) extends KeyedEntity[CompositeKey2[Long, Int]] {
  def id = compositeKey(applicationId, value)
  lazy val application: ManyToOne[Application] = Database.applicationsToCategories.right(this)
}

object Category {
  def apply(application: Application, category: DotDesktopCategories.Value): Category =
    Category(application.id, category.id)
}