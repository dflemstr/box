package org.openpandora.box.model

import org.squeryl.KeyedEntity

trait LongKeyedEntity extends KeyedEntity[Long] {
  val id: Long = 0l //Is changed via reflection on demand
}
