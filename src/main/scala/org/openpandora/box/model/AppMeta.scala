package org.openpandora.box.model

import org.squeryl.dsl.ManyToOne
import java.util.Locale
import org.openpandora.box.util.Languages
import org.squeryl.KeyedEntity
import org.squeryl.dsl.CompositeKey2
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.annotations.Column
import scala.annotation.target.field


case class AppMeta(applicationId:    Long,   //id
                   @Column(length = 6)
                   languageName:     String, //lang
                   @Column(length = 512)
                   title:            String,
                   @Column(length = 2048)
                   description:      String) extends KeyedEntity[CompositeKey2[Long, String]] {
  def id = compositeKey(applicationId, languageName)
  lazy val application: ManyToOne[Application] = Database.applicationsToAppMetas.right(this)
  def language = Languages.locales.find(_.toString.toLowerCase == languageName.toLowerCase) getOrElse Locale.getDefault
}

object AppMeta {
  def apply(application: Application, language: Locale, title: String, description: String): AppMeta = {
    AppMeta(application.id, language.toString, title, description)
  }
}