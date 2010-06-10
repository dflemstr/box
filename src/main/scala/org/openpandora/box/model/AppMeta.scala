package org.openpandora.box.model

import org.squeryl.dsl.ManyToOne
import java.util.Locale
import org.openpandora.box.util.Languages
import org.squeryl.annotations._
import scala.annotation.target.field


case class AppMeta(applicationId:    Long,   //id
                   @(Column @field)(length = 6)
                   languageName:     String, //lang
                   @(Column @field)(length = 64)
                   title:            String,
                   @(Column @field)(length = 2048)
                   description:      String) extends LongKeyedEntity {
  lazy val application: ManyToOne[Application] = Database.applicationsToAppMetas.right(this)
  def language = Languages.locales.find(_.toString.toLowerCase == languageName.toLowerCase) getOrElse Locale.getDefault
}

object AppMeta {
  def apply(application: Application, language: Locale, title: String, description: String): AppMeta = {
    AppMeta(application.id, language.toString, title, description)
  }
}