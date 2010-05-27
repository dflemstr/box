package org.openpandora.box.model

import org.squeryl.dsl.ManyToOne
import java.util.Locale
import org.squeryl.annotations._


case class AppMeta(applicationId:    Long,   //id
                   @Column(length = 6)
                   languageName:         String, //lang
                   @Column(length = 64)
                   title:            String,
                   @Column(length = 2048)
                   description:      String) extends LongKeyedEntity {
  lazy val application: ManyToOne[Application] = Database.applicationsToAppMetas.right(this)
  def language = new Locale(languageName)
}

object AppMeta {
  def apply(application: Application, language: Locale, title: String, description: String): AppMeta = {
    require(title.length <= 64, "Title too long")
    require(description.length <= 2048, "Description too long")
    AppMeta(application.id, language.toString, title, description)
  }
}