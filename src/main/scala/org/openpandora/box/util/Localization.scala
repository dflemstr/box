package org.openpandora.box.util

import java.util.Locale
import java.util.ResourceBundle
import net.liftweb.common.Logger
import net.liftweb.http.LiftRules
import net.liftweb.util.Helpers
import net.liftweb.util.NamedPF

object Localization extends Logger {
  private def resourceBundles(locale: Locale) =
    LiftRules.resourceNames.flatMap {name =>
      Helpers tryo {
        List(ResourceBundle.getBundle(name, locale))
      } openOr {
        NamedPF.applyBox((name, locale), LiftRules.resourceBundleFactories.toList).map(List(_)) openOr Nil
      }
    }

  private def resourceBundles =
    LiftRules.resourceNames.flatMap {name =>
      Helpers tryo {
        List(ResourceBundle.getBundle(name))
      } openOr Nil
    }

  def loc(key: String, locale: Locale): String = resourceBundles(locale).flatMap(r => Helpers.tryo(r.getObject(key) match {
        case s: String => Some(s)
        case _ => None
      }).flatten).headOption getOrElse {
    warn("Couldn't translate key, key=" + key)
    key
  }

  def loc(key: String): String = resourceBundles.flatMap(r => Helpers.tryo(r.getObject(key) match {
        case s: String => Some(s)
        case _ => None
      }).flatten).headOption getOrElse {
    warn("Couldn't translate key, key=" + key)
    key
  }
}
