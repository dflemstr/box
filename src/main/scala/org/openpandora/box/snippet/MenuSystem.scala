package org.openpandora.box.snippet

import net.liftweb.http.DispatchSnippet
import net.liftweb.http.LiftRules
import net.liftweb.http.S
import net.liftweb.sitemap.MenuItem
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq

class MenuSystem extends DispatchSnippet {
  def dispatch = {
    case "build" => build
  }

  def build(build: NodeSeq): NodeSeq = {
    val pathOnly = S.attr("pathOnly").isDefined
    val toRender: Seq[MenuItem] = for {
      sitemap <- LiftRules.siteMap.toSeq
      req <- S.request.toSeq
      line <- if(pathOnly) req.buildMenu.lines else LiftRules.siteMap.toSeq.flatMap(_.buildMenu(req.location).lines)
    } yield line

    toRender match {
      case Seq() => NodeSeq.Empty
      case menuItems =>
        val pathAttrs = S.prefixedAttrsToMap("path")
        val activeAttrs = S.prefixedAttrsToMap("active")
        def makeMenuItem(menuItems: Seq[MenuItem])(template: NodeSeq): NodeSeq = menuItems.flatMap { menuItem =>
          def makeBind(leaf: NodeSeq, items: Seq[MenuItem]) =
            bind("item", template,
                 "menu" -> leaf,
                 "items" -> makeMenuItems(items) _)

          menuItem match {
            case m @ MenuItem(text, _, kids, _, _, _) if m.placeholder_? =>
              makeBind(<span>{text}</span>, kids)
            case MenuItem(text, uri, kids, true, _, _) =>
              makeBind(<a href={uri}>{text}</a> % S.prefixedAttrsToMetaData("active", activeAttrs), kids)
            case MenuItem(text, uri, kids, _, true, _) =>
              makeBind(<a href={uri}>{text}</a> % S.prefixedAttrsToMetaData("path", pathAttrs), kids)
            case MenuItem(text, uri, kids, _, _, _) =>
              makeBind(<a href={uri}>{text}</a>, kids)
          }
        }

        def makeMenuItems(menuItems: Seq[MenuItem])(template: NodeSeq): NodeSeq = if(menuItems.isEmpty)
          NodeSeq.Empty
        else
          bind("items", template, "item" -> makeMenuItem(menuItems) _)

        bind("build", build,
             "items" -> makeMenuItems(menuItems) _)
    }
  }
}
