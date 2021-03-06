package org.openpandora.box.snippet

import net.liftweb.common.Logger
import net.liftweb.http.DispatchSnippet
import org.openpandora.box.model.Database
import org.openpandora.box.util.DotDesktopCategories
import org.squeryl.PrimitiveTypeMode._
import scala.xml.NodeSeq
import scala.xml.Text

class ApplicationStatistics extends DispatchSnippet
                               with Logger {
  def dispatch = {
    case "uploadHistory" => uploadHistory
    case "categoryCloud" => categoryCloud
    case "searchCloud"   => searchCloud
  }

  def uploadHistory(downloadHistory: NodeSeq): NodeSeq = {
    val uploads: Seq[Long] =
      from(Database.packages){pkg => select(pkg.uploadTime) orderBy(pkg.uploadTime asc)}.toSeq

    val uploadsCurrent: Long =
      from(Database.packages){pkg =>compute(count)}

    val uploadsCurrentEntry = ((new java.util.Date).getTime.toLong, uploadsCurrent.toInt - 1)

    Text((uploads.zipWithIndex :+ uploadsCurrentEntry) map ({
          case (time, uploads) =>
            "[" + (time + 1).toString + "," + uploads.toString + "]"
        }) mkString("[", ",", "]"))
  }

  def categoryCloud(categoryCloud: NodeSeq): NodeSeq = {
    val counts: Map[Int, Long] = from(Database.categories)(category => groupBy(category.value) compute(count)).map(x => x.key -> x.measures)(collection.breakOut)
    val min = if(counts.isEmpty) 0.0 else counts.values.min.toDouble
    val max = if(counts.isEmpty) 0.0 else counts.values.max.toDouble
    counts.keys.flatMap {category =>
      val catName = DotDesktopCategories(category).toString
      <a href={"/applications/list?search=category:" + catName} class="category-cloud-element" style={"font-size:" + ((counts(category) - min) / (max - min) + 0.5) + "em;"}>{catName}</a> ++ Text(" ")
    }.toSeq
  }

  def searchCloud(searchCloud: NodeSeq): NodeSeq = {
    val counts: Map[String, Long] = from(Database.searchKeywords)(keyword => groupBy(keyword.keyword) compute(count)).map(x => x.key -> x.measures)(collection.breakOut)
    val min = if(counts.isEmpty) 0.0 else counts.values.min.toDouble
    val max = if(counts.isEmpty) 0.0 else counts.values.max.toDouble
    counts.keys.flatMap {keyword =>
      <a href={"/applications/list?search=" + keyword} class="keyword-cloud-element" style={"font-size:" + ((counts(keyword) - min) / (max - min) + 0.5) + "em;"}>{keyword}</a> ++ Text(" ")
    }.toSeq
  }
}
