package org.openpandora.box.snippet

import java.util.Date
import net.liftweb.common.Logger
import net.liftweb.http.DispatchSnippet
import net.liftweb.util.Helpers._
import org.openpandora.box.model.Database
import org.openpandora.box.model.User
import org.openpandora.box.util.DotDesktopCategories
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.dsl.ast._
import scala.xml.NodeSeq
import scala.xml.Text

class ApplicationStatistics extends DispatchSnippet
                               with Logger {
  def dispatch = {
    case "uploadHistory" => uploadHistory
  }

  def round[A](f: NumericalExpression[A]) = new UnaryFloatOp[A](f, "round")

  def uploadHistory(downloadHistory: NodeSeq): NodeSeq = {
    //Group stats by days
    val groupWidth = (24*60*60*1000)
    val uploadsLastMonth =
      from(Database.packages){pkg =>
        groupBy(round(pkg.uploadTime div groupWidth)) compute(count) orderBy(pkg.uploadTime asc)
      }.toSeq

    val counts = uploadsLastMonth.scanLeft(0l)((acc, next) => acc + next.measures)
    val times = uploadsLastMonth.map(_.key.toLong * groupWidth)
    Text(times zip counts map { upload: (Long, Long) =>
        "[" + upload._1.toString + "," + upload._2.toString + "]"
      } mkString("[", ",", "]"))
  }
}
