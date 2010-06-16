package org.openpandora.box.util

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.PackratParsers

object ApplicationQueryParser {
  sealed trait Expression
  case class MaxResults(num: Int) extends Expression
  case class SearchAuthor(author: String) extends Expression
  case class SearchUploader(uploader: String) extends Expression
  case class SearchKeyword(word: String) extends Expression
  case class SearchTitle(title: String) extends Expression
  case class SearchDescription(description: String) extends Expression
  case class SearchCategory(category: String) extends Expression
  case class SearchVersion(major: Int, minor: Int, release: Int, build: Int) extends Expression
  case class SearchVersionMajor(major: Int) extends Expression
  case class SearchVersionMinor(minor: Int) extends Expression
  case class SearchVersionRelease(release: Int) extends Expression
  case class SearchVersionBuild(build: Int) extends Expression
  sealed trait OrderingExpression extends Expression {
    val ascending: Boolean
  }
  
  case class OrderByTitle(ascending: Boolean) extends OrderingExpression
  case class OrderByTime(ascending: Boolean) extends OrderingExpression
  case class OrderByRating(ascending: Boolean) extends OrderingExpression
  case class OrderByPxmlId(ascending: Boolean) extends OrderingExpression
  case class OrderByAuthor(ascending: Boolean) extends OrderingExpression

  val default: ApplicationQueryParser = new ApplicationQueryParserImpl
}

trait ApplicationQueryParser extends JavaTokenParsers {
  val exprs: Parser[Seq[ApplicationQueryParser.Expression]]
}

private[util] class ApplicationQueryParserImpl extends ApplicationQueryParser
                                                   with PackratParsers {
  import ApplicationQueryParser._

  val stringArgs: PackratParser[String] = (stringLiteral | ident | failure("Invalid string"))

  val numberArgs = wholeNumber | failure("Invalid number")

  val versionArgs = wholeNumber ~ "." ~ wholeNumber ~ "." ~ wholeNumber ~ "." ~ wholeNumber ^^ {
    case maj ~ "." ~ min ~ "." ~ rev ~ "." ~ build => (maj.toInt, min.toInt, rev.toInt, build.toInt)
  }
  
  val author: PackratParser[Expression]      = ("author:")                ~> stringArgs  ^^ (SearchAuthor(_))
  val uploader: PackratParser[Expression]    = ("uploader:")              ~> stringArgs  ^^ (SearchUploader(_))
  val title: PackratParser[Expression]       = ("title:")                 ~> stringArgs  ^^ (SearchTitle(_))
  val description: PackratParser[Expression] = ("description:" | "desc:") ~> stringArgs  ^^ (SearchDescription(_))
  val category: PackratParser[Expression]    = ("category:" | "cat:")     ~> stringArgs  ^^ (SearchCategory(_))
  val major: PackratParser[Expression]       = ("major:" | "maj:")        ~> numberArgs  ^^ (x => SearchVersionMajor(x.toInt))
  val minor: PackratParser[Expression]       = ("minor:" | "min:")        ~> numberArgs  ^^ (x => SearchVersionMinor(x.toInt))
  val release: PackratParser[Expression]     = ("release:" | "rel:")      ~> numberArgs  ^^ (x => SearchVersionRelease(x.toInt))
  val build: PackratParser[Expression]       = ("build:" | "bui:")        ~> numberArgs  ^^ (x => SearchVersionBuild(x.toInt))
  val max: PackratParser[Expression]         = ("max:" | "maxResults:")   ~> numberArgs  ^^ (x => MaxResults(x.toInt))
  val version: PackratParser[Expression]     = ("version:" | "ver:")      ~> versionArgs ^^ ((SearchVersion.apply _).tupled)
  val keyword: PackratParser[Expression]     = stringArgs                                ^^ (SearchKeyword(_))

  val byNameAsc: PackratParser[Expression]   = "orderby:titleasc"   ^^ (x => OrderByTitle(true))
  val byTimeAsc: PackratParser[Expression]   = "orderby:timeasc"    ^^ (x => OrderByTime(true))
  val byRatingAsc: PackratParser[Expression] = "orderby:ratingasc"  ^^ (x => OrderByRating(true))
  val byPxmlIdAsc: PackratParser[Expression] = "orderby:pxmlidasc"  ^^ (x => OrderByPxmlId(true))
  val byAuthorAsc: PackratParser[Expression] = "orderby:authorasc"  ^^ (x => OrderByAuthor(true))
  val byName: PackratParser[Expression]      = "orderby:title"      ^^ (x => OrderByTitle(true))
  val byTime: PackratParser[Expression]      = "orderby:time"       ^^ (x => OrderByTime(false))
  val byRating: PackratParser[Expression]    = "orderby:rating"     ^^ (x => OrderByRating(false))
  val byPxmlId: PackratParser[Expression]    = "orderby:pxmlid"     ^^ (x => OrderByPxmlId(true))
  val byAuthor: PackratParser[Expression]    = "orderby:author"     ^^ (x => OrderByAuthor(true))
  val byNameDesc: PackratParser[Expression]  = "orderby:titledesc"  ^^ (x => OrderByTitle(false))
  val byTimeDesc: PackratParser[Expression]  = "orderby:timedesc"   ^^ (x => OrderByTime(false))
  val byRatingDesc: PackratParser[Expression]= "orderby:ratingdesc" ^^ (x => OrderByRating(false))
  val byPxmlIdDesc: PackratParser[Expression]= "orderby:pxmliddesc" ^^ (x => OrderByPxmlId(false))
  val byAuthorDesc: PackratParser[Expression]= "orderby:authordesc" ^^ (x => OrderByAuthor(false))

  val expr: PackratParser[Expression] = (
    byNameAsc  | byTimeAsc  | byRatingAsc  | byPxmlIdAsc  | byAuthorAsc  |
    byNameDesc | byTimeDesc | byRatingDesc | byPxmlIdDesc | byAuthorDesc |
    byName     | byTime     | byRating     | byPxmlId     | byAuthor     |
    major | minor | release | build | version |
    title | description | category | author | uploader |
    max |
    keyword | failure("Unknown function")
  )

  val exprs: PackratParser[Seq[Expression]] = rep1(expr)
}
