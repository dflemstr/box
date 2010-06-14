package org.openpandora.box.util

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.PackratParsers

object ApplicationFilterParser {
  sealed trait Expression
  case class MaxResults(num: Int) extends Expression
  case class FilterAuthor(author: String) extends Expression
  case class FilterUploader(uploader: String) extends Expression
  case class FilterKeyword(word: String) extends Expression
  case class FilterTitle(title: String) extends Expression
  case class FilterDescription(description: String) extends Expression
  case class FilterCategory(category: String) extends Expression
  case class FilterVersion(major: Int, minor: Int, release: Int, build: Int) extends Expression
  case class FilterVersionMajor(major: Int) extends Expression
  case class FilterVersionMinor(minor: Int) extends Expression
  case class FilterVersionRelease(release: Int) extends Expression
  case class FilterVersionBuild(build: Int) extends Expression
  sealed trait OrderingExpression extends Expression {
    val ascending: Boolean
  }
  case class OrderByTitle(ascending: Boolean) extends OrderingExpression
  case class OrderByTime(ascending: Boolean) extends OrderingExpression
  case class OrderByRating(ascending: Boolean) extends OrderingExpression

  val default: ApplicationFilterParser = new ApplicationFilterParserImpl
}

trait ApplicationFilterParser extends JavaTokenParsers {
  val exprs: Parser[Seq[ApplicationFilterParser.Expression]]
}

private[util] class ApplicationFilterParserImpl extends ApplicationFilterParser
                                                   with PackratParsers {
  import ApplicationFilterParser._

  val stringArgs: PackratParser[String] = (stringLiteral | ident)

  val numberArgs = wholeNumber

  val versionArgs = wholeNumber ~ "." ~ wholeNumber ~ "." ~ wholeNumber ~ "." ~ wholeNumber ^^ {
    case maj ~ "." ~ min ~ "." ~ rev ~ "." ~ build => (maj.toInt, min.toInt, rev.toInt, build.toInt)
  }
  
  val author: PackratParser[Expression]      = ("author:")                ~> stringArgs  ^^ (FilterAuthor(_))
  val uploader: PackratParser[Expression]    = ("uploader:")              ~> stringArgs  ^^ (FilterUploader(_))
  val title: PackratParser[Expression]       = ("title:")                 ~> stringArgs  ^^ (FilterTitle(_))
  val description: PackratParser[Expression] = ("description:" | "desc:") ~> stringArgs  ^^ (FilterDescription(_))
  val category: PackratParser[Expression]    = ("category:" | "cat:")     ~> stringArgs  ^^ (FilterCategory(_))
  val major: PackratParser[Expression]       = ("major:" | "maj:")        ~> numberArgs  ^^ (x => FilterVersionMajor(x.toInt))
  val minor: PackratParser[Expression]       = ("minor:" | "min:")        ~> numberArgs  ^^ (x => FilterVersionMinor(x.toInt))
  val release: PackratParser[Expression]     = ("release:" | "rel:")      ~> numberArgs  ^^ (x => FilterVersionRelease(x.toInt))
  val build: PackratParser[Expression]       = ("build:" | "bui:")        ~> numberArgs  ^^ (x => FilterVersionBuild(x.toInt))
  val max: PackratParser[Expression]         = ("max:" | "maxResults:")   ~> numberArgs  ^^ (x => MaxResults(x.toInt))
  val version: PackratParser[Expression]     = ("version:" | "ver:")      ~> versionArgs ^^ ((FilterVersion.apply _).tupled)
  val keyword: PackratParser[Expression]     = stringArgs                                ^^ (FilterKeyword(_))

  val byNameAsc: PackratParser[Expression]   = "orderby:titleasc"   ^^ (x => OrderByTitle(true))
  val byTimeAsc: PackratParser[Expression]   = "orderby:timeasc"    ^^ (x => OrderByTime(true))
  val byRatingAsc: PackratParser[Expression] = "orderby:ratingasc"  ^^ (x => OrderByRating(true))
  val byName: PackratParser[Expression]      = "orderby:title"      ^^ (x => OrderByTitle(true))
  val byTime: PackratParser[Expression]      = "orderby:time"       ^^ (x => OrderByTime(true))
  val byRating: PackratParser[Expression]    = "orderby:rating"     ^^ (x => OrderByRating(false))
  val byNameDesc: PackratParser[Expression]  = "orderby:titledesc"  ^^ (x => OrderByTitle(false))
  val byTimeDesc: PackratParser[Expression]  = "orderby:timedesc"   ^^ (x => OrderByTime(false))
  val byRatingDesc: PackratParser[Expression]= "orderby:ratingdesc" ^^ (x => OrderByRating(false))

  val expr: PackratParser[Expression] = (
    byNameAsc | byTimeAsc | byRatingAsc |
    byNameDesc | byTimeDesc | byRatingDesc |
    byName | byTime | byRating |
    major | minor | release | build | version |
    title | description | category | author | uploader |
    max |
    keyword 
  )

  val exprs: PackratParser[Seq[Expression]] = rep1(expr)
}
