package org.openpandora.box.util

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.PackratParsers

object ApplicationFilterParser extends JavaTokenParsers with PackratParsers {
  private val stringArgs: PackratParser[String] = (stringLiteral | ident)

  private val numberArgs = wholeNumber

  private val versionArgs = wholeNumber ~ "." ~ wholeNumber ~ "." ~ wholeNumber ~ "." ~ wholeNumber ^^ {
    case maj ~ "." ~ min ~ "." ~ rev ~ "." ~ build => (maj.toInt, min.toInt, rev.toInt, build.toInt)
  }
  
  private val author: PackratParser[Expression]      = ("author:")                ~> stringArgs  ^^ (FilterAuthor(_))
  private val uploader: PackratParser[Expression]    = ("uploader:")              ~> stringArgs  ^^ (FilterUploader(_))
  private val title: PackratParser[Expression]       = ("title:")                 ~> stringArgs  ^^ (FilterTitle(_))
  private val description: PackratParser[Expression] = ("description:" | "desc:") ~> stringArgs  ^^ (FilterDescription(_))
  private val category: PackratParser[Expression]    = ("category:" | "cat:")     ~> stringArgs  ^^ (FilterCategory(_))
  private val major: PackratParser[Expression]       = ("major:" | "maj:")        ~> numberArgs  ^^ (x => FilterVersionMajor(x.toInt))
  private val minor: PackratParser[Expression]       = ("minor:" | "min:")        ~> numberArgs  ^^ (x => FilterVersionMinor(x.toInt))
  private val release: PackratParser[Expression]     = ("release:" | "rel:")      ~> numberArgs  ^^ (x => FilterVersionRelease(x.toInt))
  private val build: PackratParser[Expression]       = ("build:" | "bui:")        ~> numberArgs  ^^ (x => FilterVersionBuild(x.toInt))
  private val version: PackratParser[Expression]     = ("version:" | "ver:")      ~> versionArgs ^^ ((FilterVersion.apply _).tupled)
  private val keyword: PackratParser[Expression]     = stringArgs      ^^ (FilterKeyword(_))

  private val byNameAsc: PackratParser[Expression]   = "orderby:titleasc"   ^^ (x => OrderByTitle(true))
  private val byTimeAsc: PackratParser[Expression]   = "orderby:timeasc"    ^^ (x => OrderByTime(true))
  private val byRatingAsc: PackratParser[Expression] = "orderby:ratingasc"  ^^ (x => OrderByRating(true))
  private val byName: PackratParser[Expression]      = "orderby:title"       ^^ (x => OrderByTitle(false))
  private val byTime: PackratParser[Expression]      = "orderby:time"        ^^ (x => OrderByTime(true))
  private val byRating: PackratParser[Expression]    = "orderby:rating"      ^^ (x => OrderByRating(false))
  private val byNameDesc: PackratParser[Expression]  = "orderby:titledesc"  ^^ (x => OrderByTitle(false))
  private val byTimeDesc: PackratParser[Expression]  = "orderby:timedesc"   ^^ (x => OrderByTime(false))
  private val byRatingDesc: PackratParser[Expression]= "orderby:ratingdesc" ^^ (x => OrderByRating(false))

  val expr: PackratParser[Expression] = (
    byNameAsc | byTimeAsc | byRatingAsc |
    byNameDesc | byTimeDesc | byRatingDesc |
    byName | byTime | byRating |
    major | minor | release | build | version |
    title | description | category | author | uploader |
    keyword 
  )

  val exprs: PackratParser[Seq[Expression]] = rep1(expr)

  sealed trait Expression
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
    def ascending: Boolean
  }
  case class OrderByTitle(ascending: Boolean) extends OrderingExpression
  case class OrderByTime(ascending: Boolean) extends OrderingExpression
  case class OrderByRating(ascending: Boolean) extends OrderingExpression
}
