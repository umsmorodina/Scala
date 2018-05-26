import java.text.SimpleDateFormat
import java.util.Date

import scala.util.parsing.combinator.RegexParsers

object Parsers extends RegexParsers {

  def mainParser(command: String, userId : Int = 0): String = {
   ""
  }
  def commandNameParser: Parser[String] = "/" ~> "[^(]+".r <~ rep(any) ^^ { a => a }

  def oneArgParser: Parser[Int] =  stopParser | startParser | resultParser |
    deleteParser | beginParser | deleteQuestionParser

  def arglessParser: Parser[String] = listParser | viewParser | endParser

  def id: Parser[Int] = "(" ~> "\\d+".r <~ ")" ^^ { a => a.toInt }

  def numbers: Parser[Seq[Int]] = "(" ~> rep("\\d+".r) <~ ")" ^^ { a => a.map(_.toInt) }

  def parserType: Parser[String] = "(" ~> "open|choice|multi".r <~ ")" ^^ { a => a}

  def resultParser: Parser[Int] =
    "/result".withFailureMessage("ERROR BY COMMAND NAME") ~> id.withFailureMessage("ERROR BY ID - IT IS NOT INT")

  def startParser: Parser[Int] =
    "/start_poll".withFailureMessage("ERROR BY COMMAND NAME") ~> id.withFailureMessage("ERROR BY ID - IT IS NOT INT")

  def stopParser: Parser[Int] =
    "/stop_poll".withFailureMessage("ERROR BY COMMAND NAME") ~> id.withFailureMessage("ERROR BY ID - IT IS NOT INT")

  def deleteParser: Parser[Int] =
    "/delete_poll".withFailureMessage("ERROR BY COMMAND NAME") ~> id.withFailureMessage("ERROR BY ID - IT IS NOT INT")

  def listParser: Parser[String] = "/list".withFailureMessage("ERROR BY COMMAND NAME")

  def parserContinuous: Parser[Boolean] = "(" ~> "afterstop|continuous".r <~ ")" ^^ { a => a == "afterstop" }

  def parserAnnonymous: Parser[Boolean] = "(" ~> "yes|no".r <~ ")" ^^ { a => a == "yes" }

  def time: Parser[Option[Date]] = "(" ~> "\\d{2}:\\d{2}:\\d{2};\\d{2}:\\d{2}:\\d{2}".r <~ ")" ^^
    { a => Some(new SimpleDateFormat("HH:mm:ss;yy:MM:dd").parse(a)) }

  def any: Parser[String] = "(" ~> "[^()]+".r <~ ")"

  def first: Parser[(String, Boolean, Boolean, Option[Date], Option[Date])] =
    "/create_poll ".withFailureMessage("ERROR BY COMMAND NAME") ~> any.withFailureMessage("ERROR BY ARG #1") ^^
      { a => (a, true, true, None, None) }

  def second: Parser[(String, Boolean, Boolean, Option[Date], Option[Date])] =
    first ~ parserAnnonymous.withFailureMessage("ERROR BY ARG #2") ^^ { a => (a._1._1, a._2, true, None, None) }

  def third: Parser[(String, Boolean, Boolean, Option[Date], Option[Date])] =
    second ~ parserContinuous.withFailureMessage("ERROR BY ARG #3") ^^ { a => (a._1._1, a._1._2, a._2, None, None) }

  def fourth: Parser[(String, Boolean, Boolean, Option[Date], Option[Date])] =
    third ~ time.withFailureMessage("ERROR BY ARG #4") ^^ { a => (a._1._1, a._1._2, a._1._3, a._2, None)}

  def fifth: Parser[(String, Boolean, Boolean, Option[Date], Option[Date])] =
    fourth ~ time.withFailureMessage("ERROR BY ARG #5") ^^ { a => (a._1._1, a._1._2, a._1._3, a._1._4, a._2) }

  def createPollParser: Parser[(String, Boolean, Boolean, Option[Date], Option[Date])] =
    fifth | fourth | third | second | first

  def beginParser: Parser[Int] =
    "/begin".withFailureMessage("ERROR BY COMMAND NAME") ~> id.withFailureMessage("ERROR BY ARG #1")

  def endParser: Parser[String] = "/end".withFailureMessage("ERROR BY COMMAND NAME")

  def viewParser: Parser[String] = "/view".withFailureMessage("ERROR BY COMMAND NAME")

  def deleteQuestionParser: Parser[Int] = "/delete_question".withFailureMessage("ERROR BY COMMAND NAME") ~>
    id.withFailureMessage("ERROR BY ARG #1")

  def answerParser: Parser[(Int, String)] = "/answer".withFailureMessage("ERROR BY COMMAND NAME") ~>
    id.withFailureMessage("ERROR BY ARG #1") ~ any.withFailureMessage("ERROR BY ARG #2") ^^ { a => (a._1, a._2)}

  def firstAddQuestionParser: Parser[(String, String, List[String])] =
    "/add_question".withFailureMessage("ERROR BY COMMAND NAME") ~>
    any.withFailureMessage("ERROR BY ARG #1") ~ parserType.withFailureMessage("ERROR BY ARG #2") ~ rep(any) ^^
    { a =>  (a._1._1, a._1._2, a._2)}

  def secondAddQuestionParser: Parser[(String, String, List[String])] =
    "/add_question".withFailureMessage("ERROR BY COMMAND NAME") ~>
    any.withFailureMessage("ERROR BY ARG #1") ^^ { a => (a, "open", Nil) }

  def addQuestionParser: Parser[(String, String, List[String])] = firstAddQuestionParser | secondAddQuestionParser

}
