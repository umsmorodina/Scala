import java.text.SimpleDateFormat
import java.util.Date

import scala.util.parsing.combinator.RegexParsers

class Parser extends RegexParsers {
  val PW = new PollWorker()
  val defaultTime = new SimpleDateFormat("HH:mm:ss,yy:MM:dd").parse("00:00:00,11:01:01")
  var userID = 0

//  def start_parser(text: String): Unit = {
//    val a = text.split(nl + nl).map(mainParser)
//  }

  def mainParser(command: String, userId : Int = 0): String = {
    userID = userId
    val newCommand = command.replace("((", "^").replace("))", "~")
    val result = parseAll(globalParser, newCommand)
    val isItCreatePoll = (newCommand.startsWith("/create_poll"))
    if (!isItCreatePoll) {
      result match {
        case NoSuccess(msg, _) => msg match {
          case "end of input expected" => "ERROR BY EXTRA ARGS"
          case _ => msg
        }
        case _ => result.get.toString.replace("^", "(").replace("`", ")")
      }
    }
    else mainCreatePollParser(newCommand, userID)
  }

  def mainCreatePollParser(command: String, userId : Int): String = {
    val result = parseAll(createPollParser, command)
    result match {
      case NoSuccess(msg, _) => msg
      case _ => {
        val args = result.get
        PW.createPoll(args._1.replace("^", "(").replace("~", ")"),
          args._2, args._3, args._4, args._5, userId)
      }
    }
  }
  
  def commandNameParser: Parser[String] = "/" ~> "[^(]+".r <~ rep(any) ^^ { a => a }

  def globalParser: Parser[Any] = listParser | stopParser | startParser | resultParser | deleteParser | contextParser

  def id: Parser[Int] = "(" ~> "\\d+".r <~ ")" ^^ { a => a.toInt }

  def numbers: Parser[Seq[Int]] = "(" ~> rep("\\d+".r) <~ ")" ^^ { a => a.map(_.toInt) }

  def parserType: Parser[String] = "(" ~> "open|choice|multi".r <~ ")" ^^ { a => a}

  def resultParser: Parser[String] =
    "/result".withFailureMessage("ERROR BY COMMAND NAME") ~> id.withFailureMessage("ERROR BY ID - IT IS NOT INT") ^^
      { a => (PW.result(a)) }

  def startParser: Parser[String] =
    "/start_poll".withFailureMessage("ERROR BY COMMAND NAME") ~> id.withFailureMessage("ERROR BY ID - IT IS NOT INT") ^^
      { a => (PW.startPoll(a, userID)) }

  def stopParser: Parser[String] =
    "/stop_poll".withFailureMessage("ERROR BY COMMAND NAME") ~> id.withFailureMessage("ERROR BY ID - IT IS NOT INT") ^^
      { a => (PW.stopPoll(a, userID)) }

  def deleteParser: Parser[String] =
    "/delete_poll".withFailureMessage("ERROR BY COMMAND NAME") ~>
      id.withFailureMessage("ERROR BY ID - IT IS NOT INT") ^^ { a => (PW.deletePoll(a, userID)) }

  def listParser: Parser[String] = "/list".withFailureMessage("ERROR BY COMMAND NAME") ^^ { _ => PW.list.head }

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
    //first | second | third  | fourth | fifth
    fifth | fourth | third | second | first

  def beginParser: Parser[String] =
    "/begin".withFailureMessage("ERROR BY COMMAND NAME") ~> id.withFailureMessage("ERROR BY ARG #1") ^^
      { a => (PW.beginPoll(a, userID)) }

  def endParser: Parser[String] = "/end".withFailureMessage("ERROR BY COMMAND NAME") ^^ { _ => PW.endPoll(userID) }

  def viewParser: Parser[String] =
    "/view".withFailureMessage("ERROR BY COMMAND NAME") ^^ { _ => PW.viewPoll(userID) }

  def deleteQuestionParser: Parser[String] = "/delete_question".withFailureMessage("ERROR BY COMMAND NAME") ~>
    id.withFailureMessage("ERROR BY ARG #1") ^^ { a => (PW.deleteQuestion(a, userID)) }

  def answerParser: Parser[String] = "/answer".withFailureMessage("ERROR BY COMMAND NAME") ~>
    id.withFailureMessage("ERROR BY ARG #1") ~ any.withFailureMessage("ERROR BY ARG #2") ^^
    { a => PW.answer(a._2, a._1, userID)}

  def firstAddQuestionParser: Parser[String] = "/add_question".withFailureMessage("ERROR BY COMMAND NAME") ~>
    any.withFailureMessage("ERROR BY ARG #1") ~ parserType.withFailureMessage("ERROR BY ARG #2") ~ rep(any) ^^
    { a => PW.addQuestion(userID, a._1._1, a._1._2, a._2)}

  def secondAddQuestionParser: Parser[String] = "/add_question".withFailureMessage("ERROR BY COMMAND NAME") ~>
    any.withFailureMessage("ERROR BY ARG #1") ^^ { a => print(a.substring(0, a.length - 1))
    if (a.substring(0, a.length - 1).lastIndexOf(")") == -1)
    PW.addQuestion(userID, a)
    else "ERROR BY ARG#2"}

  def addQuestionParser: Parser[String] = firstAddQuestionParser | secondAddQuestionParser

  def contextParser: Parser[String] =
    beginParser | endParser | viewParser | deleteQuestionParser | answerParser | addQuestionParser
}
