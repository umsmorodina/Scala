import java.util.Date

import scala.util.parsing.combinator.RegexParsers
 
case class CommandCreator(command : String, user : User)  extends  RegexParsers{
  val PW = new PollWorker()

  def getCommandName() = {
    Parsers.parseAll(Parsers.commandNameParser, command).getOrElse("/unknown")
  }

  def gelNullArg() : Either[String, String] = {
    val res = Parsers.parseAll(Parsers.arglessParser, command)
    res match {
      case Parsers.NoSuccess(msg, _) => msg match {
        case "end of input expected" => Left("ERROR BY EXTRA ARGS")
        case _ => Left(msg)
      }
      case _ => Right(res.get)
    }
  }

  def getIdArg(): Either[String, Int] = {
    val res = Parsers.parseAll(Parsers.oneArgParser, command)
    res match {
      case Parsers.NoSuccess(msg, _) => msg match {
        case "end of input expected" => Left("ERROR BY EXTRA ARGS")
        case _ => Left(msg)
      }
      case _ => Right(res.get)
    }
  }
  
  def getArgsForAnswer() : Either[String, (Int, String)] = {
    val res = Parsers.parseAll(Parsers.answerParser, command)
    res match {
      case Parsers.NoSuccess(msg, _) => msg match {
        case "end of input expected" => Left("ERROR BY EXTRA ARGS")
        case _ => Left(msg)
      }
      case _ => Right(res.get)
    }
  }
  
  def getArgsForCreatePoll() : Either[String, (String, Boolean, Boolean, Option[Date], Option[Date])] = {
    val res = Parsers.parseAll(Parsers.createPollParser, command)
    res match {
      case Parsers.NoSuccess(msg, _) => msg match {
        case "end of input expected" => Left("ERROR BY EXTRA ARGS")
        case _ => Left(msg)
      }
      case _ => Right(res.get)
    }
  }
  
  def gerArgsForAddQuestion() : Either[String, (String, String, List[String])] = {
    val res = Parsers.parseAll(Parsers.addQuestionParser, command)
    res match {
      case Parsers.NoSuccess(msg, _) => msg match {
        case "end of input expected" => Left("ERROR BY EXTRA ARGS")
        case _ => Left(msg)
      }
      case _ => Right(res.get)
    }
  }

  def makeCommand(): String = {
    val name = getCommandName().trim()
    name match {
      case "create_poll" => getArgsForCreatePoll() match {
        case Left(msg) => msg
        case Right(args) =>
          PW.createPoll(args._1.replace("^", "(").replace("~", ")"),
            args._2, args._3, args._4, args._5, user.id)
      }
        
      case "result" => getIdArg() match {
        case Left(msg) => msg
        case Right(id) => PW.result(id)
      }
        
      case "list" => gelNullArg() match {
        case Left(msg) => msg
        case Right(id) => PW.list().mkString("")
      }

      case "start_poll" => getIdArg() match {
        case Left(msg) => msg
        case Right(id) => PW.startPoll(id, user.id)
      }
          
      case "stop_poll" => getIdArg() match {
        case Left(msg) => msg
        case Right(id) => PW.stopPoll(id, user.id)
      }
          
      case "delete_poll" => getIdArg() match {
        case Left(msg) => msg
        case Right(id) => PW.deletePoll(id, user.id)
      }
          
      case "begin" => getIdArg() match {
        case Left(msg) => msg
        case Right(id) => PW.beginPoll(id, user.id)
      }
          
      case "add_question" => gerArgsForAddQuestion() match {
        case Left(msg) => msg
        case Right(args) => PW.addQuestion(user.id, args._1, args._2, args._3)
      }
          
      case "delete_question" => getIdArg() match {
        case Left(msg) => msg
        case Right(id) => PW.deleteQuestion(id, user.id)
      }
          
      case "answer" => getArgsForAnswer() match {
        case Left(msg) => msg
        case Right(args) => PW.answer(args._2, args._1, user)
      }
          
      case "end" => gelNullArg() match {
        case Left(msg) => msg
        case Right(id) => PW.endPoll(user.id)
      }

      case "view" => gelNullArg() match {
        case Left(msg) => msg
        case Right(id) => PW.viewPoll(user.id)
      }
          
      case _ => "ERROR BY UNKNOWN COMMAND"
    }
  }
}
