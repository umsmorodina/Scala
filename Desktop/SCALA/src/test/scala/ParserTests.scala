import org.scalatest.{FunSuite, Matchers}

import scala.util.parsing.combinator.RegexParsers

class ParserTests extends FunSuite with Matchers with  RegexParsers {
  //Первая часть команд
  //Тесты для create_poll:
  def processCreate(command : String) : String = {
    val res = Parsers.parseAll(Parsers.createPollParser,
      command.replace("((", "^").replace("))", "~"))
    val either = res match {
      case Parsers.NoSuccess(msg, _) => msg match {
        case "end of input expected" => Left("ERROR BY EXTRA ARGS")
        case _ => Left(msg)
      }
      case _ => Right(res.get)
    }
    either match {
      case Left(msg) => msg
      case Right(args) => "SUCCESS"
    }
  }

  test("wrong command - wrong name of Poll") {assertResult("ERROR BY ARG #1")
    { processCreate("/create_poll (wr(ong_nam)e)")}}

  test("wrong command - without slash") {assertResult("ERROR BY COMMAND NAME")
   {processCreate("create_poll (where_slash?)")}}

  test("wrong command - without args") {assertResult("ERROR BY ARG #1")
  {processCreate("/create_poll ")}}

  test("good command - with name") {assertResult("SUCCESS")
  {processCreate("/create_poll (name)")}}

  test("good command - with name and correct brackets") {assertResult("SUCCESS")
  {processCreate("/create_poll (name((good name)))")}}

  test("good command - with strange name") {assertResult("SUCCESS")
  {processCreate("/create_poll (%%*Str$AngE@NA--me839)")}}

  test("good command - with yes annon") {assertResult("SUCCESS")
  {processCreate("/create_poll (name) (yes)")}}

  test("good command - with no annon") {assertResult("SUCCESS")
  {processCreate("/create_poll (name) (no)")}}

  test("wrong command - with wrong annon") {assertResult("ERROR BY ARG #2")
  {processCreate("/create_poll (name) (bla-bla-bla)")}}

  test("good command - with 3 args(a)") {assertResult("SUCCESS")
  {processCreate("/create_poll (norm_name) (yes) (afterstop)")}}

  test("good command - with 3 args(c)") {assertResult("SUCCESS")
  {processCreate("/create_poll (norm_name) (yes) (continuous)")}}

  test("wrong command - with wrong third arg") {assertResult("ERROR BY ARG #3")
  {processCreate("/create_poll (norm_name) (yes) (bla-bla-bla)")}}

  test("wrong command - with wrong time") {assertResult("ERROR BY ARG #4")
  {processCreate("/create_poll (norm_name) (yes) (afterstop) (abc 12:12:12)")}}

  test("wrong command - with wrong separator of time") {assertResult("ERROR BY ARG #4")
  {processCreate("/create_poll (norm_name) (yes) (afterstop) (12:12:12,12:12:12)")}}

  test("good command - with time") {assertResult("SUCCESS")
  {processCreate("/create_poll (norm_name) (yes) (afterstop) (12:12:12;12:12:12)")}}

  test("wrong command - with time but withoud third arg") {assertResult("ERROR BY ARG #3")
  {processCreate("/create_poll (norm_name) (yes) (12:12:12;12:12:12)")}}

  test("wrong command - with extra time") {assertResult("ERROR BY ARG #4")
  {processCreate("/create_poll (norm_name) (yes) (afterstop) (12:12432:12 12:1203902:12)")}}

  test("wrong command - without brackets") {assertResult("ERROR BY ARG #1")
  {processCreate("/create_poll norm_nam yes annonymous 12:12:12;12:12:12")}}

  def processOneArgCommand(command : String) : String = {
    val res = Parsers.parseAll(Parsers.oneArgParser,
      command.replace("((", "^").replace("))", "~"))
    val either = res match {
      case Parsers.NoSuccess(msg, _) => msg match {
        case "end of input expected" => Left("ERROR BY EXTRA ARGS")
        case _ => Left(msg)
      }
      case _ => Right(res.get)
    }
    either match {
      case Left(msg) => msg
      case Right(arg) => arg.toString
    }
  }

  def processSimpleCommand(command : String) : String = {
    val res = Parsers.parseAll(Parsers.arglessParser,
      command.replace("((", "^").replace("))", "~"))
    val either = res match {
      case Parsers.NoSuccess(msg, _) => msg match {
        case "end of input expected" => Left("ERROR BY EXTRA ARGS")
        case _ => Left(msg)
      }
      case _ => Right(res.get)
    }
    either match {
      case Left(msg) => msg
      case Right(arg) => "SUCCESS"
    }
  }


  //Тесты для list:
  test("good command - good list") {assertResult("SUCCESS")
  {processSimpleCommand("/list")}}

  test("wrong command - list with arg") {assertResult("ERROR BY EXTRA ARGS")
  {processSimpleCommand("/list (2)")}}


  //Тесты для start_poll:
  test("good command - good start") {assertResult("1")
  {processOneArgCommand("/start_poll (1)")}}

  test("wrong command - start with wrong arg") {assertResult("ERROR BY ID - IT IS NOT INT")
  {processOneArgCommand("/start_poll (3abgc)")}}

  test("wrong command - start without arg") {assertResult("ERROR BY ID - IT IS NOT INT")
  {processOneArgCommand("/start_poll")}}

  test("wrong command - start without brackets") {assertResult("ERROR BY ID - IT IS NOT INT")
  {processOneArgCommand("/start_poll 3")}}


  //Тесты для stop_poll:
  test("good command - good stop") {assertResult("5")
  {processOneArgCommand("/stop_poll (5)")}}

  test("wrong command - stop with wrong arg") {assertResult("ERROR BY ID - IT IS NOT INT")
  {processOneArgCommand("/stop_poll (3abgc)")}}

  test("wrong command - stop with extra arg") {assertResult("ERROR BY EXTRA ARGS")
  {processOneArgCommand("/stop_poll (3) (afterstop)")}}


  //Тесты для result:
  test("good command - good result") {assertResult("5")
  {processOneArgCommand("/result (5)")}}

  test("wrong command - result with wrong arg") {assertResult("ERROR BY ID - IT IS NOT INT")
  {processOneArgCommand("/result (3abgc)")}}

  test("wrong command - result with extra arg") {assertResult("ERROR BY EXTRA ARGS")
  {processOneArgCommand("/result (3) (afterstop)")}}


  //Тесты для delete_poll:
  test("good command - good delete") {assertResult("5")
  {processOneArgCommand("/delete_poll (5)")}}

  test("wrong command - delete with wrong arg") {assertResult("ERROR BY ID - IT IS NOT INT")
  {processOneArgCommand("/delete_poll (3abgc)")}}

  test("wrong command - delete with extra arg") {assertResult("ERROR BY EXTRA ARGS")
  {processOneArgCommand("/delete_poll (3) (afterstop)")}}



  //Вторая часть команд
  //Тесты для begin:
  test("good command - good begin") {assertResult("1")
  {processOneArgCommand("/begin (1)")}}

  test("wrong command - begin with wrong arg") {assertResult("ERROR BY ARG #1")
  {processOneArgCommand("/begin (3abgc)")}}

  test("wrong command - begin with extra arg") {assertResult("ERROR BY EXTRA ARGS")
  {processOneArgCommand("/begin (3) (multi)")}}


  //Тесты для end:
  test("good command - good end") {assertResult("SUCCESS")
  {processSimpleCommand("/end")}}

  test("wrong command - end with arg") {assertResult("ERROR BY EXTRA ARGS")
  {processSimpleCommand("/end (10)")}}


  //Тесты для view:
  test("good command - good view") {assertResult("SUCCESS")
  {processSimpleCommand("/view")}}

  test("wrong command - view with arg") {assertResult("ERROR BY EXTRA ARGS")
  {processSimpleCommand("/view (8)")}}


  def processAddQuestion(command : String) : String = {
    val res = Parsers.parseAll(Parsers.addQuestionParser,
      command.replace("((", "^").replace("))", "~"))
    val either = res match {
      case Parsers.NoSuccess(msg, _) => msg match {
        case "end of input expected" => Left("ERROR BY EXTRA ARGS")
        case _ => Left(msg)
      }
      case _ => Right(res.get)
    }
    either match {
      case Left(msg) => msg
      case Right(arg) => "SUCCESS"
    }
  }


  //Тесты для add_question:
  test("wrong add_question - wrong name of question") {
    assertResult("ERROR BY ARG #1") {Parsers.mainParser("/begin (1)")
      processAddQuestion("/add_question (wr(ong_nam)e) \n" +
      "(1) \n (2)")}}

  test("wrong add_question - without slash") {
    assertResult("ERROR BY COMMAND NAME") {processAddQuestion("add_question (where_slash?)")}}

  test("wrong add_question - without args") {
    assertResult("ERROR BY ARG #1") {processAddQuestion("/add_question ")}}

  test("good add_question - without answer") {
    assertResult("SUCCESS") {processAddQuestion("/add_question (quest)")}}

  test("wrong add_question - without type but with answer") {
    assertResult("ERROR BY ARG #2") {processAddQuestion("/add_question (quest) \n" +
      "(1) \n (2)")}}

  test("good add_question - with name and correct brackets") {
    assertResult("SUCCESS") {processAddQuestion("/add_question (quest((good quest)))")}}

  test("good add_question - with type open") {
    assertResult("SUCCESS") {processAddQuestion("/add_question (quest1) (open)")}}

  test("good add_question - with type choice") {
    assertResult("SUCCESS") {processAddQuestion("/add_question (quest2) (choice) \n" +
      "(1) \n (b)")}}

  test("good add_question - with type multi") {
    assertResult("SUCCESS") {processAddQuestion("/add_question (quest3) (multi) \n" +
      "(1) \n (b) \n (2)")}}

  test("good add_question - with many answ") {
    assertResult("SUCCESS") {processAddQuestion("/add_question (quest4) (multi) \n" +
      "(1) \n (2) \n (3) \n (4) \n (5) \n (6) \n (7) \n (9) \n (10)")}}


  def processAnswer(command : String) : String = {
    val res = Parsers.parseAll(Parsers.answerParser,
      command.replace("((", "^").replace("))", "~"))
    val either = res match {
      case Parsers.NoSuccess(msg, _) => msg match {
        case "end of input expected" => Left("ERROR BY EXTRA ARGS")
        case _ => Left(msg)
      }
      case _ => Right(res.get)
    }
    either match {
      case Left(msg) => msg
      case Right(arg) => "SUCCESS"
    }
  }


  //Тесты для answer:
  test("wrong command - answer without arg")
  {assertResult("ERROR BY ARG #2") {processAnswer("/answer (9)")}}

  test("good command - answer for choice") {assertResult("SUCCESS")
  {processAnswer("/answer (4) (1)")}}

  test("wrong command - answer for choice with wrong arg")
  {assertResult("ERROR BY ARG #1")
  {processAnswer("/answer (3abgc) (9)")}}

  test("wrong command - answer for choice with extra arg") {assertResult("ERROR BY EXTRA ARGS")
  {processAnswer("/answer (8) (901) (multi)")}}

  test("good command - answer for open") {assertResult("SUCCESS")
  {processAnswer("/answer (2) (answ)")}}

  test("wrong command - answer for open with wrong arg")
  {assertResult("ERROR BY ARG #1")
  {processAnswer("/answer (3abgc) (my_answer)")}}

  test("wrong command - answer for open with extra arg") {assertResult("ERROR BY EXTRA ARGS")
  {processAnswer("/answer (8) (my_answer) (m)")}}

  test("good command - wrong answer for choice")
  {assertResult("SUCCESS") {processAnswer("/answer (5) (1 2)")}}

  test("wrong command - answer for multi with wrong arg")
  {assertResult("ERROR BY ARG #1")
  {processAnswer("/answer (3abgc) (3 54 6)")}}

  test("wrong command - answer for multi with extra arg") {assertResult("ERROR BY EXTRA ARGS")
  {processAnswer("/answer (8) (9 1) (abc)")}}


  //Тесты для delete_question:
  test("good command - good delete_question") {assertResult("5")
  {processOneArgCommand("/delete_question (5)")}}

  test("wrong command - delete_question with wrong arg")
  {assertResult("ERROR BY ARG #1")
  {processOneArgCommand("/delete_question (3abgc)")}}

  test("wrong command - delete_question with extra arg") {assertResult("ERROR BY EXTRA ARGS")
  {processOneArgCommand("/delete_question (901) (multi)")}}

}
