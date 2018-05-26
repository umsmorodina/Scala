import java.text.SimpleDateFormat
import java.util.Date

import org.scalatest.FunSuite


class CommandTests extends FunSuite {
  val PW = new PollWorker
  val defaultDate : Date = new SimpleDateFormat("HH:mm:ss,yy:MM:dd").parse("00:00:00,11:01:01")

  test("/list at the start is empty") {
      assertResult("POLLS DON’T EXIST") {PW.list().head}}

  test("/deletePoll don't delete by wrong id"){
    assertResult("ERROR BY ID - NOT DELETE!") {PW.deletePoll(75, 1)}}

  test("/startPoll don't delete by wrong id"){
    assertResult("ERROR BY ID - NOT START!") {PW.startPoll(100, 1)}}

  test("/stopPoll don't delete by wrong id"){
    assertResult("ERROR BY ID - NOT STOP!") {PW.stopPoll(20, 1)}}

  test("/list works for the some poll")
    { PW.createPoll("FIRST", true, true, None, None, 1)
      assertResult(List[String]("POLL FIRST HAS ID = 1")) {PW.list}}

  test("/deletePoll does not delete by wrong id")
    {assertResult("ERROR BY ID - NOT DELETE!") {PW.deletePoll(50, 1)}}

  test("/startPoll start launch poll") {
    assertResult("START OK!") {PW.startPoll(1, 1)}}

  test("/deletePoll does not delete launch poll") {
    assertResult("ERROR BY LAUNCH - NOT DELETE!") {PW.deletePoll(1, 1)}}

  test("/stopPoll stop launch poll") {
    assertResult("STOP OK!") {PW.stopPoll(1, 1)}}

  test("/deletePoll delete usual poll") {
    PW.createPoll("SECOND", true, true, Some(defaultDate), Some(defaultDate), 1)
    assertResult("DELETE OK!") {PW.deletePoll(2, 1)}}

  test("/deletePoll delete stopped poll"){
    assertResult("DELETE OK!") {PW.deletePoll(1, 1)}}

  test("/deletePoll does not delete removed poll")
    {assertResult("ERROR BY ID - NOT DELETE!") {PW.deletePoll(1, 1)}}

  test("/list is empty if all polls were deleted") {
    assertResult("POLLS DON’T EXIST") {PW.list().head}}


  //Тесты для begin:
  test("/begin activates some good poll") {
    assertResult("BEGIN OK!") {
      PW.createPoll("THIRD", true, true, None, None, 1)
      PW.startPoll(3, 1)
      PW.beginPoll(3, 1)
    }
  }

  test("/begin does not activate not existed poll") {assertResult("ERROR BY ID - NOT BEGIN!") {
    PW.beginPoll(4, 1)
  }}

  test("/begin does not activate not started poll") {assertResult("ERROR BY NOT LAUNCH - NOT BEGIN!") {
    PW.createPoll("FOURTH", true, true, None, None, 2)
    PW.beginPoll(4, 1)
  }}

  test("/begin does not activate stopped poll") {assertResult("ERROR BY NOT LAUNCH - NOT BEGIN!") {
    PW.stopPoll(4, 1)
    PW.beginPoll(4, 1)
  }}

  test("/begin activates some poll from different users") {assertResult("BEGIN OK!") {
    println(PW.startPoll(4, 2))
    PW.beginPoll(4, 1)
  }}

  //Тесты для end:
  test("/end does not end for not launched poll")
  {assertResult("ERROR BY NOT BEGIN - NOT END!") {PW.endPoll(3)}}

  test("/end ends some good poll")
  {assertResult("END OK!") {PW.endPoll(1)}}


  //Тесты для add_question:
  test("/add_question adds first open question") {
    assertResult("1") {
      PW.beginPoll(4, 2)
      PW.addQuestion(2, "first_quest")
    }
  }

  test("/add_question adds second question with open arg")
  {assertResult("2") { PW.addQuestion(2, "new quest", "open")}}

  test("/add_question adds third question with choice arg")
  {assertResult("3")
  { PW.addQuestion(2, "new quest", "choice", List[String]("a", "b", "c"))}}

  test("/add_question adds fourth question with multi arg")
  {assertResult("4")
  { PW.addQuestion(2, "new quest", "multi", List[String]("a", "b", "c"))}}

  test("/add_question does not add question for not launched poll")
  {assertResult("ERROR BY NOT BEGIN - NOT ADD QUESTION!")
  { PW.endPoll(2)
    PW.addQuestion(2, "new quest", "open")}}

  test("/add_question does not add question from simple user")
  {assertResult("ERROR BY ACCESS - NOT ADD QUESTION!")
   { PW.beginPoll(4, 1)
     PW.addQuestion(1, "new quest", "open")}}



  //Тесты для delete_question:
  test("/delete_question does not delete question for not launched poll")
  {assertResult("ERROR BY NOT BEGIN - NOT DELETE QUESTION!") {
    PW.deleteQuestion(1, 2)
  }}

  test("/delete_question delete some good question")
  {assertResult("DELETE QUESTION OK!") {
    PW.beginPoll(4, 2)
    PW.deleteQuestion(1, 2)
  }}

  test("/delete_question does not delete question for wrong number of question")
  {assertResult("ERROR BY POLL DOES NOT HAVE THIS QUESTION - NOT DELETE QUESTION!") {
    PW.deleteQuestion(10, 2)
  }}

  test("/delete_question does not delete question from simple user")
  {assertResult("ERROR BY ACCESS - NOT DELETE QUESTION!") {
    PW.beginPoll(4, 1)
    PW.deleteQuestion(2, 1)
  }}

  //Тесты для answer:
  test("/answer does not answer for not launched poll")
  {assertResult("ERROR BY NOT BEGIN - NOT ANSWER!") {
    PW.endPoll(2)
    PW.answer("a", 2, 2) }}

  test("/answer answers for some good quest (open)")
  {assertResult("ANSWER OK!") {
    PW.beginPoll(4, 2)
    PW.answer("a", 2, 2)
  }}

  test("/answer does not answer for passed question")
  {assertResult("ERROR BY POLL ALREADY PASSED - NOT ANSWER!")
  { PW.answer("a", 2, 2) }}

  test("/answer answers for some good quest (choice)")
  {assertResult("ANSWER OK!") {PW.answer("1", 3, 2)}}

  test("/answer answers for some good quest (multi)")
  {assertResult("ANSWER OK!") {PW.answer("2 3", 4, 2)}}

  test("/answer answers for some good quest from simple user")
  {assertResult("ANSWER OK!")
  { PW.beginPoll(4, 1)
    PW.answer("b", 2 , 1)}}

  test("/answer does not answer for question with wrong number")
  {assertResult("ERROR BY POLL DOES NOT HAVE THIS QUESTION - NOT ANSWER!")
  { PW.answer("b", 8 , 1)}}

  test("/answer does not answer for question (multi) with repetitive numbers")
  {assertResult("ERROR BY REPETITIVE NUMBERS OF VARIANT - NOT ANSWER!")
  { PW.answer("1 1", 4, 1)}}

  test("/answer does not answer for question (multi) with wrong number of variant")
  {assertResult("ERROR BY WRONG NUMBER OF VARIANT - NOT ANSWER!")
  {PW.answer("9 1", 4, 1)}}

  test("/answer does not answer for question (choice) with wrong number of variant")
  {assertResult("ERROR BY WRONG NUMBER OF VARIANT - NOT ANSWER!")
  {PW.answer("9", 3, 1)}}

}



