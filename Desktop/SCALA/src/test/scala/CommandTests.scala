import java.text.SimpleDateFormat
import java.util.Date

import org.scalatest.FlatSpec


class CommandTests extends FlatSpec {
  val PW = new PollWorker
  val defaultDate: Date = new SimpleDateFormat("HH:mm:ss,yy:MM:dd").parse("00:00:00,11:01:01")

  "not start of work with bot" should "be right" in {
    assertResult("POLLS DON’T EXIST") {PW.list().head}
    assertResult("ERROR BY ID - NOT DELETE!") {PW.deletePoll(75, 1)}
    assertResult("ERROR BY ID - NOT START!") {PW.startPoll(100, 1)}
    assertResult("ERROR BY ID - NOT STOP!") {PW.stopPoll(20, 1)}
  }

  "create first poll" should "be right" in {
    PW.createPoll("FIRST", true, true, None, None, 1)
    assertResult(List[String]("POLL FIRST HAS ID = 1")) {PW.list}
    assertResult("START OK!") {PW.startPoll(1, 1)}
    assertResult("ERROR BY LAUNCH - NOT DELETE!") {PW.deletePoll(1, 1)}
    assertResult("STOP OK!") {PW.stopPoll(1, 1)}
    assertResult("DELETE OK!") {PW.deletePoll(1, 1)}
  }

  "next works" should "be right" in {
    val id = PW.createPoll("SECOND", true, true, Some(defaultDate),
      Some(defaultDate), 1).toInt
    assertResult("DELETE OK!") {PW.deletePoll(id, 1)}
    assertResult("ERROR BY ID - NOT DELETE!") {PW.deletePoll(id + 1, 1)}
    assertResult("POLLS DON’T EXIST") {PW.list().head}
  }


  "context works" should "be right" in {
    val id = PW.createPoll("THIRD", true, true, None, None, 1).toInt
    assertResult("BEGIN OK!") {PW.startPoll(id, 1)
      PW.beginPoll(id, 1)
    }
    assertResult("ERROR BY ID - NOT BEGIN!") {PW.beginPoll(id + 1, 1)}
    val nextId = PW.createPoll("FOURTH", true, true, None, None, 2).toInt
    assertResult("END OK!") {PW.endPoll(1)}
    assertResult("ERROR BY NOT LAUNCH - NOT BEGIN!") {PW.beginPoll(nextId, 1)}
    assertResult("ERROR BY NOT BEGIN - NOT END!") {PW.endPoll(3)}


    assertResult("BEGIN OK!") {PW.startPoll(nextId, 2)
      PW.beginPoll(nextId, 2)}
    assertResult("1") {PW.addQuestion(2, "first_quest")}
    assertResult("2") {PW.addQuestion(2, "new quest", "open")}
    assertResult("3")
      {PW.addQuestion(2, "new quest", "choice", List[String]("a", "b", "c"))}
    assertResult("4")
      {PW.addQuestion(2, "new quest", "multi", List[String]("a", "b", "c"))}
    assertResult("ERROR BY NOT BEGIN - NOT ADD QUESTION!") {PW.endPoll(2)
      PW.addQuestion(2, "new quest", "open")}
    assertResult("ERROR BY ACCESS - NOT ADD QUESTION!") {PW.beginPoll(nextId, 1)
      PW.addQuestion(1, "new quest", "open")}

    assertResult("ERROR BY NOT BEGIN - NOT DELETE QUESTION!") {PW.deleteQuestion(1, 2)}
    assertResult("DELETE QUESTION OK!") {PW.beginPoll(nextId, 2)
      PW.deleteQuestion(1, 2)}
    assertResult("ERROR BY POLL DOES NOT HAVE THIS QUESTION - NOT DELETE QUESTION!") {
      PW.deleteQuestion(10, 2)}
    assertResult("ERROR BY ACCESS - NOT DELETE QUESTION!") {PW.beginPoll(nextId, 1)
    PW.deleteQuestion(nextId, 1)}

    val secondUser = User(2, "name", "")
    assertResult("ERROR BY NOT BEGIN - NOT ANSWER!") {PW.endPoll(2)
    PW.answer("a", 2, secondUser) }
    assertResult("ANSWER OK!") {PW.beginPoll(nextId, 2)
    PW.answer("a", 2, secondUser)}
    assertResult("ERROR BY POLL ALREADY PASSED - NOT ANSWER!")
      {PW.answer("a", 2, secondUser)}
    assertResult("ANSWER OK!") {PW.answer("1", 3, secondUser)}
    assertResult("ANSWER OK!") {PW.answer("2 3", 4, secondUser)}

    val firstUser = User(1, "name", "")
    assertResult("ANSWER OK!") { PW.beginPoll(nextId, 1)
      PW.answer("b", 2 , firstUser)}
    assertResult("ERROR BY POLL DOES NOT HAVE THIS QUESTION - NOT ANSWER!")
      {PW.answer("b", 8 , firstUser)}
    assertResult("ERROR BY REPETITIVE NUMBERS OF VARIANT - NOT ANSWER!")
      {PW.answer("1 1", 4, firstUser)}
    assertResult("ERROR BY WRONG NUMBER OF VARIANT - NOT ANSWER!")
      {PW.answer("9 1", 4, firstUser)}
    assertResult("ERROR BY WRONG NUMBER OF VARIANT - NOT ANSWER!")
      {PW.answer("9", 3, firstUser)} }
}



