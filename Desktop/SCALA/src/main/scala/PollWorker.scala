import java.util.Date
import shapeless._

class PollWorker() {

  def createPoll(name: String, annon: Boolean, visibility: Boolean,
                 timesUp: Option[Date], timesDown: Option[Date], userID: Int): String = {
    val id = PollRepository.getIndex()
    PollRepository.store(Poll(id, name, annon, visibility, timesUp, timesDown, userID))
    id.toString
  }


  def list(): Seq[String] =
    if (PollRepository.all().isEmpty) List[String]("POLLS DON’T EXIST")
    else for (i <- PollRepository.all()) yield "POLL " + i.name + " HAS ID = " + i.id


  def deletePoll(id: Int, userID: Int): String =
    if (!PollRepository.isContains(id)) "ERROR BY ID - NOT DELETE!"
    else if (PollRepository.isLaunch(id)) "ERROR BY LAUNCH - NOT DELETE!"
    else if (!PollRepository.isAdmin(id, userID)) "ERROR BY ACCESS - NOT DELETE"
    else {
      PollRepository.delete(id)
      "DELETE OK!"
    }


  def startPoll(id: Int, userId: Int): String =
    if (!PollRepository.isContains(id)) "ERROR BY ID - NOT START!"
    else if (PollRepository.hasStart(id)) "ERROR BY TIME - NOT START!"
    else if (!PollRepository.isAdmin(id, userId)) "ERROR BY ACCESS - NOT START!"
    else {
      PollRepository.start(id)
      "START OK!"
    }


  def stopPoll(id: Int, userId: Int): String =
    if (!PollRepository.isContains(id)) "ERROR BY ID - NOT STOP!"
    else if (PollRepository.hasEnd(id)) "ERROR BY TIME - NOT STOP!"
    else if (!PollRepository.get(id).get.launch) "ERROR BY NOT LAUNCH - NOT STOP!"
    else if (!PollRepository.isAdmin(id, userId)) "ERROR BY ACCESS - NOT STOP!"
    else {
      PollRepository.end(id)
      "STOP OK!"
    }


  def result(id: Int): String =
    if (!PollRepository.isContains(id)) "ERROR BY ID - NOT RESULT"
    else PollRepository.get(id).get.getResult()


  def beginPoll(id: Int, userId: Int): String =
    if (!PollRepository.isContains(id)) "ERROR BY ID - NOT BEGIN!"
    else if (!PollRepository.get(id).get.launch) "ERROR BY NOT LAUNCH - NOT BEGIN!"
    else {
      CurrentPolls.store(id, userId)
      "BEGIN OK!"
    }


  def endPoll(userId: Int): String =
    if (!CurrentPolls.isContains(userId)) "ERROR BY NOT BEGIN - NOT END!"
    else {
      CurrentPolls.delete(userId)
      "END OK!"
    }


  def viewPoll(userId: Int): String =
    if (CurrentPolls.isContains(userId)) CurrentPolls.get(userId).get.toString()
    else "ERROR BY NOT BEGIN - NOT VIEW!"


  def deleteQuestion(numberQuest: Int, userId: Int): String =
    if (CurrentPolls.isContains(userId)) {
      val oldPoll = CurrentPolls.get(userId).get
      if (oldPoll.isBeginByAdmin(userId)) {
        if (oldPoll.questions.contains(numberQuest)) {
          val modifiedPoll, numb = oldPoll.deleteQuestion(numberQuest)(oldPoll)
          CurrentPolls.store(modifiedPoll.id, userId)
          PollRepository.store(modifiedPoll)
          "DELETE QUESTION OK!"
        }
        else "ERROR BY POLL DOES NOT HAVE THIS QUESTION - NOT DELETE QUESTION!"
      }
      else "ERROR BY ACCESS - NOT DELETE QUESTION!"
    }
    else "ERROR BY NOT BEGIN - NOT DELETE QUESTION!"


  def answer(answer: String, number: Int, userId: Int): String =
    if (CurrentPolls.isContains(userId)) {
      val oldPoll = CurrentPolls.get(userId).get
      if (oldPoll.questions.contains(number)) {
        if (!oldPoll.isPassedByUser(number, userId)) {
          val modifiedPoll = oldPoll.answer(answer, number, userId)(oldPoll)
          if (modifiedPoll._2 == "ANSWER OK!") {
            PollRepository.store(modifiedPoll._1)
            CurrentPolls.store(modifiedPoll._1.id, userId)
          }
          modifiedPoll._2
        }
        else "ERROR BY POLL ALREADY PASSED - NOT ANSWER!"
      }
      else "ERROR BY POLL DOES NOT HAVE THIS QUESTION - NOT ANSWER!"
    }
    else "ERROR BY NOT BEGIN - NOT ANSWER!"


  def addQuestion(userId: Int, name: String, qType: String = "open",
                  variantsSeq: Seq[String] = List.empty): String =
    if (CurrentPolls.isContains(userId)) {
      val oldPoll = CurrentPolls.get(userId).get
      if (oldPoll.isBeginByAdmin(userId)) {
        val modifiedPoll = oldPoll.addQuestion(name, qType, variantsSeq.toList)(oldPoll)
        PollRepository.store(modifiedPoll._1)
        CurrentPolls.store(modifiedPoll._1.id, userId)
        modifiedPoll._2.toString
      }
      else "ERROR BY ACCESS - NOT ADD QUESTION!"
    }
    else "ERROR BY NOT BEGIN - NOT ADD QUESTION!"

}