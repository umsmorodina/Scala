import java.util.Date

import shapeless._

case class Poll(id : Int,
                name : String,
                annon : Boolean,
                visibility : Boolean,
                timesUp : Option[Date] ,
                timesDown : Option[Date],
                admin: Int,
                launch : Boolean = false,
                used : Boolean = false,
                questions: Map[Int, Question] = Map.empty,
                setOfNumbers : Iterator[Int] = Stream.from(1).iterator) {

  def getResult(): String = {
    if ((!visibility) || ((visibility) && (!launch) && (used))) {
      if (annon) {
        (for (q <- questions.keys.toList) yield s"QUESTION WITH NAME ${questions.get(q).get.name}: \n${
          questions.get(q).get.getAnonInfoForResult() }").mkString("\n")
      }
      else {
          (for (q <- questions.keys.toList) yield s"QUESTION WITH NAME ${questions.get(q).get.name}: \n${
            questions.get(q).get.getInfoForResult()}").mkString("\n")
        }
      }
    else "ERROR BY NOT STOPPED POLL- NOT RESULT!"
  }

  def answer(answer: Either[String, List[Int]], number: Int, user : User) : Poll => (Poll, String)  = {
    p => val oldQuest = p.questions.get(number).get
        val result = oldQuest.check_answer(answer)
        if (result == "ANSWER OK!") {
            val newQuest = oldQuest.
            copy(answers = oldQuest.answers updated(user, answer), results = createResult(oldQuest, answer))
            val resultPoll = lens[Poll].modify(p)(_.copy(questions = questions updated(number, newQuest)))
            (resultPoll, result)
        }
        else (p, result)
  }

  def createResult(oldQuest : Question, answer : Either[String, List[Int]]): Map[Int, Int] = {
    answer match {
      case Left(_) => Map.empty
      case Right(res) => oldQuest.results.map(r => if (res.contains(r._1)) (r._1, r._2 + 1) else (r._1, r._2))
    }
  }
  
  def deleteQuestion(number : Int) : Poll => Poll = { p =>
    lens[Poll].modify(p)(_.copy(questions=questions - number))
  }

  def addQuestion(name: String, qtype: String, variants: List[String] = List.empty): Poll => (Poll, Int) = {
    val newQuest = if (variants.nonEmpty) {
      val varMap = createMapVariants(variants)
      val resMap = varMap.map(v => (v._1, 0))
      Question(name, qtype, varMap, resMap)
    }
    else Question(name, qtype)
    val newNumber = setOfNumbers.next()
    p => (lens[Poll].modify(p)(_.copy(questions=questions updated(newNumber, newQuest))), newNumber)
  }

  def createMapVariants(v : List[String]): Map[Int, String] = {
    val setOfNumbers = Stream.from(1).iterator
    v.map(variant => (setOfNumbers.next() -> variant)).toMap
  }

  def isPassedByUser(number : Int, user : User) : Boolean = {
   this.questions.get(number).get.answers.contains(user)
  }

  def isBeginByAdmin(userId : Int) : Boolean = {
    this.admin.equals(userId)
  }

  override def toString: String = {
    val firstPollType : String = {
      if (annon) "anonymous"
      else "public"}

    val secondPollType : String = {
      if (visibility) "afterstop"
      else "continuous"}

    val startTime : String = {
      if (timesUp.isEmpty) "without start time"
      else timesUp.get.toString
    }

    val finishTime : String = {
      if (timesDown.isEmpty) "without finish time"
      else timesDown.get.toString
    }

    val condition : String = {
      if (launch)
        if (used) "passed and started"
        else "started but not passed"
      else "not started"
    }

      s"INFO BY POLL #$id: \n" +
        s" NAME: $name \n ANONYMOUS: $firstPollType \n VISIBILITY: $secondPollType \n " +
        s"START: $startTime \n FINISH: $finishTime \n CONDITION: $condition \n" + getInfoAboutQuestion()
  }

  def getInfoAboutQuestion(): String = {
      val a = for (number <- questions.keys) yield s"$number ) QUESTION WITH NAME ${questions.get(number).get.name} : \n${
        questions.get(number).get.getInfoForView()
      }"
    a.mkString("")
  }
}
