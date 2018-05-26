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
          if (questions.get(q).get.qType == "open") {
            val res = (for (r <- questions.get(q).get.answers.values) yield s"UNKNOWN USER ANSWERS: $r \n")
            .mkString("")
            if (res == "") "QUESTION DOES NOT HAVE ANSWERS"
            else res
          }
          else (for ((a, k) <- questions.get(q).get.results) yield s"$k PEOPLE CHOSE VARIANT $a \n").mkString("")
        }").mkString("\n")
      }
      else {
          (for (q <- questions.keys.toList) yield s"QUESTION WITH NAME ${questions.get(q).get.name}: \n${
          if (questions.get(q).get.qType == "open") {
            val res = (for (id <- questions.get(q).get.answers.keys;
                  answ <- questions.get(q).get.answers.values) yield s"USER WITH ID $id ANSWERS: $answ \n").mkString("")
            if (res == "") "QUESTION DOES NOT HAVE ANSWERS"
            else res
          }
          else (for ((a, k) <- questions.get(q).get.results)
            yield s"$k PEOPLE CHOSE VARIANT $a \n").mkString("")}").mkString("\n")
        }
      }
    else "ERROR BY NOT STOPPED POLL- NOT RESULT!"
  }

  def answer(answer: String, number: Int, userId: Int) : Poll => (Poll, String)  = {
    p => val oldQuest = p.questions.get(number).get
        val result = check_answer(answer, oldQuest)
        if (result == "ANSWER OK!") {
            val newQuest = oldQuest.
            copy(answers = oldQuest.answers updated(userId, answer), results = createResult(oldQuest, answer))
            val resultPoll = lens[Poll].modify(p)(_.copy(questions = questions updated(number, newQuest)))
            (resultPoll, result)
        }
        else (p, result)
  }

  def check_answer(answer : String, question : Question): String = {
    question.qType match {
      case "open" => "ANSWER OK!"
      case _ => {
        val allAnswers = answer.split(" ")
        try {
          val goodAnswers = allAnswers.filter(a => question.variants.contains(a.toInt))
          if (goodAnswers.length != allAnswers.length) "ERROR BY WRONG NUMBER OF VARIANT - NOT ANSWER!"
          else if (question.qType == "choice") {
            if (allAnswers.length == 1) "ANSWER OK!"
            else "ERROR BY EXTRA VARIANTS FOR CHOICE - NOT ANSWER!"
          }
          else if (goodAnswers.toList.toSet.size == goodAnswers.length) "ANSWER OK!"
          else "ERROR BY REPETITIVE NUMBERS OF VARIANT - NOT ANSWER!"
        }
        catch {
          case e: Exception => "ERROR BY NOT INT NUMBERS OF VARIANT - NOT ANSWER!"
        }
      }
    }
  }

  def createResult(oldQuest : Question, answer : String): Map[Int, Int] = {
    if (oldQuest.qType != "open") {
      val results = answer.split(" ")
      oldQuest.results.map(r => if (results.contains(r._1.toString)) (r._1, r._2 + 1) else (r._1, r._2))
      //results.map(variant => (variant.toInt -> oldQuest.results.getOrElse(variant.toInt, 0).+(1))).toMap
    }
    else Map.empty
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

  def isPassedByUser(number : Int, userId : Int) : Boolean = {
   this.questions.get(number).get.answers.contains(userId)
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
        if (questions.get(number).get.qType == "open") "OPEN QUESTION DOES NOT HAVE VARIANTS \n"
        else {
          val variants = for (v <- questions.get(number).get.variants) yield s"${v._1} : ${v._2} \n"
          variants.mkString("")
        }
      }"
    a.mkString("")
  }
}
