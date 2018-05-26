case class Question(name : String,
                    qType: String,
                    variants: Map[Int, String] = Map.empty,
                    results: Map[Int, Int] = Map.empty,
                    setOfNumbers : Iterator[Int] = Stream.from(1).iterator,
                    answers: Map[User, Either[String, List[Int]]] = Map.empty) {

  def getInfoForView() : String = {
    if (qType == "open") "OPEN QUESTION DOES NOT HAVE VARIANTS \n"
    else {
      val variants = for (v <- this.variants) yield s"${v._1} : ${v._2} \n"
      variants.mkString("")
    }
  }

  def getInfoForResult() : String = {
    val res = (for (user <- answers.keys;
              answ <- answers.values)
              yield s"${user.name} ${user.secondName} ANSWERS: ${getAnswer(answ)} \n").mkString("")
    if (qType == "open") {
      if (res == "") "QUESTION DOES NOT HAVE ANSWERS\n"
      else res
    }
    else {
      val counts = (for ((a, k) <- results) yield s"$k PEOPLE CHOSE VARIANT $a \n").mkString("")
      if (res == "") "QUESTION DOES NOT HAVE ANSWERS\n"
      else res+"\nRESULTS:\n"+counts
    }
  }

  def getAnonInfoForResult(): String = {
    val res = (for (user <- answers.keys;
                    answ <- answers.values) yield s"UNKNOWN USER ANSWERS: ${getAnswer(answ)} \n").mkString("")
    if (qType == "open") {
      if (res == "") "QUESTION DOES NOT HAVE ANSWERS\n"
      else res
    }
    else {
      val counts = (for ((a, k) <- results) yield s"$k PEOPLE CHOSE VARIANT $a \n").mkString("")
      if (res == "") "QUESTION DOES NOT HAVE ANSWERS\n"
      else res+"\nRESULTS:\n"+counts
    }
  }

  def getAnswer(a : Either[String, List[Int]]) : String = {
    a match {
      case Left(answ) => answ
      case Right(answ) => answ.mkString(", ")
    }
  }

  def check_answer(answer : Either[String, List[Int]]): String = {
    answer match {
      case Left(_) => "ANSWER OK!"
      case Right(allAnswers) => {
        val goodAnswers = allAnswers.filter(a => variants.contains(a))
        if (goodAnswers.length != allAnswers.length) "ERROR BY WRONG NUMBER OF VARIANT - NOT ANSWER!"
        else if (qType == "choice") {
          if (allAnswers.length == 1) "ANSWER OK!"
          else "ERROR BY EXTRA VARIANTS FOR CHOICE - NOT ANSWER!"
        }
        else if (goodAnswers.toSet.size == goodAnswers.length) "ANSWER OK!"
        else "ERROR BY REPETITIVE NUMBERS OF VARIANT - NOT ANSWER!"
      }
    }
  }
}