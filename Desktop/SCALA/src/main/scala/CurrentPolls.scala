object CurrentPolls{
  var polls: Map[Int, Poll] = Map.empty

  def store(id : Int, userId : Int): Unit = polls += (userId -> PollRepository.get(id).get)

  def get(id: Int): Option[Poll] = polls.get(id)

  def delete(userId: Int): Unit = polls -= userId

  def isContains(id: Int): Boolean = polls.contains(id)

  def isStarted(userId : Int): Boolean =
    true

  def isStopped(userId : Int): Boolean =
    false

  def isActive(userId : Int): Boolean =
    isStarted(userId) && !isStopped(userId)

  def isVisible(userId : Int): Boolean = {
    val poll = polls.get(userId).get
    poll.visibility && isStarted(userId) || !poll.visibility && isStopped(userId)
  }
}