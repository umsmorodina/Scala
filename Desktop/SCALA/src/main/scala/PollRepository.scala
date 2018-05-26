

object PollRepository extends Repository {
  var polls: Map[Int, Poll] = Map.empty
  val setOfId = Stream.from(1).iterator

  override def store(poll: Poll): Unit = polls += (poll.id -> poll)

  override def get(id: Int): Option[Poll] = polls.get(id)

  override def all(): Seq[Poll] = polls.values.toList

  override def delete(id: Int): Unit = polls -= id

  override def isLaunch(id: Int): Boolean = polls(id).launch

  override def isContains(id: Int): Boolean = polls.contains(id)

  override def hasStart(id: Int): Boolean =
    !polls(id).timesUp.isEmpty

  override def start(id: Int): Unit = {
    val poll = polls(id)
    polls += (poll.id -> poll.copy(launch = true, used = false))}

  override def hasEnd(id: Int): Boolean =
    !polls(id).timesDown.isEmpty

  override def end(id: Int): Unit = {
    val poll = polls(id)
    polls += (poll.id -> poll.copy(launch = false, used = true))}

  override def getIndex(): Int = {setOfId.next()}

  override def isAdmin(id : Int, userId : Int): Boolean = polls(id).admin == userId
}