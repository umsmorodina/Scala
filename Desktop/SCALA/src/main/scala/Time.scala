import java.util.Date

object Time extends Runnable{
  override def run(): Unit = {
    while (true) {
      PollRepository.all.filter(p => PollRepository.hasEnd(p.id))
        .filter(p => new Date().after(p.timesDown.getOrElse(new Date())))
        .map(p => PollRepository.end(p.id))
      PollRepository.all.filter(p => PollRepository.hasStart(p.id))
        .filter(p => (!p.used) && new Date().after(p.timesUp.getOrElse(new Date())))
        .map(p => PollRepository.start(p.id))
      Thread.sleep(1000)
    }
  }
}