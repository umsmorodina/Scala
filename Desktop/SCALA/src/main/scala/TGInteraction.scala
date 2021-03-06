import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._


object TGInteraction extends App with TelegramBot with Polling with Commands {
  lazy val token = "551139940:AAEUlqHt9ZjQfc-53zBgiSel6FKpjjhvRKU"
  //onCommand('start) { implicit msg => reply("My token is SAFE!") }

  override def receiveMessage(msg: Message): Unit = {
    for (text <- msg.text)
      request(SendMessage(msg.source, CommandCreator(msg.text.getOrElse(""),
        User(msg.from.get.id, msg.from.get.firstName, msg.from.get.lastName.getOrElse(""))).makeCommand()))
  }

  TGInteraction.run()
  val timer = new Thread(Time)
  timer.setDaemon(true)
  timer.start()
}