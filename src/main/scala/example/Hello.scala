package example

object Hello {

  val app = new AppComponents
  import app._

  def main(args: Array[String]): Unit = {
    consoleChat.run()
  }
}
