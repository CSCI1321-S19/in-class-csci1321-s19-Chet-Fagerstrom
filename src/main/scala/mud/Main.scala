package mud

import scala.io.StdIn._

object Main {
  def main(args: Array[String]): Unit = {
    val usr = new Player(Room.rooms(0), List[Item]())
    usr.processCommand("look")
    var cmd = ""
    while(cmd != "exit"){
      cmd = readLine
      usr.processCommand(cmd)
    }
  }
}