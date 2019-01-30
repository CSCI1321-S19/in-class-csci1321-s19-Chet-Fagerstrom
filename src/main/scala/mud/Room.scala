package mud

class Room(
  val name: String,
  val desc: String,
  private var items: List[Item],
  private val exits: Array[Int]
  ) {
  def descr(): String = {
    val nVal = if(exits(0)<0) "" else "North"
    val sVal = if(exits(1)<0) "" else "South"
    val eVal = if(exits(2)<0) "" else "East"
    val wVal = if(exits(3)<0) "" else "West"
    val uVal = if(exits(4)<0) "" else "Up"
    val dVal = if(exits(5)<0) "" else "Down"
    val exitsU = Array[String](nVal, sVal, eVal, wVal, uVal, dVal)
    
    val stuff = items.map(i => i.name).mkString(", ")
    
    
    name+"\n"+desc+"\n"+"Exits: "+exitsU.filter(e => e!="").mkString(", ")+ "\n"+"Items: "+ (if(stuff.length == 0) "None" else stuff)
  }
  
  def getExit(dir: Int): Option[Room] = {
    if(exits(dir) == -1) None else Some(Room.rooms(exits(dir)))
  }
  
  def getItem(itemName: String): Option[Item] = {
    val itemNames = items.map(_.name.toLowerCase)
    if(itemNames.contains(itemName)){
      val item = items.filter(i => i.name.toLowerCase == itemName)(0)
      items = items.filter(i => i != item)
      Some(item)
    }
    else{
      None
    }
  }
  
  def dropItem(item: Item): Unit = {
    items ::= item
  }
}

object Room {
  val rooms = readRooms()
  
  def readRooms(): Array[Room] = {
    val source = scala.io.Source.fromFile("map.txt")
    val lines = source.getLines()
    Array.fill(lines.next.trim.toInt)(readRoom(lines))
  }
  
  def readRoom(lines: Iterator[String]): Room = {
    val name = lines.next
    val desc = lines.next
    val items = List.fill(lines.next.trim.toInt) {
      Item(lines.next, lines.next)
    }
    val exits = lines.next.split(",").map(_.trim.toInt)
    new Room(name, desc, items, exits)
  }
}