package mud

class Player(
  private var location: Room,
  private var inventory: List[Item]
  ) {
  
  def inventoryListing(): String = {
    "Inventory:\n" + inventory.map(i => "\t"+i.name + " - " + i.desc).mkString("\n")
  }
  
  def processCommand(command: String): Unit = {
    val cmdU = command.toLowerCase()
    if(cmdU=="look") println(location.descr)
    else if(cmdU=="inv" | cmdU=="inventory") println(inventoryListing())
    else if(cmdU.startsWith("get")){
      val itemName = cmdU.substring(cmdU.indexOf(" ")+1)
      try{
        val item = location.getItem(itemName).get
        addToInventory(item)
        println(item.name + " added to inventory.")
      }catch{
        case ex: NoSuchElementException => {println("No such item here.")}
      }
    }
    else if(cmdU.startsWith("drop")){
      val itemName = cmdU.substring(cmdU.indexOf(" ")+1)
      try{
        val item = getFromInventory(itemName).get
        location.dropItem(item)
        println(item.name + " dropped.")
      }catch{
        case ex: NoSuchElementException => {println("You can't drop something you don't have in your inventory.")}
      }
    }
    else if(cmdU == "exit") println("A wise decision...")
    else if(cmdU.startsWith("n")){move("n"); println(location.descr)}
    else if(cmdU.startsWith("s")){move("s"); println(location.descr)}
    else if(cmdU.startsWith("e")){move("e"); println(location.descr)}
    else if(cmdU.startsWith("w")){move("w"); println(location.descr)}
    else if(cmdU.startsWith("u")){move("u"); println(location.descr)}
    else if(cmdU.startsWith("d")){move("d"); println(location.descr)}
    else if(cmdU == "help") println("north - move north\nsouth - move south\neast - move east\nwest - move west\nup - move up\ndown - move down\nlook - prints description of the room you're in\ninv - displays your inventory\ninventory - displays your inventory\nget [item] - adds item from the room you're in to your inventory\ndrop [item] - drops item from your inventory to the room you're in\nexit - leave the game\nhelp - displays this page. How did you get here if you didn't know this?")
    else(println("Unknown command"))
  }
  
  def getFromInventory(itemName: String): Option[Item] = {
    val itemNames = inventory.map(_.name.toLowerCase)
    if(itemNames.contains(itemName)){
      val item = inventory.filter(i => i.name.toLowerCase == itemName)(0)
      inventory = inventory.filter(i => i != item)
      Some(item)
    }
    else{
      None
    }
  }
  
  def addToInventory(item: Item): Unit = {
    inventory ::= item
  }
  
  def move(dir: String): Unit = {
    val dirU = if(dir.toLowerCase().startsWith("n")) 0 else if(dir.toLowerCase().startsWith("s")) 1 else if(dir.toLowerCase().startsWith("e")) 2 else if(dir.toLowerCase().startsWith("w")) 3 else if(dir.toLowerCase().startsWith("u")) 4 else if(dir.toLowerCase().startsWith("d")) 5 else -1
    val dest = location.getExit(dirU)
    if(dest != None) location = dest.get
  }
}