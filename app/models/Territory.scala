package models

class Territory(val name: String, var ownerName: String = "", var armyCount: Int = 0, val cont: Int = 0) {

  def getAdjList(): List[Int] = {
    val terrIndex: Int = name.charAt(name.length() - 1).toInt - 48
    val preFilteredList: List[Int] = List(terrIndex + 1, terrIndex - 1, terrIndex + 8, terrIndex - 8)
    preFilteredList.filter((x: Int) => x >= 0 && x <= 47)
  }

  def incrementArmy(amount: Int):Unit = {
    armyCount += amount
  }

  def decrementArmy(amount: Int):Unit = {
    armyCount -= amount
  }

  def getOwner:String = {
    if (ownerName.equals("")) "no one" else ownerName
  }

  def setOwner(newOwner: String):Unit = {
    ownerName = newOwner
  }

  final override def equals(other: Any):Boolean = other match {
    case that: Territory => name == that.name
    case _ => false
  }

  final override def hashCode:Int = name.##

}
