package models

class Territory(val name: String, var ownerName: String = "", var armyCount: Int = 0, val cont: Int = 0, val index: Int) {

  def getAdjList(): List[Int] = {
    var preFilteredList: List[Int] =
    if (index % 8 == 0) {
      List(index + 1, index + 8, index - 8)
    } else if ((index % 7 == 1 && index != 1) || index == 7) {
      List(index - 1, index + 8, index - 8)
    } else {
      List(index + 1, index - 1,  index + 8, index - 8)
    }
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
