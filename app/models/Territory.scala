package models

class Territory(val name: String, var ownerName: String = "", var armyCount: Int = 0) {

  def incrementArmy(amount: Int) = {
    armyCount += amount
  }

  def decrementArmy(amount: Int) = {
    armyCount -= amount
  }

  def getOwner = {
    if (ownerName.equals("")) "no one" else ownerName
  }

  def setOwner(newOwner: String) = {
    ownerName = newOwner
  }

  final override def equals(other: Any) = other match {
    case that: Territory => name == that.name
    case _ => false
  }

  final override def hashCode = (name).##

}
