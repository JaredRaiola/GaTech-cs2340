package models

import controllers.GameData

class Player(val name: String, var armyBinCount: Int = 0, var turnMode: Int = 0, var territoryCount: Int = 0 ) {
  //turn mode 0 is not their turn, 1 is place armies, 2 is attack, 3 is move, then 0
  //  def updateBinCount   depending on how many territories they own

  final override def equals(other: Any):Boolean = other match {
    case that: Player => name == that.name
    case _ => false
  }

  final override def hashCode:Int = name.##

  def selectMyTerritory(terr: Territory): Boolean = {
    terr.ownerName.equals(this.name)
  }

  def setArmyCount(count: Int):Unit = {
    armyBinCount = count
  }

  def decrementArmyCount(amount: Int):Unit = {
    armyBinCount -= amount
  }

  def incrementArmyCount(amount: Int):Unit = {
    armyBinCount += amount
  }

  def decrementTerritoryCount(amount: Int):Unit = {
    territoryCount -= amount
  }

  def incrementTerritoryCount(amount: Int):Unit = {
    territoryCount += amount
  }

  def selectEnemyTerritory(terr: Territory): Boolean = {
    !terr.ownerName.equals(this.name)
  }

  def continentBonus(): Int = {

    var total = 0
    var owned = true

    for(i <- 0 to 15) {

      if(GameData.terrArray(i).ownerName != name) {

        owned = false

      }
    }

    if(owned) {
      total += 5
    }

    owned = true;

    for(i <- 16 to 31) {
      if(GameData.terrArray(i).ownerName != name) {

        owned = false

      }
    }

    if(owned) {
      total += 5
    }

    owned = true;

    for(i <- 32 to 47) {
      if(GameData.terrArray(i).ownerName != name) {

        owned = false

      }
    }

    if(owned) {
      total += 5
    }

    owned = true;


    total

  }
}
