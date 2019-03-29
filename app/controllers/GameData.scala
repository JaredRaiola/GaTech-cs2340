package controllers

import models.{Player, Territory}

object GameData {
  var players = scala.collection.mutable.ArrayBuffer(new Player("", 0, 0))
  var currPlayerIndex: Int = 0

  val numTerritories = 48
  var turnCounter: Int = 0
  val terrArray = new Array[Territory](numTerritories)
  for (i <- terrArray.indices) {
    var continent:Int = 0
    if (i >= 0 && i <= 15) continent = 0
    if (i >= 16 && i <= 31) continent = 1
    if (i >= 32 && i <= 47) continent = 2
    terrArray(i) = new Territory("TerritoryName" + i, "", 0, continent)
  }

  def getCurrentPlayer = {
    players(currPlayerIndex)
  }

  def calculateNewArmies(index: Int) = {
    var newArmies = 0
    var territoriesOwned = 0
    var continentsCount = Array(0, 0, 0)
    for (i <- terrArray.indices) {
      if (terrArray(i).getOwner == players(index).name) {
        territoriesOwned += 1
        continentsCount(terrArray(i).cont) += 1
      }
    }

    //if player owns continent 0
    if (continentsCount(0) == 16) {
      newArmies += 3
    }
    if (continentsCount(1) == 16) {
      newArmies += 7
    }
    if (continentsCount(2) == 16) {
      newArmies += 5
    }

    if (players(index).territoryCount < 9) {
      newArmies += 3
    } else {
      newArmies += players(index).territoryCount / 3
    }
  }
}
