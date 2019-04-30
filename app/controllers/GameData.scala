package controllers

import models.{Player, Territory}

object GameData {
  var players: scala.collection.mutable.ArrayBuffer[Player] = scala.collection.mutable.ArrayBuffer(new Player(""))

  var currPlayerIndex: Int = 0

  var randomNumberMaker: scala.util.Random = new scala.util.Random()

  val numTerritories = 48
  var turnCounter: Int = 0
  val terrArray = new Array[Territory](numTerritories)
  for (i <- terrArray.indices) {
    var continent:Int = 0
    if (i >= 0 && i <= 15) continent = 0
    if (i >= 16 && i <= 31) continent = 1
    if (i >= 32 && i <= 47) continent = 2
    terrArray(i) = new Territory("TerritoryName" + i, "", 0, continent, i)
  }

  var attackDiceRoll: Array[Int] = Array(0,0,0)
  var defenceDiceRoll: Array[Int] = Array(0,0)

  def isAllDigits(x: String): Boolean = x forall Character.isDigit
  def isValidNum(str: String): Boolean = str != "" && isAllDigits(str)
  def isInRange(str: String): Boolean = isValidNum(str) && str.toInt <= 47 && str.toInt >= 0

  def startStateIncomplete: Boolean = terrArray.isEmpty || players.size < 3

  def checkForWin(playerIndex: Int): Boolean = {
    val sumOfActive = for (player <- players) yield {
      if (player.active) 1 else 0
    }
    val sum = sumOfActive.foldLeft(0)(_ + _)
    sum == 0 && players(playerIndex).active
  }

  def setInactive(playerIndex: Int): Unit = {
    players(playerIndex).active = false
  }

  def getAttackRolls = {
    var rolls = ""
    for (w <- attackDiceRoll) {
      if (w != 0) {
        rolls += w + "  "
      }
    }
    rolls
  }

  def getDefenceRolls = {
    var rolls = ""
    for (w <- defenceDiceRoll) {
      if (w != 0) {
        rolls += w + "  "
      }
    }
    rolls
  }


  def isAttackLoop: Boolean = {
    turnCounter >= players.size
  }

  def getCurrentPlayer: Player = {
    players(currPlayerIndex)
  }

  def setCurrentPlayerIndex(index: Int): Unit = {
    currPlayerIndex = index
  }

  def newTurn:Unit = {
    turnCounter += 1
    if (currPlayerIndex == players.length - 1) {
      currPlayerIndex = 0
    } else {
      currPlayerIndex += 1
    }
  }

  def getNextPlayerIndex(currPlayerIndex: Int): Int = (currPlayerIndex + 1) % players.length

  def checkTerritoryAdjacency(terr1Index: Int, terr2Index: Int): Boolean = {
    //need to make the territory map graph
    val difference = terr1Index - terr2Index
    Math.abs(difference) == 8 || Math.abs(difference) == 1
  }

  def doesCurrPlayerOwnTerr(terr: Territory): Boolean = {
    terr.getOwner == GameData.players(GameData.currPlayerIndex).name
  }

  def calculateTerritoriesOwned(playerIndex: Int): Int = {
    var territoriesOwned = 0
    for (i <- terrArray.indices) {
      if (terrArray(i).getOwner == players(playerIndex).name) {
        territoriesOwned += 1
      }
    }
    territoriesOwned
  }

  def calculateContinentsArray(index: Int):Array[Int] = {
    var continentsCount = Array(0, 0, 0)
    for (i <- terrArray.indices) {
      if (terrArray(i).getOwner == players(index).name) {
        continentsCount(terrArray(i).cont) += 1
      }
    }
    continentsCount
  }

  def calculateContinentsOwned(index: Int):Int = {
    var continentsCount = calculateContinentsArray(index)
    var numContinentsOwned = 0
    for (i <- continentsCount.indices) {
      if (continentsCount(i) == 16) {
        numContinentsOwned += 1
      }
    }
    numContinentsOwned
  }

  def assignNewArmies: Unit = {
    val index = GameData.currPlayerIndex
    val newArmies = GameData.calculateNewArmies(index)
    GameData.players(index).incrementArmyCount(newArmies)
  }

  def calculateNewArmies(index: Int): Int = {
    var newArmies = 0
    var territoriesOwned = calculateTerritoriesOwned(index)
    var continentsCount = calculateContinentsArray(index)

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

    if (territoriesOwned < 9) {
      newArmies += 3
    } else {
      newArmies += territoriesOwned / 3
    }

    newArmies
  }
}
