package controllers

import models.{Player, Territory}

object GameData {
  var players = scala.collection.mutable.ArrayBuffer(new Player("", 0, 0))
  var currPlayerIndex: Int = 0

  var randomNumberMaker = new scala.util.Random(55)

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

  var attackDiceRoll: Array[Int] = Array(0,0,0)
  var defenceDiceRoll: Array[Int] = Array(0,0)

  def isAllDigits(x: String) = x forall Character.isDigit
  def isValidNum(str: String) = str != "" && isAllDigits(str)
  def isInRange(str: String) = isValidNum(str) && str.toInt <= 47 && str.toInt >= 0

 def startStateIncomplete = terrArray.isEmpty || players.size < 3


  def isAttackLoop: Boolean = {
    turnCounter >= players.size
  }

  def getCurrentPlayer = {
    players(currPlayerIndex)
  }

  def newTurn:Unit = {
    turnCounter += 1
    if (currPlayerIndex == players.length - 1) {
      currPlayerIndex = 0
    } else {
      currPlayerIndex += 1
    }
    //to keep things randomer
    randomNumberMaker.nextInt(10)
  }


  def checkTerritoryAdjacency(terr1: Territory, terr2: Territory): Boolean = {
    //need to make the territory map graph
    true
  }

  def doesCurrPlayerOwnTerr(terr: Territory): Boolean = {
    terr.getOwner == GameData.players(GameData.currPlayerIndex).name
  }

  def calculateTerritoriesOwned(index: Int) = {
    var territoriesOwned = 0
    for (i <- terrArray.indices) {
      if (terrArray(i).getOwner == players(index).name) {
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

  def assignNewArmies = {
    var index = GameData.currPlayerIndex
    var newArmies = GameData.calculateNewArmies(index)
    GameData.players(index).incrementArmyCount(newArmies)
  }

  def calculateNewArmies(index: Int) = {
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
