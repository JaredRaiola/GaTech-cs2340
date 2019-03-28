package controllers

import models.{Player, Territory}

object GameData {
  var players = scala.collection.mutable.ArrayBuffer(new Player("", 0, 0))
  var currPlayerIndex: Int = 0


  var turnCounter: Int = 0
  val terrArray = new Array[Territory](48)
  for (i <- terrArray.indices) {
    var continent:Int = 0
    if (i >= 0 && i <= 15) continent = 0
    if (i >= 16 && i <= 31) continent = 1
    if (i >= 32 && i <= 47) continent = 2
    terrArray(i) = new Territory("TerritoryName" + i, "", 0, continent)
  }
}
