package controllers

import models.{Player, Territory}

object GameData {
  var players = scala.collection.mutable.ArrayBuffer(new Player("", 0, 0))
  var currPlayerIndex: Int = 0


  var turnCounter: Int = 0
  val terrArray = new Array[Territory](48)
  for (i <- terrArray.indices) {
    terrArray(i) = new Territory("TerritoryName" + i, "", 0)
  }
}
