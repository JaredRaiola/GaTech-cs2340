package controllers


import models._

import javax.inject.Inject

import play.api.mvc._




import scala.collection.mutable.ArrayBuffer


class ArmySetUpController @Inject()(cc: MessagesControllerComponents)(var players: ArrayBuffer[Player], val terrCont: TerritoryController) extends MessagesAbstractController(cc) {


  //index into the ArrayBuffer which player is active
  private var currPlayerIndex: Int = 0

  def newTurn = {
    if (currPlayerIndex == players.length) {
      currPlayerIndex = 0
    } else {
      currPlayerIndex += 1
    }
  }

  def checkTerritory(terrIndex: Int): Boolean = {
    terrCont.terrArray(terrIndex).ownerName != ""
  }

  def claimTerritory(terrIndex: Int) = //Action {
    {//
    if (checkTerritory(terrIndex)) {
      terrCont.terrArray(terrIndex).incrementArmy(1)
      terrCont.terrArray(terrIndex).setOwner(players(currPlayerIndex).name)
      players(currPlayerIndex).decrementArmyCount(1)
      Ok(views.html.armyview(players, terrCont))
    } else {
     // Redirect(routes.ArmySetUpController.claimTerritory(terrIndex)).flashing(
       // "Warning" -> "Selected Territory has already been claimed")
    }

  }


}
