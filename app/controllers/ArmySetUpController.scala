package controllers


import javax.inject.Inject

import models._



import play.api.mvc._




import scala.collection.mutable.ArrayBuffer


class ArmySetUpController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {


//(var players: ArrayBuffer[Player], val terrCont: TerritoryController)

  var players: ArrayBuffer[Player] = null
  var terrs: Array[Territory] = null

  def createArmySetUpController(playersInput: ArrayBuffer[Player], terrsInput: Array[Territory]) = {
    players = playersInput
    terrs = terrsInput
  }





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
    terrs(terrIndex).ownerName != ""
  }

  def claimTerritory(terrIndex: Int) = Action { //implicit request: MessagesRequest[AnyContent] =>

    if (checkTerritory(terrIndex) && players(currPlayerIndex).armyBinCount != 0) {
      terrs(terrIndex).incrementArmy(1)
      terrs(terrIndex).setOwner(players(currPlayerIndex).name)
      players(currPlayerIndex).decrementArmyCount(1)
      newTurn
      //Ok(views.html.armyview(players, terrs))
    //} else {
      //throw exception like this
     //Redirect(routes.ArmySetUpController.claimTerritory(terrIndex)).flashing(
       //"Warning" -> "Selected Territory has already been claimed")
    }
    Ok(views.html.armyview(players, terrs))
  }


}
