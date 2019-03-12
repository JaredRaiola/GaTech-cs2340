package controllers


import javax.inject.Inject

import models._



import play.api.mvc._




import scala.collection.mutable.ArrayBuffer


class ArmySetUpController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {


//(var players: ArrayBuffer[Player], val terrCont: TerritoryController)



  def createArmySetUpController(players: ArrayBuffer[Player], terrCont: TerritoryController) = {
  }

  val players: ArrayBuffer[Player] = null
  val terrCont: TerritoryController = null



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

  def claimTerritory(terrIndex: Int) = Action { //implicit request: MessagesRequest[AnyContent] =>

    if (checkTerritory(terrIndex)) {
      terrCont.terrArray(terrIndex).incrementArmy(1)
      terrCont.terrArray(terrIndex).setOwner(players(currPlayerIndex).name)
      players(currPlayerIndex).decrementArmyCount(1)
      //Ok(views.html.armyview(players, terrCont))
    //} else {
      //throw exception like this
     //Redirect(routes.ArmySetUpController.claimTerritory(terrIndex)).flashing(
       //"Warning" -> "Selected Territory has already been claimed")
    }
    Ok(views.html.armyview(players, terrCont))
  }


}
