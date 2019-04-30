package controllers


import javax.inject.Inject
import models.Player
import models.Territory
import play.api.data._
import play.api.i18n._
import play.api.mvc._
import scala.collection.mutable.Queue
import scala.collection.immutable.HashSet

class FortifyController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {

  import FortifyForm._

  private def checkValidNums(data: FortifyData): Boolean = {
    if (GameData.isInRange(data.terrFrom)
      && GameData.isValidNum(data.numArmies) && GameData.isInRange(data.terrToFortify))
      true
    else
      false
  }

  private def isContinuous(terrFrom: Int, terrToFortify: Int): Boolean = {
    var counter = 0
    var toReturn = false
    var terrQueue = new Queue[Int]
    var visitedSet = new HashSet[Int]
    for (x <- GameData.terrArray(terrFrom).getAdjList()) {
      if (GameData.terrArray(x).ownerName == GameData.getCurrentPlayer.name) {
        terrQueue += x
      }
    }
    visitedSet += terrFrom
    while(!terrQueue.isEmpty && visitedSet.size < GameData.calculateTerritoriesOwned(GameData.currPlayerIndex)) {
      var currentTerr = terrQueue.dequeue()
      if (currentTerr == terrToFortify) {
        toReturn = true
      }
      else if (!visitedSet.contains(currentTerr)) {
        visitedSet += currentTerr
        for (x <- GameData.terrArray(currentTerr).getAdjList()) {
          if (GameData.terrArray(x).ownerName == GameData.getCurrentPlayer.name) {
            terrQueue += x
          }
        }
      }
      counter += 1
    }
    toReturn
  }

  def updateFortifyView:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.fortifyView(fortifyForm))
  }

  def fortifyTerritory:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction: Form[FortifyData] => Result = { formWithErrors: Form[FortifyData] =>
      BadRequest(views.html.fortifyView(fortifyForm))
    }

    val successFunction: FortifyData => Result = { data: FortifyData =>
      val errorString = errorHandleFortifyInput(data)

      errorString match {
        case None => fortifyFunction(data)
        case Some(tuple: (String, String)) => Redirect(routes.FortifyController.updateFortifyView()).flashing(errorString.get)
      }
    }

    val formValidationResult = fortifyForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

  private def fortifyFunction(data: FortifyData) = {
    val terrFromIndex = data.terrFrom.toInt
    val terrToFortifyIndex = data.terrToFortify.toInt
    val numArmies = data.numArmies.toInt
    GameData.terrArray(terrFromIndex).decrementArmy(numArmies)
    GameData.terrArray(terrToFortifyIndex).incrementArmy(numArmies)
    Redirect(routes.TerritoryController.updatePlacements()).flashing(GameData.getCurrentPlayer.name + " has moved " + numArmies + " armies!" ->
      ("Territory " + data.terrToFortify + " now has " + GameData.terrArray(terrToFortifyIndex).armyCount
        + " armies, while Territory " + data.terrFrom + " now has " + GameData.terrArray(terrFromIndex).armyCount + " armies."))
  }

  private def errorHandleFortifyInput(data: FortifyData): Option[(String, String)] =
  {
    val errorStringTuple =
    if (checkValidNums(data)) {
      val terrFromIndex = data.terrFrom.toInt
      val terrToFortifyIndex = data.terrToFortify.toInt
      val numArmies = data.numArmies.toInt
      if (!GameData.doesCurrPlayerOwnTerr(GameData.terrArray(terrFromIndex))) {
        ("Hey!", "You don't own the territory supplying the armies!")
      } else if (!GameData.doesCurrPlayerOwnTerr(GameData.terrArray(terrToFortifyIndex))) {
        ("Hey!", "You don't own the territory you're trying to fortify!")
      } else if (!isContinuous(terrFromIndex, terrToFortifyIndex)) {
        ("Hey!", "Those territories aren't continuous!")
      } else if (numArmies.toInt <= 0 || numArmies >= GameData.terrArray(terrFromIndex).armyCount) {
        ("Hey!", "You can't fortify with that number of armies!")
      } else {
        null
      }
    } else {
      ("Hey!", "You gotta enter valid numbers to fortify!")
    }
    if (errorStringTuple != null) Some(errorStringTuple) else None
  }
}
