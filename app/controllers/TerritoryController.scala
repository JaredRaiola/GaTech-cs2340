package controllers

import javax.inject.Inject
import models.Player
import models.Territory
import play.api.data._
import play.api.i18n._
import play.api.mvc._

/**
  * The classic WidgetController using MessagesAbstractController.
  *
  * Instead of MessagesAbstractController, you can use the I18nSupport trait,
  * which provides implicits that create a Messages instance from a request
  * using implicit conversion.
  *
  * See https://www.playframework.com/documentation/2.6.x/ScalaForms#passing-messagesprovider-to-form-helpers
  * for details.
  */
class TerritoryController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {

  import TerriForm._
  import AdditionalArmiesForm._

  private def startStateIncomplete = GameData.terrArray.isEmpty || GameData.players.size < 3
  private def isAllDigits(x: String) = x forall Character.isDigit
  private def isValidNum(str: String) = str != "" && isAllDigits(str)
  private def isInRange(str: String) = isValidNum(str) && str.toInt <= 47 && str.toInt >= 0
  private def territoryIsOccupied(terrIndex: Int): Boolean = GameData.terrArray(terrIndex).ownerName != ""
  private def getRandomIndex = {
    var randomter = scala.util.Random.nextInt(GameData.numTerritories)
    var iterations = 0
    while (territoryIsOccupied(randomter)) {
      randomter = scala.util.Random.nextInt(GameData.numTerritories)
    }
    randomter
  }
  private def fillAll = {
    for (w <- 0 until (48 - GameData.turnCounter)) {
      var terrIndex = getRandomIndex
      GameData.terrArray(terrIndex).incrementArmy(1)
      GameData.terrArray(terrIndex).setOwner(GameData.players(GameData.currPlayerIndex).name)
      GameData.players(GameData.currPlayerIndex).decrementArmyCount(1)
      newTurn
    }
  }

  def newTurn:Unit = {
    if (GameData.currPlayerIndex == GameData.players.length - 1) {
      GameData.currPlayerIndex = 0
    } else {
      GameData.currPlayerIndex += 1
    }
    GameData.turnCounter += 1
  }

  def index:Action[AnyContent] = Action {
    Ok(views.html.index())
  }

  def listTerritories:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.armyview(GameData.players, GameData.currPlayerIndex, GameData.terrArray, terriform))
  }

  def claimTerritories:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[TerritoryData] =>
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.armyview(GameData.players, GameData.currPlayerIndex, GameData.terrArray, terriform))
    }

    val successFunction = { data: TerritoryData =>
      // This is the good case, where the form was successfully parsed as a Data object.
      var terrIndex = -1
      if (data.terr.toLowerCase() == "fillall") {
        fillAll
      }
      if (startStateIncomplete) {
        Redirect(routes.TerritoryController.listTerritories()).flashing("Huh" -> "Something went wrong.")
      } else if (data.terr.toLowerCase() == "random") {
        terrIndex = getRandomIndex
      } else if (isInRange(data.terr) && !territoryIsOccupied(data.terr.toInt)) {
        terrIndex = data.terr.toInt
      }
      if (terrIndex != -1) {
        //success
        GameData.terrArray(terrIndex).incrementArmy(1)
        GameData.terrArray(terrIndex).setOwner(GameData.players(GameData.currPlayerIndex).name)
        GameData.players(GameData.currPlayerIndex).decrementArmyCount(1)
        newTurn
        //now check turncounter
        if (GameData.turnCounter != GameData.terrArray.length) {
          Ok(views.html.armyview(GameData.players, GameData.currPlayerIndex, GameData.terrArray, terriform))
          val result = "Territory " + terrIndex + " now has " + GameData.terrArray(terrIndex).armyCount + " armies."
          Redirect(routes.TerritoryController.listTerritories()).flashing("Tubular! " -> result)
        } else {
          Ok(views.html.armyPlacement(GameData.players, GameData.currPlayerIndex, GameData.terrArray, additionalArmiesForm))
        }
      } else {
        //failure
        Redirect(routes.TerritoryController.listTerritories()).flashing("Straight-up wack! " -> "You can't claim a territory there.")
      }
    }

    val formValidationResult = terriform.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

  def updatePlacements:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.armyPlacement(GameData.players, GameData.currPlayerIndex, GameData.terrArray, additionalArmiesForm))
  }

  def placeAdditionalArmies:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[AdditionalArmiesData] =>
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.armyPlacement(GameData.players, GameData.currPlayerIndex, GameData.terrArray, additionalArmiesForm))
    }

    val successFunction = { data: AdditionalArmiesData =>
      // This is the good case, where the form was successfully parsed as a Data object.
      var terrIndex = -1
      if (startStateIncomplete) {
        Redirect(routes.TerritoryController.listTerritories()).flashing("Huh" -> "Something went wrong.")
      } else if (isInRange(data.terr) &&
        GameData.terrArray(data.terr.toInt).ownerName == GameData.players(GameData.currPlayerIndex).name) {
        terrIndex = data.terr.toInt
      }
      if (data.numArmies <= 0 || data.numArmies > GameData.players(GameData.currPlayerIndex).armyBinCount) {
          Redirect(routes.TerritoryController.updatePlacements).flashing("Hey!" -> "That's an invalid number of armies >:(")
      } else if (terrIndex != -1) {
        //success
          GameData.terrArray(terrIndex).incrementArmy(data.numArmies)
          GameData.players(GameData.currPlayerIndex).decrementArmyCount(data.numArmies)
          newTurn
      }
      Ok(views.html.armyPlacement(GameData.players, GameData.currPlayerIndex, GameData.terrArray, additionalArmiesForm))
    }
    val formValidationResult = additionalArmiesForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }
}
