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
  import AttackForm._

  private var armiesOnTurn = 0


  private def territoryIsOccupied(terrIndex: Int): Boolean = GameData.terrArray(terrIndex).ownerName != ""
  private def isArmyAmountInvalid(numArmies: Int): Boolean = numArmies <= 0 || numArmies > GameData.players(GameData.currPlayerIndex).armyBinCount
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
  private def assignNewArmies = {
    GameData.assignNewArmies
  }

  def newTurn:Unit = {
    GameData.newTurn
    armiesOnTurn = 0
  }

  def index:Action[AnyContent] = Action {
    Ok(views.html.index())
  }

  def listTerritories:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.armyview(terriform))
  }

  def claimTerritories:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[TerritoryData] =>
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.armyview(terriform))
    }

    val successFunction = { data: TerritoryData =>
      // This is the good case, where the form was successfully parsed as a Data object.
      var terrIndex = -2
      if (GameData.startStateIncomplete) {
        terrIndex = -3
      } else if (data.terr.toLowerCase() == "all random") {
        fillAll
        terrIndex = -1
        //GameData.turnCounter = GameData.turnCounter - 1
        GameData.turnCounter = 0
      } else if (data.terr.toLowerCase() == "random") {
        terrIndex = getRandomIndex
      } else if (GameData.isInRange(data.terr) && !territoryIsOccupied(data.terr.toInt)) {
        terrIndex = data.terr.toInt
      }
      if (terrIndex > -2) {
        //success
        if (terrIndex != -1) {
          GameData.terrArray(terrIndex).incrementArmy(1)
          GameData.terrArray(terrIndex).setOwner(GameData.players(GameData.currPlayerIndex).name)
          GameData.players(GameData.currPlayerIndex).decrementArmyCount(1)
          newTurn
        }
        //now check turncounter
        if (GameData.turnCounter != GameData.terrArray.length && terrIndex != -1) {
          Ok(views.html.armyview(terriform))
          val result = "Territory " + terrIndex + " now has " + GameData.terrArray(terrIndex).armyCount + " armies."
          Redirect(routes.TerritoryController.listTerritories()).flashing("Tubular! " -> result)
        } else {
          assignNewArmies
          GameData.turnCounter = 0
          Ok(views.html.armyPlacement(additionalArmiesForm))
        }
      } else if (terrIndex == -2) {
        //failure
        Redirect(routes.TerritoryController.listTerritories()).flashing("Straight-up wack! " -> "You can't claim a territory there!")
      } else {
        Redirect(routes.TerritoryController.listTerritories()).flashing("Huh" -> "Something went wrong.")
      }
    }

    val formValidationResult = terriform.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

  def endTurn:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    if (GameData.getCurrentPlayer.armyBinCount == 0) {
      //GameData.turnCounter += 1
      newTurn
      assignNewArmies
    }
    Ok(views.html.armyPlacement(additionalArmiesForm))
  }

  def updatePlacements:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.armyPlacement(additionalArmiesForm))
  }


  def placeAdditionalArmies:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[AdditionalArmiesData] =>
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.armyPlacement(additionalArmiesForm))
    }
    val successFunction = { data: AdditionalArmiesData =>
      // This is the good case, where the form was successfully parsed as a Data object.
      var terrIndex = -1
      if (GameData.startStateIncomplete) {
        Redirect(routes.TerritoryController.updatePlacements).flashing("Huh" -> "Something went wrong.")
      } else if (!GameData.isInRange(data.terr)) {
        Redirect(routes.TerritoryController.updatePlacements).flashing("Warning: " -> "This is not a valid territory value.")
      } else if (GameData.isInRange(data.terr) & !GameData.doesCurrPlayerOwnTerr(GameData.terrArray(data.terr.toInt))) {
        Redirect(routes.TerritoryController.updatePlacements).flashing("You can't do that! " -> "You don't own that territory.")
      } else {
        terrIndex = data.terr.toInt
        if(isArmyAmountInvalid(data.numArmies.toInt)) {
          Redirect(routes.TerritoryController.updatePlacements).flashing("You and what army? " -> "That's more armies than you have.")
        } else {
          //success
          GameData.terrArray(terrIndex).incrementArmy(data.numArmies)
          GameData.players(GameData.currPlayerIndex).decrementArmyCount(data.numArmies)
          armiesOnTurn = armiesOnTurn + data.numArmies
          Ok(views.html.armyPlacement(additionalArmiesForm))
        }
      }
    }

    val formValidationResult = additionalArmiesForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

}
