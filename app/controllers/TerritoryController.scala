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

  private def startStateIncomplete = GameData.terrArray.isEmpty || GameData.players.size < 3
  private def isAllDigits(x: String) = x forall Character.isDigit
  private def isValidNum(str: String) = str != "" && isAllDigits(str)
  private def isInRange(str: String) = isValidNum(str) && str.toInt <= 47 && str.toInt >= 0
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
    var index = GameData.currPlayerIndex
    var newArmies = GameData.calculateNewArmies(index)
    GameData.players(index).incrementArmyCount(newArmies)
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
      if (startStateIncomplete) {
        Redirect(routes.TerritoryController.listTerritories()).flashing("Huh" -> "Something went wrong.")
      } else if (data.terr.toLowerCase() == "all random") {
        fillAll
        terrIndex = -1
        //GameData.turnCounter = GameData.turnCounter - 1
        GameData.turnCounter = 0
      } else if (data.terr.toLowerCase() == "random") {
        terrIndex = getRandomIndex
      } else if (isInRange(data.terr) && !territoryIsOccupied(data.terr.toInt)) {
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
      } else {
        //failure
        Redirect(routes.TerritoryController.listTerritories()).flashing("Straight-up wack! " -> "You can't claim a territory there!")
      }
    }

    val formValidationResult = terriform.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

  def endTurn:Action[AnyContent] = {
    newTurn
    assignNewArmies
    updatePlacements
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
      if (startStateIncomplete) {
        Redirect(routes.TerritoryController.updatePlacements).flashing("Huh" -> "Something went wrong.")
      } else if (!isInRange(data.terr)) {
        Redirect(routes.TerritoryController.updatePlacements).flashing("Warning: " -> "This is not a valid territory value.")
      } else if (isInRange(data.terr) & !GameData.doesCurrPlayerOwnTerr(GameData.terrArray(data.terr.toInt))) {
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
//          if (GameData.players(GameData.currPlayerIndex).armyBinCount == 0) {
//            newTurn
//            assignNewArmies
//          }
//          Ok(views.html.attackview(attackForm))
          Ok(views.html.armyPlacement(additionalArmiesForm))
        }
      }
    }

    val formValidationResult = additionalArmiesForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

//  def endTurn: Unit = {
//
//    if (armiesOnTurn < GameData.calculateNewArmies(GameData.currPlayerIndex)) {
//      Redirect(routes.TerritoryController.updatePlacements).flashing("Hey!" -> "You need to place all of your new armies.")
//    }
//
//
//
//    newTurn
//    assignNewArmies
//
//
//  }

}
