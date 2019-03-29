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

  def newTurn = {
    if (GameData.currPlayerIndex == GameData.players.length - 1) {
      GameData.currPlayerIndex = 0
    } else {
      GameData.currPlayerIndex += 1
    }
    GameData.turnCounter += 1
  }

  def checkTerritory(terrIndex: Int): Boolean = {
    GameData.terrArray(terrIndex).ownerName != ""
  }

  def index = Action {
    Ok(views.html.index())
  }

  def listTerritories = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.armyview(GameData.players, GameData.currPlayerIndex, GameData.terrArray, terriform))
  }

  def isAllDigits(x: String) = x forall Character.isDigit


  def claimTerritories = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[TerritoryData] =>
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.armyview(GameData.players, GameData.currPlayerIndex, GameData.terrArray, terriform))
    }


    val successFunction = { data: TerritoryData =>
      // This is the good case, where the form was successfully parsed as a Data object.
      var terrIndex = -1
      if (GameData.terrArray.isEmpty || GameData.players.size < 3) {
        Redirect(routes.TerritoryController.listTerritories()).flashing("Huh" -> "Something went wrong.")
      } else if (data.terr.toLowerCase() == "random") {
        var randomter = scala.util.Random.nextInt(47)
        while (checkTerritory(randomter)) {
          randomter = scala.util.Random.nextInt(47)
        }
        terrIndex = randomter
      } else if (data.terr != "" && (isAllDigits(data.terr)) && (data.terr.toInt <= 47 && data.terr.toInt >= 0) && GameData.terrArray(data.terr.toInt).ownerName == "") {
        terrIndex = data.terr.toInt
      }
      if (terrIndex != -1) {
        //success
        GameData.terrArray(terrIndex).incrementArmy(1)
        GameData.terrArray(terrIndex).setOwner(GameData.players(GameData.currPlayerIndex).name)
        GameData.players(GameData.currPlayerIndex).decrementArmyCount(1)
        newTurn
        //now check turncounter
        if (GameData.turnCounter != GameData.terrArray.size) {
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



  def updatePlacements = Action { implicit request: MessagesRequest[AnyContent] =>
    // Pass an unpopulated form to the template
    Ok(views.html.armyPlacement(GameData.players, GameData.terrArray, additionalArmiesForm ))
  }

  def placeAdditionalArmies = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[AdditionalArmiesData] =>
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.armyPlacement(GameData.players, GameData.terrArray, additionalArmiesForm))
    }

    val successFunction = { data: AdditionalArmiesData =>
      // This is the good case, where the form was successfully parsed as a Data object.
      var terrIndex = -1;
      if (GameData.terrArray.isEmpty || GameData.players.size < 3) {
        Redirect(routes.TerritoryController.listTerritories()).flashing("Huh" -> "Something went wrong.")
      } else if (data.terr != "" && (isAllDigits(data.terr)) && (data.terr.toInt <= 47 && data.terr.toInt >= 0) && GameData.terrArray(data.terr.toInt).ownerName == "") {
          terrIndex = data.terr.toInt
      }
      if (data.numArmies <= 0 || data.numArmies > GameData.players(GameData.currPlayerIndex).armyBinCount) {
          //Redirect(routes.TerritoryController.updatePlacements).flashing("Hey!" -> "That's an invalid number of armies >:(")
      } else if (terrIndex != -1) {
        //success
          GameData.terrArray(terrIndex).incrementArmy(data.numArmies)
          GameData.players(GameData.currPlayerIndex).decrementArmyCount(data.numArmies)
          newTurn
      }
      Ok(views.html.armyPlacement(GameData.players, GameData.terrArray, additionalArmiesForm))
    }
    val formValidationResult = additionalArmiesForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }
}
