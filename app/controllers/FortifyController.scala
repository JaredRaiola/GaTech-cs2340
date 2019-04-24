package controllers


import javax.inject.Inject
import models.Player
import models.Territory
import play.api.data._
import play.api.i18n._
import play.api.mvc._

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
    true
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
        case null => fortifyFunction(data)
        case _ => Redirect(routes.FortifyController.updateFortifyView()).flashing(errorString)
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
    Redirect(routes.FortifyController.updateFortifyView()).flashing("Nice!" -> ("Territory " + data.terrToFortify
      + " now has " + GameData.terrArray(terrToFortifyIndex).armyCount + " armies, while Territory " + data.terrFrom
      + " now has " + GameData.terrArray(terrFromIndex).armyCount + " armies."))
  }

  private def errorHandleFortifyInput(data: FortifyData): (String, String) = {
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
      ("Cmon!", "You gotta enter valid numbers to fortify!")
    }
  }
}
