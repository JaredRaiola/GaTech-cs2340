package controllers

import javax.inject.Inject
import models.Player
import models.Territory
import play.api.data._
import play.api.i18n._
import play.api.mvc._


class AttackController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {

  import AttackForm._
  import AdditionalArmiesForm._

  //need to make forms and import them here
  //forms needed: myTerr, otherTerr, attackDiceCount, defenceDiceCount
  //then make funtion that puts all parts together

  def index:Action[AnyContent] = Action {
    Ok(views.html.index())
  }

  def checkAttackDiceCount(count: Int, terr: Territory): Boolean = {
    terr.armyCount >= count + 1 && count < 4
  }

  def checkDefenceDiceCount(count: Int, terr: Territory): Boolean = {
    terr.armyCount >= count && count < 3
  }







  /*
   * returns tuple of length 2 consisting of: attacker losses, defender losses
   */
  def getAttackLosses(myDiceCount: Int, otherDiceCount: Int) = {
    val attackDice = GameData.attackDiceRoll
    val defenceDice = GameData.attackDiceRoll
    val lossTuple = if (myDiceCount > otherDiceCount) {
      attackDiceLossTuple(attackDice, defenceDice, otherDiceCount)
    } else {
      attackDiceLossTuple(attackDice, defenceDice, myDiceCount)
    }
    lossTuple
  }

    def attackDiceLossTuple(attackDice: Array[Int], defenceDice: Array[Int], comparisonCount: Int) = {
      var attackLoss = 0
      var defenceLoss = 0
      for (i <- 0 to comparisonCount) {
        val loss: Int = attackDice(i) - defenceDice(i)
        if (loss > 0) {
          defenceLoss += 1
        } else {
          attackLoss += 1
        }
      }
      (attackLoss, defenceLoss)
    }


  /*
   * returns a sorted array (largest to smallest) of length diceCount of numbers 1 to 6
   */
  private def getDiceRollArray(diceCount: Int): Array[Int] = {
    val diceNumVect = for (i <- 0 to diceCount) yield {
      val rand = new scala.util.Random
      val num = rand.nextInt(5) + 1
      num
    }
    diceNumVect.toArray.sortWith(_ > _)
  }

  def updateView:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.attackview(attackForm))
  }

  def attack:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[AttackData] =>
      BadRequest(views.html.attackview(attackForm))
    }

    val successFunction = { data: AttackData =>
      // Stubbed case
      GameData.newTurn
      Redirect(routes.TerritoryController.updatePlacements()).flashing("Whoa! " -> "We haven't coded this functionality yet!")

    }

    val formValidationResult = attackForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }
}
