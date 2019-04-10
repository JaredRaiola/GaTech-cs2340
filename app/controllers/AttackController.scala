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
    terr.armyCount >= count + 1 && count < 4 && count > 0
  }

  def checkDefenceDiceCount(count: Int, terr: Territory): Boolean = {
    terr.armyCount >= count && count < 3 && count > 0
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
      //error check
      //set GameData attackDiceRoll and defenceDiceRoll from the getDiceRollArray
      //call get AttackLosses to get the loss tuple
      //apply the losses
      //other stuff happens
      val myTerrIndex = data.terr.toInt
      val otherTerrIndex = data.otherTerr.toInt


      val myTerr: Territory = GameData.terrArray(myTerrIndex)
      val otherTerr: Territory = GameData.terrArray(otherTerrIndex)
      val attackDiceCount = data.attackDiceCount.toInt
      val defenceDiceCount = data.defenseDiceCount.toInt

      if (!GameData.doesCurrPlayerOwnTerr(myTerr)) {
        Redirect(routes.AttackController.updateView()).flashing("Hey!" -> "You cant attack from someone elses territory")
      } else if (GameData.doesCurrPlayerOwnTerr(otherTerr)) {
        Redirect(routes.AttackController.updateView()).flashing("Hey!" -> "Stop hitting yourself")
      } else if (!checkAttackDiceCount(attackDiceCount, myTerr)) {
        Redirect(routes.AttackController.updateView()).flashing("Hey!" -> "Thats an invalid number of dice for the attacking territory")
      } else if (!checkDefenceDiceCount(defenceDiceCount, otherTerr)) {
        Redirect(routes.AttackController.updateView()).flashing("Hey!" -> "Thats an invalid number of dice for the defending territory")
      } else if (!GameData.checkTerritoryAdjacency(myTerr, otherTerr)) {
        Redirect(routes.AttackController.updateView()).flashing("Hey!" -> "Those territories dont share a border")
      } else { // i hope there arent any other errors
               //do all the attack stuff
        GameData.attackDiceRoll = getDiceRollArray(attackDiceCount)
        GameData.defenceDiceRoll = getDiceRollArray(defenceDiceCount)
        val attackLosses = getAttackLosses(attackDiceCount, defenceDiceCount)

      }

      // Stubbed case
      GameData.newTurn
      Redirect(routes.TerritoryController.updatePlacements()).flashing("Whoa! " -> "We haven't coded this functionality yet!")

    }

    val formValidationResult = attackForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }
}
