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
    val defenceDice = GameData.defenceDiceRoll
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
    for (i <- 0 to comparisonCount - 1) {
      val loss: Int = attackDice(i) - defenceDice(i)
      println("Comparing " + attackDice(i) + " and " + defenceDice(i))
      println("Loss: " + loss)
      if (loss > 0) {
        defenceLoss += 1
      } else {
        attackLoss += 1
      }
    }
    println((attackLoss, defenceLoss))
    (attackLoss, defenceLoss)
  }


  /*
   * returns a sorted array (largest to smallest) of length diceCount of numbers 1 to 6
   */
  private def getDiceRollArray(diceCount: Int): Array[Int] = {
    val diceNumVect = for (i <- 1 to diceCount) yield {
      val rand = GameData.randomNumberMaker
      val num = rand.nextInt(6) + 1
      num
    }
    diceNumVect.toArray.sortWith(_ > _)
  }

  def updateView:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.attackview(attackForm))
  }

  def beginAttackPhase:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    GameData.newTurn
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

      if (GameData.startStateIncomplete) {
        Redirect(routes.TerritoryController.updatePlacements).flashing("Huh" -> "Something went wrong.")
      } else {


        val terrs = Vector(data.terr, data.otherTerr)

        val terrsValidVec = for (i <- 0 to 1) yield {
          val terr = terrs(i).toString
          GameData.isInRange(terr)
        }

        val terrsValidCheck = terrsValidVec.reduce((a, b) => a && b)

        if (true) {

          val myTerrIndex = data.terr.toInt
          val otherTerrIndex = data.otherTerr.toInt


          val myTerr: Territory = GameData.terrArray(myTerrIndex)
          val otherTerr: Territory = GameData.terrArray(otherTerrIndex)
          val attackDiceCount = data.attackDiceCount.toInt
          val defenceDiceCount = data.defenceDiceCount.toInt

          if (!GameData.doesCurrPlayerOwnTerr(myTerr)) {
            Redirect(routes.AttackController.updateView()).flashing("Hey!" -> "You cant attack from someone elses territory")
          } else if (GameData.doesCurrPlayerOwnTerr(otherTerr)) {
            Redirect(routes.AttackController.updateView()).flashing("Hey!" -> "Stop hitting yourself")
          } else if (!checkAttackDiceCount(attackDiceCount, myTerr)) {
            Redirect(routes.AttackController.updateView()).flashing("Hey!" -> "Thats an invalid number of dice for the attacking territory")
          } else if (!checkDefenceDiceCount(defenceDiceCount, otherTerr)) {
            Redirect(routes.AttackController.updateView()).flashing("Hey!" -> "Thats an invalid number of dice for the defending territory")
          } else if (!GameData.checkTerritoryAdjacency(myTerr, otherTerr)) {
            Redirect(routes.AttackController.updateView()).flashing("Hey!" -> "Those territories do not share a border")
          } else { // i hope there arent any other errors
                   //do all the attack stuff
            GameData.attackDiceRoll = getDiceRollArray(attackDiceCount)
            GameData.defenceDiceRoll = getDiceRollArray(defenceDiceCount)
            println(GameData.attackDiceRoll.deep.mkString(", "))
            println(GameData.defenceDiceRoll.deep.mkString(", "))
            val attackLosses = getAttackLosses(attackDiceCount, defenceDiceCount)
            myTerr.decrementArmy(attackLosses._1)
            otherTerr.decrementArmy(attackLosses._2)

            GameData.terrArray(myTerrIndex) = myTerr
            GameData.terrArray(otherTerrIndex) = otherTerr


            if (otherTerr.armyCount == 0) {
              otherTerr.setOwner(GameData.getCurrentPlayer.name)
              otherTerr.incrementArmy(attackDiceCount - attackLosses._1)
              myTerr.decrementArmy(attackDiceCount - attackLosses._1)
              GameData.terrArray(myTerrIndex) = myTerr
              GameData.terrArray(otherTerrIndex) = otherTerr
              Redirect(routes.AttackController.updateView()).flashing("WoW!" -> (GameData.getCurrentPlayer.name
                + " just claimed Territory " + otherTerrIndex))
            } else {
              val defenderLossesString = attackLosses._2.toString
              val attackerLossesString = attackLosses._1.toString
              val defender = GameData.terrArray(otherTerrIndex).getOwner
              val attacker = GameData.getCurrentPlayer.name
              val displayString = attacker + " lost " + attackerLossesString + " armies and " + defender + " lost " + defenderLossesString + " armies"
              Redirect(routes.AttackController.updateView()).flashing("Oh No! " -> displayString)
            }
          }
        } else {
          Redirect(routes.AttackController.updateView()).flashing("Zoinks! " -> "Those numbers are not territories")
        }
      }
    }

    val formValidationResult = attackForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

  private def newTurn = {
    GameData.newTurn
  }

  private def assignNewArmies = {
    var index = GameData.currPlayerIndex
    var newArmies = GameData.calculateNewArmies(index)
    GameData.players(index).incrementArmyCount(newArmies)
  }
}
