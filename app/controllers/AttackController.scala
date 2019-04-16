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

  def checkTerrInRange(terrIndex: Int): Boolean = {
    terrIndex > -1 && terrIndex < 48
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
  private def getAttackLosses(myDiceCount: Int, otherDiceCount: Int) = {
    val attackDice = GameData.attackDiceRoll
    val defenceDice = GameData.defenceDiceRoll
    val lossTuple = if (myDiceCount > otherDiceCount) {
      attackDiceLossTuple(attackDice, defenceDice, otherDiceCount)
    } else {
      attackDiceLossTuple(attackDice, defenceDice, myDiceCount)
    }
    lossTuple
  }

  private def attackDiceLossTuple(attackDice: Array[Int], defenceDice: Array[Int], comparisonCount: Int) = {
    var attackLoss = 0
    var defenceLoss = 0
    for (i <- 0 to comparisonCount - 1) {
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
    val diceNumVect = for (i <- 1 to diceCount) yield {
      val rand = new scala.util.Random
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
    val errorFunction: Form[AttackData] => Result = { formWithErrors: Form[AttackData] =>
      BadRequest(views.html.attackview(attackForm))
    }

    val successFunction: AttackData => Result = { data: AttackData =>
      val errorString = errorHandleAttackInput(data)

      errorString match {
        case null => attackFunction(data)
        case _ => Redirect(routes.AttackController.updateView()).flashing(errorString)
      }
    }

    val formValidationResult = attackForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }


  private def attackFunction(data: AttackData) = {
    val myTerrIndex = data.terr.toInt
    val otherTerrIndex = data.otherTerr.toInt
    val myTerr: Territory = GameData.terrArray(myTerrIndex)
    val otherTerr: Territory = GameData.terrArray(otherTerrIndex)
    val attackDiceCount = data.attackDiceCount.toInt
    val defenceDiceCount = data.defenceDiceCount.toInt

    val otherOwnerName = otherTerr.ownerName
    GameData.attackDiceRoll = getDiceRollArray(attackDiceCount)
    GameData.defenceDiceRoll = getDiceRollArray(defenceDiceCount)

    val attackLosses = getAttackLosses(attackDiceCount, defenceDiceCount)

    myTerr.decrementArmy(attackLosses._1)
    otherTerr.decrementArmy(attackLosses._2)
    if (otherTerr.armyCount == 0) {
      otherTerr.setOwner(GameData.getCurrentPlayer.name)
      val armiesMoving = (attackDiceCount - attackLosses._1)
      otherTerr.incrementArmy(armiesMoving)
      myTerr.decrementArmy(armiesMoving)
      Redirect(routes.AttackController.updateView()).flashing("WoW!" -> (GameData.getCurrentPlayer.name +
        " just claimed Territory " + otherTerrIndex + ". " + GameData.getCurrentPlayer.name + " lost "  + attackLosses._1 + " armies. " + otherOwnerName + " lost " +
        attackLosses._2 + " armies."))
    } else {
      Redirect(routes.AttackController.updateView()).flashing("Oh No!" -> (GameData.getCurrentPlayer.name +
        " did not claim Territory " + otherTerrIndex + ". " + GameData.getCurrentPlayer.name + " lost "  + attackLosses._1 + " armies. " + otherOwnerName + " lost " +
        attackLosses._2 + " armies."))
    }

  }



  private def errorHandleAttackInput(data: AttackData): (String, String) = {
    if (checkValidNums(data)) {
      val myTerrIndex: Int = data.terr.toInt
      val otherTerrIndex: Int = data.otherTerr.toInt
      val attackDiceCount: Int = data.attackDiceCount.toInt
      val defenceDiceCount: Int = data.defenceDiceCount.toInt
      if (!(checkTerrInRange(myTerrIndex) && checkTerrInRange(otherTerrIndex))) {
        ("Hey!", "You can't attack from a territory that does't exist!")
      } else if (!GameData.doesCurrPlayerOwnTerr(GameData.terrArray(myTerrIndex))) {
        ("Hey!", "You cant attack from someone elses territory")
      } else if (GameData.doesCurrPlayerOwnTerr(GameData.terrArray(otherTerrIndex))) {
        ("Hey!", "Stop hitting yourself")
      } else if (!GameData.checkTerritoryAdjacency(myTerrIndex, otherTerrIndex)) {
        ("Hey!", "Those territories dont share a border")
      } else if (!checkAttackDiceCount(attackDiceCount,
        GameData.terrArray(myTerrIndex))) {
        ("Hey attacker!", "You can't roll that many die! (You must roll up to 3 die and you must have at least as many armies in the attacking territory per number of die you roll)")
      } else if (!checkDefenceDiceCount(defenceDiceCount,
        GameData.terrArray(otherTerrIndex))) {
        ("Hey defender!", "You can't roll that many die! (You must roll up to 2 die and you must have at least as many armies in the defending territory per number of die you roll)")
      } else {
        null
      }
    } else {
      ("Fiddle Sticks!!", "You can only input numeric values into the forms")
    }
  }

  private def checkValidNums(data: AttackData): Boolean = {
    val stringList: List[String] = List(data.terr, data.otherTerr,
      data.attackDiceCount, data.defenceDiceCount)
    val bools = for (i <- 0 to stringList.length - 1) yield {
      GameData.isValidNum(stringList(i))
    }
    bools.foldLeft(true)(_ && _)
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
