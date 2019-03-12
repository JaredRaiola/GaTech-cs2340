package controllers

import javax.inject.Inject

import scala.collection.mutable.ArrayBuffer

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
class PlayerFormController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {
  import PlayerForm._

  private var players = ArrayBuffer(new Player("", 0, 0))

  // The URL to the widget.  You can call this directly from the template, but it
  // can be more convenient to leave the template completely stateless i.e. all
  // of the "WidgetController" references are inside the .scala file.
  private val postUrl = routes.PlayerFormController.createPlayer()
  players.remove(0)


  ///////////////its a hack//////////////////////////////////////
  var terrs: Array[Territory] = null

  def createArmySetUpController(playersInput: ArrayBuffer[Player], terrsInput: Array[Territory]) = {
    players = playersInput
    terrs = terrsInput
  }

  //index into the ArrayBuffer which player is active
  private var currPlayerIndex: Int = 0

  def newTurn = {
    if (currPlayerIndex == players.length) {
      currPlayerIndex = 0
    } else {
      currPlayerIndex += 1
    }
  }

  def checkTerritory(terrIndex: Int): Boolean = {
    terrs(terrIndex).ownerName != ""
  }

  def claimTerritory(terrIndex: Int) = Action { //implicit request: MessagesRequest[AnyContent] =>

    if (checkTerritory(terrIndex) && players(currPlayerIndex).armyBinCount != 0) {
      terrs(terrIndex).incrementArmy(1)
      terrs(terrIndex).setOwner(players(currPlayerIndex).name)
      players(currPlayerIndex).decrementArmyCount(1)
      newTurn
      //Ok(views.html.armyview(players, terrs))
    //} else {
      //throw exception like this
     //Redirect(routes.ArmySetUpController.claimTerritory(terrIndex)).flashing(
       //"Warning" -> "Selected Territory has already been claimed")
    }
    Ok(views.html.armyview(players, terrs))
  }



  ////////////////////////////////////////////////////////////////



  def index = Action {
    Ok(views.html.index())
  }

  def armyview = Action {
    val terrArray = new Array[Territory](48)
    for (i <- terrArray.indices) {
      terrArray(i) = new Territory("TerritoryName" + i, "", 0)
    }
    terrs = terrArray
    Ok(views.html.armyview(players, terrs))
  }

  def listPlayers = Action { implicit request: MessagesRequest[AnyContent] =>
    // Pass an unpopulated form to the template
    Ok(views.html.listPlayers(players, form, postUrl))
  }

  // This will be the action that handles our form post
  def createPlayer = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[Data] =>
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.listPlayers(players, formWithErrors, postUrl))
    }

    val successFunction = { data: Data =>
      // This is the good case, where the form was successfully parsed as a Data object.
      val player = new Player(data.name, 0, 0)
      if (!players.isEmpty){
        if (players.contains(player)) {
          Redirect(routes.PlayerFormController.listPlayers()).flashing("Warning" -> "Please enter a unique name!")
        } else if (players.length < 6) {
          players.append(player)
          players = scala.util.Random.shuffle(players)
          for(w <- players) {
            w.setArmyCount(35 - (5* (players.length-3)))
          }
          if (players.length < 3) {
            val numplayR = 3 - players.length
            val playremain = "You have " + numplayR.toString + " player slots remaining in order to play"
            Redirect(routes.PlayerFormController.listPlayers()).flashing("Note" -> playremain)
          } else {
            val numplay = players.length
            val playremain = "You have " + numplay.toString + " player slots filled"
            Redirect(routes.PlayerFormController.listPlayers()).flashing("Note" -> playremain)
          }
        } else {
          Redirect(routes.PlayerFormController.listPlayers()).flashing("Warning" -> "You have entered 6 players already!")
        }
      } else {
        players.append(player)
        val numplayR = 3 - players.length
        val playremain = "You have " + numplayR.toString + " player slots remaining in order to play"
        for(w <- players) {
          w.setArmyCount(35 - (5* (players.length-3)))
        }
        Redirect(routes.PlayerFormController.listPlayers()).flashing("Note"  -> playremain)
      }
    }

    val formValidationResult = form.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }
}
