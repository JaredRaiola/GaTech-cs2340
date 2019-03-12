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
class PlayerFormController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {
  import PlayerForm._
  import TerriForm._

  private var players = scala.collection.mutable.ArrayBuffer(new Player("", 0, 0))
  private var terrCont: TerritoryController = _

  // The URL to the widget.  You can call this directly from the template, but it
  // can be more convenient to leave the template completely stateless i.e. all
  // of the "WidgetController" references are inside the .scala file.
  private val postUrl = routes.PlayerFormController.createPlayer()
  players.remove(0)


  val terrArray = new Array[Territory](48)
  for (i <- terrArray.indices) {
    terrArray(i) = new Territory("TerritoryName" + i, "", 0)
  }
  terrCont = new TerritoryController(terrArray)

  def index = Action {
    Ok(views.html.index())
  }

  def listTerritories = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.armyview(players, terrCont, terriform))
  }

  def claimTerritories = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[TerritoryData] =>
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.armyview(players, terrCont, terriform))
    }

    val successFunction = { data: TerritoryData =>
      // This is the good case, where the form was successfully parsed as a Data object.

      if (terrCont.terrArray.isEmpty) {
        Redirect(routes.PlayerFormController.listTerritories()).flashing("Huh" -> "Something went wrong.")
      } else {
        if (data.terr > 47 || data.terr < 0) {
          Redirect(routes.PlayerFormController.listTerritories()).flashing("Straight-up wack! " -> "You can't claim a territory there.")
        } else {
          terrCont.terrArray(data.terr).incrementArmy(10)
          Ok(views.html.armyview(players, terrCont, terriform))
          val result = "Territory " + data.terr + " now has " + terrCont.terrArray(data.terr).armyCount + " armies."
          Redirect(routes.PlayerFormController.listTerritories()).flashing("Tubular! " -> result )
        }
      }
    }

    val formValidationResult = terriform.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
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
