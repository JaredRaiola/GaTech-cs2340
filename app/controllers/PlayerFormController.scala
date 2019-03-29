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

  private val postUrl = routes.PlayerFormController.createPlayer()
  GameData.players.remove(0)

  def index:Action[AnyContent] = Action {
    Ok(views.html.index())
  }

  def listPlayers:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    // Pass an unpopulated form to the template
    Ok(views.html.listPlayers(GameData.players, form, postUrl))
  }

  // This will be the action that handles our form post
  def createPlayer:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[Data] =>
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.listPlayers(GameData.players, formWithErrors, postUrl))
    }

    val successFunction = { data: Data =>
      // This is the good case, where the form was successfully parsed as a Data object.
      val player = new Player(data.name, 0, 0)
      if (GameData.players.nonEmpty) {
        if (GameData.players.contains(player)) {
          Redirect(routes.PlayerFormController.listPlayers()).flashing("Warning" -> "Please enter a unique name!")
        } else if (GameData.players.length < 6) {
          addPlayer(player)
          shufflePlayers
          val remainMessage:String = if (GameData.players.length < 3) getRemainingMessage else getExistingMessage
          Redirect(routes.PlayerFormController.listPlayers()).flashing("Note: " -> remainMessage)
        } else {
          Redirect(routes.PlayerFormController.listPlayers()).flashing("Warning" -> "You have entered 6 players already!")
        }
      } else {
        addPlayer(player)
        val remainMessage:String = getRemainingMessage
        Redirect(routes.PlayerFormController.listPlayers()).flashing("Note: " -> remainMessage)
      }
    }
    val formValidationResult = form.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

  private def getRemainingMessage:String = {
    val remainingPlayersNeeded = 3 - GameData.players.length
    val remainMessage = "You have " + remainingPlayersNeeded + " player slots remaining in order to play"
    remainMessage
  }

  private def getExistingMessage:String = {
    val remainingPlayersPresent = GameData.players.length
    val remainMessage = "You have " + remainingPlayersPresent + " player slots filled."
    remainMessage
  }

  private def shufflePlayers = {
    GameData.players = scala.util.Random.shuffle(GameData.players)
  }

  private def addPlayer(player: Player) = {
    GameData.players.append(player)
    for (w <- GameData.players) {
      w.setArmyCount(35 - (5 * (GameData.players.length - 3)))
    }
  }
}
