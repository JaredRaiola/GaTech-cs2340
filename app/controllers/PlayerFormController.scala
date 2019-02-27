package controllers

import javax.inject.Inject


import models.Player
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
class PlayerController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {
  import PlayerForm._

  private var players = scala.collection.mutable.ArrayBuffer(new Player("", 0, 0))

  // The URL to the widget.  You can call this directly from the template, but it
  // can be more convenient to leave the template completely stateless i.e. all
  // of the "WidgetController" references are inside the .scala file.
  private val postUrl = routes.PlayerController.createPlayer()
  players.remove(0)

  def index = Action {
    Ok(views.html.index())
  }

  def armyview = Action {
    Ok(views.html.armyview(players))

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
          Redirect(routes.PlayerController.listPlayers()).flashing("info" -> "Please enter a unique name!")
        } else if (players.length < 6) {
          players.append(player)
          players = scala.util.Random.shuffle(players)
          for(w <- players) {
            w.setArmyCount(35 - (5* (players.length-3)))
          }
          if (players.length < 3) {
            val numplayR = 3 - players.length
            val playremain = "You have " + numplayR.toString + " player slots remaining in order to play"
            Redirect(routes.PlayerController.listPlayers()).flashing("info" -> playremain)
          } else {
            val numplay = players.length
            val playremain = "You have " + numplay.toString + " player slots filled"
            Redirect(routes.PlayerController.listPlayers()).flashing("info" -> playremain)
          }
        } else {
          Redirect(routes.PlayerController.listPlayers()).flashing("info" -> "You have entered 6 players already!")
        }
      } else {
        players.append(player)
        val numplayR = 3 - players.length
        val playremain = "You have " + numplayR.toString + " player slots remaining in order to play"
        for(w <- players) {
          w.setArmyCount(35 - (5* (players.length-3)))
        }
        Redirect(routes.PlayerController.listPlayers()).flashing("info" -> playremain)
      }
    }

    val formValidationResult = form.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }
}
