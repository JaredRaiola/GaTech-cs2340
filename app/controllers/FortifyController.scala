package controllers


import javax.inject.Inject
import models.Player
import models.Territory
import play.api.data._
import play.api.i18n._
import play.api.mvc._

class FortifyController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {

  import FortifyForm._

  def updateFortifyView:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    Ok(views.html.fortifyView(fortifyForm))
  }

  def fortifyTerritory:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[FortifyData] =>
      BadRequest(views.html.fortifyView(formWithErrors))
    }

    val successFunction = { data: FortifyData =>

      Redirect(routes.FortifyController.updateFortifyView).flashing("This" -> " Works")
    }
    val formValidationResult = fortifyForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }
}
