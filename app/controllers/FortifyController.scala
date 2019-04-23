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
    // Pass an unpopulated form to the template
    Ok(views.html.fortifyView(fortifyForm))
  }

  // This will be the action that handles our form post
  def fortifyTerritory:Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[FortifyData] =>
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.fortifyView(formWithErrors))
    }

    val successFunction = { data: FortifyData =>
      // This is the good case, where the form was successfully parsed as a Data object.
      Redirect(routes.FortifyController.updateFortifyView).flashing("This" -> " Works")
    }
    val formValidationResult = fortifyForm.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }
}
