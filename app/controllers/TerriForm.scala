package controllers

object TerriForm {
  import play.api.data.Form
  import play.api.data.Forms._

  /**
   * A form processing DTO that maps to the form below.
   *
   * Using a class specifically for form binding reduces the chances
   * of a parameter tampering attack and makes code clearer.
   */
  case class TerritoryData(terr: Int)

  /**
   * The form definition for the "create a widget" form.
   * It specifies the form fields and their types,
   * as well as how to convert from a Data to form data and vice versa.
   */
  val terriform = Form(
    mapping(
      "terr" -> number
    )(TerritoryData.apply)(TerritoryData.unapply)
  )
}
