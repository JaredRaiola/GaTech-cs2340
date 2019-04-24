package controllers

object FortifyForm {
  import play.api.data.Forms._
  import play.api.data.Form

  case class FortifyData(terrFrom: String, terrToFortify: String, numArmies: String)

  val fortifyForm = Form(
    mapping(
      "terrFrom" -> nonEmptyText,
      "terrToFortify" -> nonEmptyText,
      "numArmies" -> nonEmptyText
    )(FortifyData.apply)(FortifyData.unapply)
  )
}
