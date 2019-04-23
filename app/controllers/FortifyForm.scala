package controllers

object FortifyForm {
  import play.api.data.Forms._
  import play.api.data.Form

  case class FortifyData(terr: String, numArmies: Int)

  val fortifyForm = Form(
    mapping(
      "terr" -> nonEmptyText,
      "numArmies" -> number
    )(FortifyData.apply)(FortifyData.unapply)
  )
}
