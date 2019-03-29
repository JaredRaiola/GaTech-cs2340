package controllers

object AdditionalArmiesForm {
  import play.api.data.Forms._
  import play.api.data.Form

  case class AdditionalArmiesData(terr: String, numArmies: Int)

  val additionalArmiesForm = Form(
    mapping(
      "terr" -> nonEmptyText,
      "numArmies" -> number
    )(AdditionalArmiesData.apply)(AdditionalArmiesData.unapply)
  )
}
