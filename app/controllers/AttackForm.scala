package controllers

object AttackForm {
  import play.api.data.Form
  import play.api.data.Forms._

case class AttackData(terr: String, otherTerr: String, attackDiceCount: Int, defenceDiceCount: Int)

  val attackForm = Form(
    mapping(
      "terr" -> nonEmptyText,
      "otherTerr" -> nonEmptyText,
      "attackDiceCount" -> number,
      "defenceDiceCount" -> number
    )(AttackData.apply)(AttackData.unapply)
  )
}
