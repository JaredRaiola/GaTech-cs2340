package controllers

object AttackForm {
  import play.api.data.Form
  import play.api.data.Forms._

  case class AttackData(terr: String, otherTerr: String, attackDiceCount: String, defenceDiceCount: String)

  val attackForm = Form(
    mapping(
      "terr" -> nonEmptyText,
      "otherTerr" -> nonEmptyText,
      "attackDiceCount" -> nonEmptyText,
      "defenceDiceCount" -> nonEmptyText
    )(AttackData.apply)(AttackData.unapply)
  )
}
