package controllers

object AttackForm {
  import play.api.data.Form
  import play.api.data.Forms._

  case class AttackData(terr: String, otherTerr: String, attackDiceCount: Int, defenseDiceCount: Int)

  val attackForm = Form(
    mapping(
      "terr" -> nonEmptyText,
      "otherTerr" -> nonEmptyText,
      "attackDiceCount" -> number,
      "defenseDiceCount" -> number
    )(AttackData.apply)(AttackData.unapply)
  )
}
