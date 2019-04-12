package controllers

object AttackForm {
  import play.api.data.Form
  import play.api.data.Forms._

  case class AttackData(terr: Int, otherTerr: Int, attackDiceCount: Int, defenseDiceCount: Int)

  val attackForm = Form(
    mapping(
      "terr" -> number,
      "otherTerr" -> number,
      "attackDiceCount" -> number,
      "defenseDiceCount" -> number
    )(AttackData.apply)(AttackData.unapply)
  )
}
