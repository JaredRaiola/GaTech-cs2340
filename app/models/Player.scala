class Player(val name: String, var armyBinCount: Int = 0, var turnMode: Int = 0) {
  //turn mode 0 is not their turn, 1 is place armies, 2 is attack, 3 is move, then 0
  //  def updateBinCount   depending on how many territories they own

  final override def equals(other: Any) = other match {
    case that: Player => name == that.name
    case _ => false
  }

  final override def hashCode = (name).##

  def selectMyTerritory(terr: Territory): Boolean = {
    terr.ownerName.equals(this.name)
  }

  def selectEnemyTerritory(terr: Territory): Boolean = {
    !terr.ownerName.equals(this.name)
  }

  def attack(myTerr: Territory, otherTerr: Territory): List[Int] = {
    if (!turnMode.equals(2)) return Nil
    Nil
    //stub
  }

  def attackLosses(mydice: Int, defenceDice: Int): List[Int] = {
    Nil
    //stub
  }
