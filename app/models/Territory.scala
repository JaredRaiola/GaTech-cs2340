package models

class Territory(val id: Int, val name: String, var ownerName: String = "", var armyCount: Int = 0) {

  final override def equals(other: Any) = other match {
    case that: Territory => id == that.id
    case _ => false
  }

  final override def hashCode = (id).##
}
