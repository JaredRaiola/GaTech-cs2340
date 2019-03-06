package models

class Territory(val name: String, var ownerName: String = "", var armyCount: Int = 0) {

  final override def equals(other: Any) = other match {
    case that: Territory => name == that.name
    case _ => false
  }

  final override def hashCode = (name).##
}
