package pangea.model.monster

import enumeratum.{DoobieEnum, _}

sealed trait Rarity extends EnumEntry {
  val factor: Double
}

object Rarity extends Enum[Rarity] with DoobieEnum[Rarity] {

  val values = findValues

  case object Common extends Rarity {
    override def toString: String = "Обычный"
    val factor                    = 0.8
  }

  case object Uncommon extends Rarity {
    override def toString: String = "Необычный"
    val factor                    = 1.2
  }

  case object Rare extends Rarity {
    override def toString: String = "Редкий"
    val factor                    = 1.6
  }

  case object Mythical extends Rarity {
    override def toString: String = "Мифический"
    val factor                    = 2.0
  }

  case object Legendary extends Rarity {
    override def toString: String = "Легендарный"
    val factor                    = 3
  }
}
