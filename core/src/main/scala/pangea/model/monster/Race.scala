package pangea.model.monster

import enumeratum._
import pangea.model.monster.Race.RaceFactor

sealed trait Race extends EnumEntry {
  val factor: RaceFactor
}

object Race extends Enum[Race] with DoobieEnum[Race] {
  case class RaceFactor(
    hpFactor: Double,
    defenceFactor: Double,
    attackFactor: Double,
    concentrationFactor: Double,
    accuracyFactor: Double,
    evasionFactor: Double
  )

  val values = findValues

  case object Human extends Race {
    override def toString: String = "Человек"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 1,
        defenceFactor = 1.5,
        attackFactor = 1,
        concentrationFactor = 2,
        accuracyFactor = 1,
        evasionFactor = 0.7
      )
  }

  case object Elf extends Race {
    override def toString: String = "Человек"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 1,
        defenceFactor = 1,
        attackFactor = 0.6,
        concentrationFactor = 1.4,
        accuracyFactor = 1.6,
        evasionFactor = 1.6
      )
  }

  case object Murloc extends Race {
    override def toString: String = "Мурлок"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 1.2,
        defenceFactor = 0.8,
        attackFactor = 1.5,
        concentrationFactor = 1.3,
        accuracyFactor = 1.5,
        evasionFactor = 0.5
      )
  }

  case object Orc extends Race {
    override def toString: String = "Орк"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 1.2,
        defenceFactor = 1.5,
        attackFactor = 2,
        concentrationFactor = 0.8,
        accuracyFactor = 0.6,
        evasionFactor = 0.4
      )
  }

  case object Goblin extends Race {
    override def toString: String = "Гоблин"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 0.6,
        defenceFactor = 0.5,
        attackFactor = 1,
        concentrationFactor = 1.1,
        accuracyFactor = 1.5,
        evasionFactor = 2
      )
  }

  case object Demon extends Race {
    override def toString: String = "Демон"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 0.8,
        defenceFactor = 0.8,
        attackFactor = 1.5,
        concentrationFactor = 1,
        accuracyFactor = 1.2,
        evasionFactor = 0.7
      )
  }

  case object Gnome extends Race {
    override def toString: String = "Гном"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 1.2,
        defenceFactor = 2,
        attackFactor = 1.5,
        concentrationFactor = 1.3,
        accuracyFactor = 1.2,
        evasionFactor = 0.5
      )
  }

  case object Khajiit extends Race {
    override def toString: String = "Каджит"

    val factor: RaceFactor =
      RaceFactor(
        hpFactor = 1.2,
        defenceFactor = 0.6,
        attackFactor = 1.6,
        concentrationFactor = 1.5,
        accuracyFactor = 1.3,
        evasionFactor = 2
      )
  }

}
