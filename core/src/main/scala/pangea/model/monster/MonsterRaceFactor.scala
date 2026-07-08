package pangea.model.monster

/** Расовые множители для генерации боевых характеристик монстров. Применяются
  * только к монстрам (см. MonsterGenerator).
  */
case class MonsterRaceFactor(
  hpFactor: Double,
  defenceFactor: Double,
  attackFactor: Double,
  accuracyFactor: Double,
  evasionFactor: Double
)

object MonsterRaceFactor {
  private val byRace: Map[Race, MonsterRaceFactor] = Map(
    Race.Human -> MonsterRaceFactor(
      hpFactor = 1.0,
      defenceFactor = 1.5,
      attackFactor = 1.0,
      accuracyFactor = 0.8,
      evasionFactor = 0.7
    ),
    Race.Elf -> MonsterRaceFactor(
      hpFactor = 1.0,
      defenceFactor = 1.0,
      attackFactor = 0.7,
      accuracyFactor = 1.4,
      evasionFactor = 1.6
    ),
    Race.Murloc -> MonsterRaceFactor(
      hpFactor = 1.2,
      defenceFactor = 0.8,
      attackFactor = 1.5,
      accuracyFactor = 1.3,
      evasionFactor = 0.5
    ),
    Race.Orc -> MonsterRaceFactor(
      hpFactor = 1.2,
      defenceFactor = 1.5,
      attackFactor = 1.8,
      accuracyFactor = 0.5,
      evasionFactor = 0.4
    ),
    Race.Goblin -> MonsterRaceFactor(
      hpFactor = 0.6,
      defenceFactor = 0.5,
      attackFactor = 1.0,
      accuracyFactor = 1.3,
      evasionFactor = 2.0
    ),
    Race.Demon -> MonsterRaceFactor(
      hpFactor = 0.8,
      defenceFactor = 0.8,
      attackFactor = 1.5,
      accuracyFactor = 1.0,
      evasionFactor = 0.7
    ),
    Race.Gnome -> MonsterRaceFactor(
      hpFactor = 1.2,
      defenceFactor = 2.0,
      attackFactor = 1.5,
      accuracyFactor = 1.0,
      evasionFactor = 0.5
    ),
    Race.Khajiit -> MonsterRaceFactor(
      hpFactor = 1.2,
      defenceFactor = 0.6,
      attackFactor = 1.6,
      accuracyFactor = 1.1,
      evasionFactor = 2.0
    )
  )

  def of(race: Race): MonsterRaceFactor = byRace(race)
}
