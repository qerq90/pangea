package pangea.model.monster

/** Расовые множители для генерации боевых характеристик монстров.
 *  Применяются только к монстрам (см. MonsterGenerator). */
case class MonsterRaceFactor(
  hpFactor: Double,
  defenceFactor: Double,
  attackFactor: Double,
  concentrationFactor: Double,
  accuracyFactor: Double,
  evasionFactor: Double
)

object MonsterRaceFactor {
  private val byRace: Map[Race, MonsterRaceFactor] = Map(
    Race.Human   -> MonsterRaceFactor(1.0, 1.5, 1.0, 2.0, 1.0, 0.7),
    Race.Elf     -> MonsterRaceFactor(1.0, 1.0, 0.6, 1.4, 1.6, 1.6),
    Race.Murloc  -> MonsterRaceFactor(1.2, 0.8, 1.5, 1.3, 1.5, 0.5),
    Race.Orc     -> MonsterRaceFactor(1.2, 1.5, 2.0, 0.8, 0.6, 0.4),
    Race.Goblin  -> MonsterRaceFactor(0.6, 0.5, 1.0, 1.1, 1.5, 2.0),
    Race.Demon   -> MonsterRaceFactor(0.8, 0.8, 1.5, 1.0, 1.2, 0.7),
    Race.Gnome   -> MonsterRaceFactor(1.2, 2.0, 1.5, 1.3, 1.2, 0.5),
    Race.Khajiit -> MonsterRaceFactor(1.2, 0.6, 1.6, 1.5, 1.3, 2.0)
  )

  def of(race: Race): MonsterRaceFactor = byRace(race)
}
