package pangea.model.monster

/** Расовые множители для генерации боевых характеристик монстров. Применяются
  * только к монстрам (см. MonsterGenerator).
  */
case class MonsterRaceFactor(
  hpFactor: Double,
  // Множитель БРОНИ — пула, который выбивают перед HP.
  armorFactor: Double,
  // Множитель ЗАЩИТЫ — процентного снижения урона. Ряд отдельный и подобран
  // в противовес броне: у кого толстая броня (гном, человек), защита низкая,
  // и наоборот — гоблин почти не бронирован, зато по нему трудно попасть
  // как следует. Так ни одна раса не становится толстой дважды.
  defenceFactor: Double,
  attackFactor: Double,
  accuracyFactor: Double,
  evasionFactor: Double
)

object MonsterRaceFactor {
  private val byRace: Map[Race, MonsterRaceFactor] = Map(
    Race.Human -> MonsterRaceFactor(
      hpFactor = 1.0,
      armorFactor = 1.5,
      defenceFactor = 1.0,
      attackFactor = 1.0,
      accuracyFactor = 0.8,
      evasionFactor = 0.7
    ),
    Race.Elf -> MonsterRaceFactor(
      hpFactor = 1.0,
      armorFactor = 1.0,
      defenceFactor = 2.0,
      attackFactor = 0.7,
      accuracyFactor = 1.4,
      evasionFactor = 1.6
    ),
    Race.Murloc -> MonsterRaceFactor(
      hpFactor = 1.2,
      armorFactor = 0.8,
      defenceFactor = 1.0,
      attackFactor = 1.5,
      accuracyFactor = 1.3,
      evasionFactor = 0.5
    ),
    Race.Orc -> MonsterRaceFactor(
      hpFactor = 1.2,
      armorFactor = 1.5,
      defenceFactor = 1.0,
      attackFactor = 1.8,
      accuracyFactor = 0.5,
      evasionFactor = 0.4
    ),
    Race.Goblin -> MonsterRaceFactor(
      hpFactor = 0.6,
      armorFactor = 0.5,
      defenceFactor = 3.0,
      attackFactor = 1.0,
      accuracyFactor = 1.3,
      evasionFactor = 2.0
    ),
    Race.Demon -> MonsterRaceFactor(
      hpFactor = 0.8,
      armorFactor = 0.8,
      defenceFactor = 1.2,
      attackFactor = 1.5,
      accuracyFactor = 1.0,
      evasionFactor = 0.7
    ),
    Race.Gnome -> MonsterRaceFactor(
      hpFactor = 1.2,
      armorFactor = 2.0,
      defenceFactor = 0.6,
      attackFactor = 1.5,
      accuracyFactor = 1.0,
      evasionFactor = 0.5
    ),
    Race.Khajiit -> MonsterRaceFactor(
      hpFactor = 1.2,
      armorFactor = 0.6,
      defenceFactor = 0.5,
      attackFactor = 1.6,
      accuracyFactor = 1.1,
      evasionFactor = 2.0
    )
  )

  def of(race: Race): MonsterRaceFactor = byRace(race)
}
