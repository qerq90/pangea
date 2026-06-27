package pangea.generator.monster

import pangea.domain.Rng
import pangea.model.monster.Rarity._
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.stats.FightStats

object MonsterGenerator {

  private val N = 1.1

  // Модификатор «Отмеченный тьмой»: 5% шанс, +20% ко всем показателям.
  // Доступен только мобам редкости Rare и выше — обычные/необычные «отмеченными»
  // не бывают.
  private val MarkedChance     = 5L
  private val MarkedMultiplier = 1.2
  private val MarkedRarities: Set[Rarity] = Set(Rare, Mythical, Legendary)

  // Weighted pool: 50% Common, 25% Uncommon, 15% Rare, 7% Mythical, 3% Legendary
  private val rarityPool: List[Rarity] =
    List.fill(50)(Common) ++
    List.fill(25)(Uncommon) ++
    List.fill(15)(Rare) ++
    List.fill(7)(Mythical) ++
    List.fill(3)(Legendary)

  def generate(dungeonLevel: Int, rng: Rng): (Monster, Rng) = {
    val (race,   rng1) = rng.pick(Race.values.toList)
    val (rarity, rng2) = rng1.pick(rarityPool)
    val stats          = buildStats(dungeonLevel, rarity, race)
    val (markRoll, rng3) = rng2.between(0L, 100L)
    val marked         = MarkedRarities.contains(rarity) && markRoll < MarkedChance
    val finalStats     = if (marked) boost(stats, MarkedMultiplier) else stats
    (Monster(0L, dungeonLevel.toLong, race, rarity, finalStats, marked), rng3)
  }

  // +X% ко всем показателям (атака/HP зажаты снизу единицей, как в buildStats).
  private def boost(s: FightStats, factor: Double): FightStats =
    FightStats(
      atk           = (s.atk * factor).toLong.max(1L),
      hp            = (s.hp * factor).toLong.max(1L),
      armor         = (s.armor * factor).toLong,
      defence       = (s.defence * factor).toLong,
      evasion       = (s.evasion * factor).toLong,
      accuracy      = (s.accuracy * factor).toLong,
      concentration = (s.concentration * factor).toLong
    )

  private def buildStats(level: Int, rarity: Rarity, race: Race): FightStats = {
    val base = level.toDouble * rarity.factor * N
    val f    = race.factor
    FightStats(
      atk           = (16.0  * base * f.attackFactor).toLong.max(1L),
      hp            = (32.0  * base * f.hpFactor).toLong.max(1L),
      armor         = (16.0  * base * f.defenceFactor).toLong,
      defence       = 0L,
      evasion       = (16.25 * base * f.evasionFactor).toLong,
      accuracy      = (16.5  * base * f.accuracyFactor).toLong,
      concentration = (16.0  * base * f.concentrationFactor).toLong
    )
  }
}
