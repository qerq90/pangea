package pangea.generator.monster

import pangea.domain.Rng
import pangea.model.monster.Rarity._
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.stats.FightStats

object MonsterGenerator {

  private val N = 1.1

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
    (Monster(0L, dungeonLevel.toLong, race, rarity, stats), rng2)
  }

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
