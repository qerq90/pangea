package pangea.generator.monster

import pangea.domain.Rng
import pangea.model.monster.Rarity._
import pangea.model.monster.{Monster, MonsterRaceFactor, Race, Rarity}
import pangea.model.stats.FightStats

object MonsterGenerator {

  private val N = 1.1

  // Модификатор «Отмеченный тьмой»: 7% шанс, +20% ко всем показателям.
  // Доступен только мобам редкости Rare и выше — обычные/необычные «отмеченными»
  // не бывают.
  private val MarkedChance                = 7L
  private val MarkedMultiplier            = 1.2
  private val MarkedRarities: Set[Rarity] = Set(Rare, Mythical, Legendary)

  // Weighted pool: 50% Common, 28% Uncommon, 17% Rare, 4% Mythical, 1% Legendary
  private val rarityPool: List[Rarity] =
    List.fill(50)(Common) ++
      List.fill(28)(Uncommon) ++
      List.fill(17)(Rare) ++
      List.fill(4)(Mythical) ++
      List.fill(1)(Legendary)

  // Пул редкостей для гарантированно отмеченного моба: только Rare+ в тех же
  // относительных весах, что и в общем пуле (17 : 4 : 1).
  private val markedRarityPool: List[Rarity] =
    rarityPool.filter(MarkedRarities.contains)

  def generate(dungeonLevel: Int, rng: Rng): (Monster, Rng) = {
    val (race, rng1) = rng.pick(Race.values.toList)
    generateOfRace(dungeonLevel, race, rng1)
  }

  /** Генерация моба фиксированной расы (раса задаётся снаружи — например, в
    * цепочке боёв события «мобы с сокровищем» все бои одной расы). Редкость и
    * «отмеченность» по-прежнему роллятся как обычно.
    */
  def generateOfRace(
      dungeonLevel: Int,
      race: Race,
      rng: Rng
  ): (Monster, Rng) = {
    val (rarity, rng2)   = rng.pick(rarityPool)
    val stats            = buildStats(dungeonLevel, rarity, race)
    val (markRoll, rng3) = rng2.between(0L, 100L)
    val marked     = MarkedRarities.contains(rarity) && markRoll < MarkedChance
    val finalStats = if (marked) boost(stats, MarkedMultiplier) else stats
    (Monster(0L, dungeonLevel.toLong, race, rarity, finalStats, marked), rng3)
  }

  /** Гарантированно «Отмеченный тьмой» моб заданного уровня — для механики
    * выслеживания прохода вглубь. Редкость роллится среди Rare+ (в тех же
    * относительных весах, что и обычный ролл), статы усилены
    * `MarkedMultiplier`. Уровень сюда передаётся ЦЕЛЕВОЙ (куда игрок хочет
    * спуститься), а не текущий.
    */
  def generateMarked(dungeonLevel: Int, rng: Rng): (Monster, Rng) = {
    val (race, rng1)   = rng.pick(Race.values.toList)
    val (rarity, rng2) = rng1.pick(markedRarityPool)
    val stats = boost(buildStats(dungeonLevel, rarity, race), MarkedMultiplier)
    (Monster(0L, dungeonLevel.toLong, race, rarity, stats, marked = true), rng2)
  }

  // +X% ко всем показателям (атака/HP зажаты снизу единицей, как в buildStats).
  private def boost(s: FightStats, factor: Double): FightStats =
    FightStats(
      atk = (s.atk * factor).toLong.max(1L),
      hp = (s.hp * factor).toLong.max(1L),
      armor = (s.armor * factor).toLong,
      defence = (s.defence * factor).toLong,
      evasion = (s.evasion * factor).toLong,
      accuracy = (s.accuracy * factor).toLong,
      energy = 0L
    )

  private def buildStats(level: Int, rarity: Rarity, race: Race): FightStats = {
    val base = level.toDouble * rarity.factor * N
    val f    = MonsterRaceFactor.of(race)
    FightStats(
      atk = (14.0 * base * f.attackFactor).toLong.max(1L),
      hp = (36.0 * base * f.hpFactor).toLong.max(1L),
      armor = (18.0 * base * f.defenceFactor).toLong,
      defence = 0L,
      evasion = (16.25 * base * f.evasionFactor).toLong,
      accuracy = (16.5 * base * f.accuracyFactor).toLong,
      energy = 0L
    )
  }
}
