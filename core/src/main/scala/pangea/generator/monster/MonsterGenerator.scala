package pangea.generator.monster

import pangea.domain.Rng
import pangea.model.monster.Rarity._
import pangea.model.monster.{Monster, MonsterRaceFactor, Race, Rarity}
import pangea.model.skill.MonsterEnergy
import pangea.model.stats.FightStats

/** Ставки башни со стрелком — за уровень. */
object TowerRates {
  val AtkPerLvl: Long      = 20L
  val AccuracyPerLvl: Long = 30L
  val DefencePerLvl: Long  = 20L
  val ArmorPerLvl: Long    = 150L
  val HpPerLvl: Long       = 45L
}

object MonsterGenerator {

  private val N = 1.1

  // Прибавки к базовым ставкам защиты и брони: идут ДО умножения на уровень,
  // редкость и расу, поэтому растут вместе с ними и чувствуются на всю игру.
  private val DefenceBonus = 2.0
  private val ArmorBonus   = 1.0

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

  /** Башня со стрелком — сооружение с собственными ставками: стоит крепко
    * (броня и HP), бьёт метко, но небыстро, и ни энергии, ни уклонения у неё
    * нет. Редкость средняя нарочно: легендарные и мифические зовут сородичей,
    * а звать башне некого. */
  def tower(lvl: Int): Monster = {
    val l = lvl.toLong.max(1L)
    Monster(0L, l, Race.Construct, Rarity.Rare, FightStats(
      atk      = TowerRates.AtkPerLvl * l,
      hp       = TowerRates.HpPerLvl * l,
      armor    = TowerRates.ArmorPerLvl * l,
      defence  = TowerRates.DefencePerLvl * l,
      evasion  = 0L,
      accuracy = TowerRates.AccuracyPerLvl * l,
      energy   = 0L))
  }

  def generate(dungeonLevel: Int, rng: Rng): (Monster, Rng) = {
    val (race, rng1) = rng.pick(Race.mortals.toList)
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
    val (name, rng4) = legendaryName(race, rarity, rng3)
    (Monster(0L, dungeonLevel.toLong, race, rarity, finalStats, marked, name), rng4)
  }

  /** Имя легендарного: из списка его расы, наугад. У прочих редкостей имени
    * нет — их зовут по расе и тиру (см. [[Monster.namesByRaceRarity]]).
    * Бросок тратится только на легендарных: у остальных RNG не трогается, и
    * порядок бросков в боевых тестах не плывёт. */
  def legendaryName(race: Race, rarity: Rarity, rng: Rng): (Option[String], Rng) =
    if (rarity != Legendary) (None, rng)
    else Monster.legendaryNames.get(race).filter(_.nonEmpty) match {
      case None        => (None, rng)
      case Some(names) => val (n, next) = rng.pick(names); (Some(n), next)
    }

  /** То же, но когда имя нужно само по себе — например, чтобы запомнить его в
    * сцене и показать до боя тем же, кем он потом и выйдет. */
  def legendaryName(race: Race, rng: Rng): (Option[String], Rng) =
    legendaryName(race, Legendary, rng)

  /** Моб заданных расы и редкости — когда сюжет решает сам, кто вышел навстречу
    * (трое бандитов первых трёх тиров, брат девицы). Метки тьмы у таких нет. */
  def generateOfRaceAndRarity(
      dungeonLevel: Int, race: Race, rarity: Rarity, name: Option[String] = None
  ): Monster =
    Monster(0L, dungeonLevel.toLong, race, rarity, buildStats(dungeonLevel, rarity, race),
      customName = name)

  /** Гарантированно «Отмеченный тьмой» моб заданного уровня — для механики
    * выслеживания прохода вглубь. Редкость роллится среди Rare+ (в тех же
    * относительных весах, что и обычный ролл), статы усилены
    * `MarkedMultiplier`. Уровень сюда передаётся ЦЕЛЕВОЙ (куда игрок хочет
    * спуститься), а не текущий.
    */
  def generateMarked(dungeonLevel: Int, rng: Rng): (Monster, Rng) = {
    val (race, rng1)   = rng.pick(Race.mortals.toList)
    val (rarity, rng2) = rng1.pick(markedRarityPool)
    val stats = boost(buildStats(dungeonLevel, rarity, race), MarkedMultiplier)
    val (name, rng3) = legendaryName(race, rarity, rng2)
    (Monster(0L, dungeonLevel.toLong, race, rarity, stats, marked = true, name), rng3)
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
      // Энергию «отмеченность» не трогает: цены умений от неё не зависят.
      energy = s.energy
    )

  private def buildStats(level: Int, rarity: Rarity, race: Race): FightStats = {
    val base = level.toDouble * rarity.factor * N
    val f    = MonsterRaceFactor.of(race)
    FightStats(
      atk = (10.0 * base * f.attackFactor).toLong.max(1L),
      hp = (40.0 * base * f.hpFactor).toLong.max(1L),
      armor = ((22.0 + ArmorBonus) * base * f.armorFactor).toLong,
      // Защита — процентное снижение урона, третий слой поверх брони и HP.
      // Растёт линейно по уровню, как и пробитие героя, — поэтому доля
      // срезанного держится ровной всю игру (см. BattleState.pierce).
      defence = ((5.0 + DefenceBonus) * base * f.defenceFactor).toLong,
      evasion = (16.25 * base * f.evasionFactor).toLong,
      accuracy = (16.5 * base * f.accuracyFactor).toLong,
      // Потолок энергии — из него моб платит за свои умения (см. MonsterEnergy).
      energy = MonsterEnergy.maxEnergy(level.toLong)
    )
  }
}
