package pangea.model.hero

import pangea.model.battle.SkillSlotState
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.trauma.{Trauma, TraumaPenalties}
import pangea.model.user.UserId

case class Hero(
  id: HeroId,
  userId: UserId,
  state: StateType,
  lvl: Long,
  exp: Long,
  upgradePoints: Long,
  race: Race,
  baseStats: BaseStats,
  fightStats: FightStats,
  equipment: Equipment,
  dungeonLevel: Int,
  maxDungeonLevel: Int,
  gold: Long,
  traumaUntil: Option[Long],
  traumaNames: List[String],
  guildReputation: Long,
  masterHornBoosts: MasterHornBoosts,
  doubloons: Long
) {
  /** Можно ли двигаться к тьме (глубже): следующий этаж открыт, только если на
   *  текущем (== максимально доступному) была повержена тьма — тогда
   *  `maxDungeonLevel` уже сдвинут вперёд. Этажи ≤ `maxDungeonLevel` доступны всегда. */
  def canGoDarker: Boolean = dungeonLevel < maxDungeonLevel

  /** Можно ли двигаться к свету (выше): на первом этаже выше уже некуда. */
  def canGoLighter: Boolean = dungeonLevel > 1

  // Защита больше не умножает броню (теперь она снижает урон процентно — см.
  // `BattleState.damageReduction`). Максимум брони — сумма брони со снаряжения
  // плюс прокачка у Мастера Горна.
  def maxArmor: Long = equipment.allArmor + masterHornBoosts.armor

  /** Максимум брони с учётом травм: штраф на броню режет потолок. Без травм
   *  равен `maxArmor`. Текущая броня (`fightStats.armor`) тратится в бою и
   *  восстанавливается до этого значения на отдыхе. */
  def effectiveMaxArmor(nowMs: Long): Long = {
    val p = combinedPenalties(nowMs)
    (maxArmor * (1.0 - p.armorPct)).toLong.max(0L)
  }


  def traumaActive(nowMs: Long): Boolean = traumaUntil.exists(_ > nowMs)

  def activeTraumas(nowMs: Long): List[Trauma] =
    if (!traumaActive(nowMs)) Nil
    else traumaNames.flatMap(Trauma.byName)

  def combinedPenalties(nowMs: Long): TraumaPenalties =
    activeTraumas(nowMs).foldLeft(TraumaPenalties.none)(_ + _.penalties)

  /** Боевые статы с заданными штрафами. Атака/защита/точность/уклонение зажаты
   *  снизу единицей (не могут упасть ниже 1). Броня здесь — текущее значение как
   *  есть: штраф травмы на броню влияет на её ПОТОЛОК (`effectiveMaxArmor`), а не
   *  режет текущий запас при каждом чтении (иначе урон считался бы неверно). */
  private def fightStatsWith(p: TraumaPenalties): FightStats =
    fightStats.copy(
      atk           = ((fightStats.atk + masterHornBoosts.attack) * (1.0 - p.atkPct)).toLong.max(1L),
      defence       = ((fightStats.defence + masterHornBoosts.defence) * (1.0 - p.defPct)).toLong.max(1L),
      accuracy      = ((fightStats.accuracy + masterHornBoosts.accuracy) * (1.0 - p.accPct)).toLong.max(1L),
      evasion       = ((fightStats.evasion + masterHornBoosts.evasion) * (1.0 - p.evasionPct)).toLong.max(1L),
      concentration = ((fightStats.concentration + masterHornBoosts.concentration) * (1.0 - p.concPct)).toLong.max(1L),
      armor         = fightStats.armor.max(0L)
    )

  /** Текущие боевые статы — с учётом активных травм. */
  def effectiveFightStats(nowMs: Long): FightStats = fightStatsWith(combinedPenalties(nowMs))

  /** Боевые статы без травм — «потолок», к которому стат вернётся после снятия травм. */
  def maxFightStats: FightStats = fightStatsWith(TraumaPenalties.none)

  /** Базовые характеристики с учётом расового бафа и травм (зажаты снизу единицей).
   *  Ловкость не имеет штрафа от травм. Расовый множитель применяется до травм с
   *  округлением вверх. */
  def effectiveBaseStats(nowMs: Long): BaseStats = {
    val p = combinedPenalties(nowMs)
    val b = HeroRaceBuff.of(race)
    BaseStats(
      agi = b.applyAgi(baseStats.agi),
      vit = (b.applyVit(baseStats.vit) * (1.0 - p.vitPct)).toLong.max(1L),
      str = (b.applyStr(baseStats.str) * (1.0 - p.strPct)).toLong.max(1L),
      int = (b.applyInt(baseStats.int) * (1.0 - p.intPct)).toLong.max(1L)
    )
  }

  /** Слоты активных навыков героя для боя: снимаются с надетых оружия и
   *  нагрудника (у каждого может быть `activeSkill`). Ключ слота — id предмета,
   *  поэтому два предмета с «одним» навыком катаются независимо. */
  def activeSkillSlots: List[SkillSlotState] =
    List(equipment.weapon, equipment.chestPlate)
      .flatMap(it => it.activeSkill.map(s => SkillSlotState(it.id, s)))

  def effectiveMaxHp(nowMs: Long): Long = {
    val p           = combinedPenalties(nowMs)
    val effectiveVit = HeroRaceBuff.of(race).applyVit(baseStats.vit)
    val base        = effectiveVit * 24L
    (base * (1.0 - p.vitPct) * (1.0 - p.hpPct)).toLong.max(1L) + equipment.allHp
  }

  def traumaRemainingText(nowMs: Long): Option[String] =
    traumaUntil.filter(_ > nowMs).map { until =>
      val secs    = (until - nowMs) / 1000L
      val hours   = secs / 3600
      val minutes = (secs % 3600) / 60
      s"${hours}ч ${minutes}мин"
    }

  def getInfo(nowMs: Long): String = {
    val effB     = effectiveBaseStats(nowMs)
    val eff      = effectiveFightStats(nowMs)
    val maxHp    = effectiveMaxHp(nowMs)
    val maxArm   = effectiveMaxArmor(nowMs)
    val curArm   = fightStats.armor.min(maxArm)
    s"""${race.toString}, Уровень $lvl
       | $getLvlExp/$getNeededExp опыта
       |
       | 💪 СИЛ ${effB.str}  ТЕЛО ${effB.vit}
       | 🏃 ЛОВ ${effB.agi}  ИНТ ${effB.int}
       |
       | ⚔ Атк ${eff.atk}  🎯 Точн ${eff.accuracy}
       | 🛡 Защ ${eff.defence}  👁 Укл ${eff.evasion}
       | ❤ ${fightStats.hp}/$maxHp  🧥 Броня $curArm/$maxArm
       |
       | Свободных очков: $upgradePoints
       |""".stripMargin
  }

  def getNeededExp: Long = Hero.neededExpForLevel(lvl)
  def getLvlExp: Long    = exp

  /** Начисление опыта с прокачкой уровней по лестнице Фибоначчи. Возвращает героя
   *  с обновлёнными `exp`/`lvl`/`upgradePoints` (4 очка характеристик за уровень,
   *  кап на `Hero.MaxLevel`). Единственное место расчёта прокачки. */
  def gainExp(amount: Long): Hero = {
    var e = exp + amount
    var l = lvl
    var p = upgradePoints
    while (e >= Hero.neededExpForLevel(l) && l < Hero.MaxLevel) {
      e -= Hero.neededExpForLevel(l)
      l += 1L
      p += Hero.PointsPerLevel
    }
    copy(exp = e, lvl = l, upgradePoints = p)
  }
}

object Hero {
  val MaxLevel: Long       = 150L
  val PointsPerLevel: Long = 4L

  /** Порог опыта для уровня по Фибоначчи: 100, 200, 300, 500, 800, 1300, …
   *  (= 100 × fib(lvl), где fib(1)=1, fib(2)=2, fib(n)=fib(n-1)+fib(n-2)). */
  def neededExpForLevel(lvl: Long): Long = fib(lvl) * 100L

  private def fib(n: Long): Long =
    if (n <= 1L) 1L
    else {
      var prev = 1L // fib(1)
      var cur  = 2L // fib(2)
      var i    = 2L
      while (i < n) {
        val next = prev + cur
        prev = cur
        cur = next
        i += 1L
      }
      cur
    }
}
