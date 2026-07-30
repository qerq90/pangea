package pangea.model.hero

import pangea.model.battle.SkillSlotState
import pangea.model.item.ItemDetails
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats, StatBoosts}
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
  doubloons: Long,
  statBoosts: StatBoosts
) {
  /** Можно ли двигаться к тьме (глубже): следующий этаж открыт, только если на
   *  текущем (== максимально доступному) была повержена тьма — тогда
   *  `maxDungeonLevel` уже сдвинут вперёд. Этажи ≤ `maxDungeonLevel` доступны всегда. */
  def canGoDarker: Boolean = dungeonLevel < maxDungeonLevel

  /** Пассивные навыки надетых предметов (дубли схлопнуты) — типизированный фасад
   *  для боя/лута/подземелья/инвентаря. */
  def passives: HeroPassives = HeroPassives(equipment.passiveKinds)

  /** Камни-усилители в гнёздах снаряжения — типизированный фасад для боя/лута. */
  def gems: HeroGems = HeroGems(equipment.weaponGems, equipment.armorGems)

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
      armor         = fightStats.armor.max(0L),
      energy        = fightStats.energy.max(0L)
    )

  /** Плоские прибавки к статам от пассивок: «Укреплённые» (+5% защиты) и «Точности»
   *  (+5% точности). Не мультипликативны — считаются от переданного значения и
   *  прибавляются (как бафы зелий пояса). Живут здесь, т.к. действуют всегда, пока
   *  предмет надет (в т.ч. на экране статов, не только в бою). */
  private def withPassiveStatBonuses(fs: FightStats): FightStats = {
    val p = passives
    fs.copy(
      defence  = fs.defence + p.defenceFlatBonus(fs.defence),
      accuracy = fs.accuracy + p.accuracyFlatBonus(fs.accuracy)
    )
  }

  /** Плоские прибавки к итоговым статам от камней (считаются от переданного, уже
   *  «почти итогового» значения, поверх бафов пассивок): аметист в оружии — к
   *  точности, аметист в снаряжении — к защите, изумруд в снаряжении — к уклонению. */
  private def withGemStatBonuses(fs: FightStats): FightStats = {
    val g = gems
    // Аметист → точность, аметист-броня → защита, изумруд-броня → уклонение,
    // воздух в оружии → +5% к итоговым уклонению и точности (постоянно).
    val accPct = g.accuracyBonusPct + g.airEvasionAccuracyBonusPct
    val evaPct = g.evasionBonusPct + g.airEvasionAccuracyBonusPct
    fs.copy(
      accuracy = fs.accuracy + fs.accuracy * accPct / 100L,
      defence  = fs.defence + fs.defence * g.defenceBonusPct / 100L,
      evasion  = fs.evasion + fs.evasion * evaPct / 100L
    )
  }

  /** Текущие боевые статы — с учётом активных травм, плоских бафов пассивок и камней. */
  def effectiveFightStats(nowMs: Long): FightStats =
    withGemStatBonuses(withPassiveStatBonuses(fightStatsWith(combinedPenalties(nowMs))))

  /** Реген перед атакой игрока от пассивок «Целебный» (4% макс.HP) и
   *  «Самовосстанавливающийся» (4% макс.брони). Прибавка каппится потолком, но
   *  никогда не опускает текущее значение ниже (если оно уже выше потолка —
   *  `.max(current)`). Величина показывается на экране боя приписками `(+N)`. */
  def withCombatRegen(nowMs: Long): Hero = {
    val p        = passives
    val maxHp    = effectiveMaxHp(nowMs)
    val maxArmor = effectiveMaxArmor(nowMs)
    // Реген HP = пассивка «Целебный» (% макс.HP) + черепа в снаряжении (промилле макс.HP).
    val hpRegen  = maxHp * p.hpRegenPct / 100L + maxHp * gems.hpRegenPerMille / 1000L
    val newHp    = (fightStats.hp + hpRegen).min(maxHp).max(fightStats.hp)
    val newArmor = (fightStats.armor + maxArmor * p.armorRegenPct / 100L).min(maxArmor).max(fightStats.armor)
    copy(fightStats = fightStats.copy(hp = newHp, armor = newArmor))
  }

  /** Боевые статы без травм — «потолок», к которому стат вернётся после снятия травм. */
  def maxFightStats: FightStats = fightStatsWith(TraumaPenalties.none)

  /** Базовые характеристики с учётом расового бафа и травм (зажаты снизу единицей).
   *  Ловкость не имеет штрафа от травм. Расовый множитель применяется до травм с
   *  округлением вверх. */
  def effectiveBaseStats(nowMs: Long): BaseStats = {
    val p = combinedPenalties(nowMs)
    val b = HeroRaceBuff.of(race)
    BaseStats(
      agi = (b.applyAgi(baseStats.agi) * statBoosts.agiFactor(nowMs)).toLong.max(1L),
      vit = (b.applyVit(baseStats.vit) * (1.0 - p.vitPct) * statBoosts.vitFactor(nowMs)).toLong.max(1L),
      str = (b.applyStr(baseStats.str) * (1.0 - p.strPct) * statBoosts.strFactor(nowMs)).toLong.max(1L),
      int = (b.applyInt(baseStats.int) * (1.0 - p.intPct) * statBoosts.intFactor(nowMs)).toLong.max(1L)
    )
  }

  /** Слоты активных навыков героя для боя: снимаются с надетых оружия и
   *  нагрудника (у каждого может быть `activeSkill`). Ключ слота — id предмета,
   *  поэтому два предмета с «одним» навыком катаются независимо. */
  def activeSkillSlots: List[SkillSlotState] =
    List(equipment.weapon, equipment.chestPlate).flatMap { it =>
      it.details match {
        case ItemDetails.Weapon(s) => Some(SkillSlotState(it.id, s, cooldown = s.initialCooldown))
        case ItemDetails.Armor(s)  => Some(SkillSlotState(it.id, s, cooldown = s.initialCooldown))
        case _                     => None
      }
    }

  /** Максимум Энергии: 5·Интеллект + 2·Ловкость + Энергия с экипировки + бусты
   *  Мастера Горна, за вычетом штрафа травм на энергию. Минимум 1. Интеллект и
   *  ловкость берутся эффективные (раса/травмы/бусты). Текущая энергия
   *  (`fightStats.energy`) тратится и восстанавливается до этого значения. */
  def maxEnergy(nowMs: Long): Long = {
    val p    = combinedPenalties(nowMs)
    val b    = effectiveBaseStats(nowMs)
    val base = 5L * b.int + 2L * b.agi + equipment.allEnergy + masterHornBoosts.energy
    // Бриллианты в снаряжении дают +% к макс. Энергии.
    (base * (1.0 - p.energyPct) * (100L + gems.energyBonusPct) / 100L).toLong.max(1L)
  }

  def effectiveMaxHp(nowMs: Long): Long = {
    val p           = combinedPenalties(nowMs)
    val effectiveVit = HeroRaceBuff.of(race).applyVit(baseStats.vit)
    val base        = effectiveVit * 24L
    val subtotal    = (base * (1.0 - p.vitPct) * (1.0 - p.hpPct) * statBoosts.vitFactor(nowMs)).toLong.max(1L) +
      equipment.allHp
    // Рубины в снаряжении: плоская прибавка + % к макс. HP.
    val g = gems
    (subtotal + g.flatHp) * (100L + g.maxHpBonusPct) / 100L
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
    val maxEn    = maxEnergy(nowMs)
    val curEn    = fightStats.energy.min(maxEn)
    s"""${race.toString}, Уровень $lvl
       | $getLvlExp/$getNeededExp опыта
       |
       | 💪 СИЛ ${effB.str}  ТЕЛО ${effB.vit}
       | 🏃 ЛОВ ${effB.agi}  ИНТ ${effB.int}
       |
       | ❤ ${fightStats.hp}/$maxHp  🧥 Броня $curArm/$maxArm  ⚡ Энергия $curEn/$maxEn
       | ⚔ Атк ${eff.atk}  🛡 Защ ${eff.defence}
       | 🎯 Точн ${eff.accuracy}  👁 Укл ${eff.evasion}
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
