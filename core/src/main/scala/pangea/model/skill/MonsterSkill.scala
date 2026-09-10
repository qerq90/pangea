package pangea.model.skill

import enumeratum._
import pangea.model.battle.{Element, Poison, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.monster.Race
import pangea.service.state.states.battle.BattleState

/**
 * Активные навыки мобов. Кулдаунов нет — есть ЭНЕРГИЯ: каждое умение стоит своих
 * очков (см. [[MonsterEnergy]]), моб копит их по столько-то за раунд и применяет
 * самое дорогое из того, что сейчас по карману, ПОВЕРХ обычной атаки.
 *
 * Четыре первых умения — базовые: они есть у любой расы и стоят 0.8 обычной цены.
 * Остальные раздаются по расам через [[races]] — у мурлока свои, у демона свои.
 *
 * `template` — описание эффекта с двумя плейсхолдерами: `{name}` — имя моба,
 * `{}` — числовая величина (урон/исцеление/починка).
 */
sealed abstract class MonsterSkill(val label: String, val template: String) extends EnumEntry {
  /** Может ли моб сейчас полезно применить скилл (например, heal только если hp < max). */
  def applicable(battle: SoloPveBattle): Boolean

  /** Расы, которым доступно умение. Пусто — базовое умение, доступное всем. */
  def races: Set[Race] = Set.empty

  /** Доля от базовой цены умения на этом уровне (см. [[MonsterEnergy]]). */
  def costFactor: Double = MonsterEnergy.BasicCostFactor

  /** Цена в энергии на уровне моба. */
  def cost(lvl: Long): Long = MonsterEnergy.cost(lvl, costFactor)

  /** Доступно ли умение этой расе. */
  def availableTo(race: Race): Boolean = races.isEmpty || races.contains(race)

  /** Применяет скилл, возвращает обновлённое состояние боя, новые статы героя и
   *  готовую текстовую строку. Реализации не делают никаких рандомов — рандом
   *  только при выборе скилла снаружи. */
  def cast(battle: SoloPveBattle, hero: Hero, nowMs: Long): MonsterSkill.Cast
}

object MonsterSkill extends Enum[MonsterSkill] {
  val values: IndexedSeq[MonsterSkill] = findValues

  /** Результат применения навыка моба: обновлённый бой, новые hp/armor героя и текст. */
  final case class Cast(
    battle:    SoloPveBattle,
    heroHp:    Long,
    heroArmor: Long,
    line:      String
  )

  case object QuickStrike extends MonsterSkill(
    label    = "Быстрый удар",
    template = "{name} делает быстрые атаки на {} урона!"
  ) {
    def applicable(battle: SoloPveBattle): Boolean = true

    def cast(battle: SoloPveBattle, hero: Hero, nowMs: Long): Cast = {
      val effHero  = hero.effectiveFightStats(nowMs)
      val buffed   = battle.heroBattleState.applyTo(effHero)
      val raw      = math.max(1L, (battle.monsterStats.atk * 0.5).toLong)
      val reduct   = BattleState.damageReduction(
                       protection  = buffed.defence,
                       defenderInt = hero.effectiveBaseStats(nowMs).int,
                       // «Интеллект» моба в атаке = его атака (у мобов нет отдельного стата интеллекта).
                       attackerInt = battle.monsterStats.atk,
                       bonusPct    = battle.heroBattleState.reductionBonusPct)
      val damage   = ((raw * (1.0 - reduct)).toLong).max(1L)
      val (newHp, newArmor) = MonsterSkill.applyPhysicalDamage(battle, hero, damage)
      val line     = template.replace("{name}", battle.monsterName).replace("{}", damage.toString)
      Cast(battle, newHp, newArmor, line)
    }
  }

  case object CrushingStrike extends MonsterSkill(
    label    = "Дробящий удар",
    template = "{name} бьёт плашмя прямо по голове, нанеся {} урона!"
  ) {
    def applicable(battle: SoloPveBattle): Boolean = true

    def cast(battle: SoloPveBattle, hero: Hero, nowMs: Long): Cast = {
      // Игнорирует и damageReduction игрока, и его физическую броню — урон уходит сразу в HP.
      val damage = math.max(1L, (battle.monsterStats.atk * 0.3).toLong)
      val newHp  = (hero.fightStats.hp - damage).max(0L)
      val line   = template.replace("{name}", battle.monsterName).replace("{}", damage.toString)
      Cast(battle, newHp, hero.fightStats.armor, line)
    }
  }

  case object HealingFlask extends MonsterSkill(
    label    = "Исцеляющая фляга",
    template = "{name} выпивает из своей фляги и восстанавливает {} HP!"
  ) {
    def applicable(battle: SoloPveBattle): Boolean = battle.monsterCurrentHp < battle.monsterStats.hp

    def cast(battle: SoloPveBattle, hero: Hero, nowMs: Long): Cast = {
      val maxHp  = battle.monsterStats.hp
      val rawHeal = math.max(1L, (maxHp * 0.2).toLong)
      // Горение ослабляет лечение на (50 + pct)% и снимается им до 0.
      val weakenPct = battle.effects.monsterBurn.map(_.healWeakenPct).getOrElse(0)
      val heal   = (rawHeal * (100 - weakenPct) / 100).max(0L)
      val newHp  = (battle.monsterCurrentHp + heal).min(maxHp)
      val healed = newHp - battle.monsterCurrentHp
      val line   = template.replace("{name}", battle.monsterName).replace("{}", healed.toString)
      // Лечение: яд ослабляется на HealCut п.п.; кровотечение и горение снимаются
      // ПОЛНОСТЬЮ (см. Poison.weakenedByHeal / Bleed / Burn).
      val healedEffects = battle.effects.copy(
        monsterPoison = battle.effects.monsterPoison.flatMap(_.weakenedByHeal),
        monsterBleed  = None,
        monsterBurn   = None
      )
      Cast(battle.copy(monsterCurrentHp = newHp, effects = healedEffects), hero.fightStats.hp, hero.fightStats.armor, line)
    }
  }

  case object EmergencyRepair extends MonsterSkill(
    label    = "Экстренная починка",
    template = "{name} выливает металлическую жижу на свои доспехи. Повреждения в его брони затягиваются на глазах! Восстановлено {} брони."
  ) {
    def applicable(battle: SoloPveBattle): Boolean = battle.monsterCurrentArmor < monsterMaxArmor(battle)

    def cast(battle: SoloPveBattle, hero: Hero, nowMs: Long): Cast = {
      val maxArm  = monsterMaxArmor(battle)
      val repair  = math.max(1L, (maxArm * 0.2).toLong)
      val newArm  = (battle.monsterCurrentArmor + repair).min(maxArm)
      val gained  = newArm - battle.monsterCurrentArmor
      val line    = template.replace("{name}", battle.monsterName).replace("{}", gained.toString)
      Cast(battle.copy(monsterCurrentArmor = newArm), hero.fightStats.hp, hero.fightStats.armor, line)
    }
  }

  /** «Порошок!» — одноразовая заготовка: моб посыпает оружие и до конца боя бьёт
   *  иначе. Что именно даёт порошок, решает раса: у мурлока и эльфа — яд на
   *  атаках, у демона, гнома и каджита — стихия. Текст у всех один: игрок видит
   *  только, что моб что-то высыпал, и должен насторожиться сам. */
  sealed abstract class Powder(race: Race) extends MonsterSkill(
    label    = "Порошок!",
    template = "{name} достал странную пыль и высыпал на своё оружие. Надо быть осторожнее."
  ) {
    /** Чем порошок меняет бой — накладывается один раз, при высыпании. */
    protected def enchant(effects: pangea.model.battle.BattleEffects): pangea.model.battle.BattleEffects

    override val races: Set[Race]   = Set(race)
    override val costFactor: Double = MonsterEnergy.RacialCostFactor

    /** Ровно один раз за бой: высыпать дважды нечего. */
    def applicable(battle: SoloPveBattle): Boolean = !battle.effects.monsterPowderUsed

    def cast(battle: SoloPveBattle, hero: Hero, nowMs: Long): Cast = {
      val enchanted = enchant(battle.effects).copy(monsterPowderUsed = true)
      Cast(battle.copy(effects = enchanted), hero.fightStats.hp, hero.fightStats.armor,
        template.replace("{name}", battle.monsterName))
    }
  }

  /** Порошок, от которого удары начинают травить. */
  sealed trait PoisonPowder { self: Powder =>
    protected def enchant(e: pangea.model.battle.BattleEffects) = e.copy(monsterPoisonsOnHit = true)
  }

  /** Порошок, переводящий удары в стихию. */
  sealed abstract class ElementPowder(race: Race, element: Element) extends Powder(race) {
    protected def enchant(e: pangea.model.battle.BattleEffects) =
      e.copy(monsterAttackElement = Some(element.entryName))
  }

  case object MurlocPowder  extends Powder(Race.Murloc) with PoisonPowder
  case object ElfPowder     extends Powder(Race.Elf)    with PoisonPowder
  case object DemonPowder   extends ElementPowder(Race.Demon,   Element.Fire)
  case object GnomePowder   extends ElementPowder(Race.Gnome,   Element.Cold)
  case object KhajiitPowder extends ElementPowder(Race.Khajiit, Element.Air)

  /** Мурлочий «Грязный удар»: бьёт слабее обычного, зато всегда травит. Защита
   *  героя срезает урон процентно, как и у прочих ударов. */
  case object DirtyStrike extends MonsterSkill(
    label    = "Грязный удар",
    template = "{name} делает грязную атаку на {} урона! Вы чувствуете как по вашему телу медленно растекается яд."
  ) {
    val DamageFactor: Double = 0.6
    val PoisonPct: Int       = Poison.OnHit

    override val races: Set[Race]   = Set(Race.Murloc)
    override val costFactor: Double = MonsterEnergy.RacialCostFactor

    def applicable(battle: SoloPveBattle): Boolean = true

    def cast(battle: SoloPveBattle, hero: Hero, nowMs: Long): Cast = {
      val raw     = math.max(1L, (battle.monsterStats.atk * DamageFactor).toLong)
      val defence = hero.effectiveFightStats(nowMs).defence.max(0L)
      // «−% защиты игрока»: защита режет удар процентно, но не в ноль.
      val damage  = (raw * (100L - defence.min(90L)) / 100L).max(1L)
      val (newHp, newArmor) = applyPhysicalDamage(battle, hero, damage)
      val poisoned = battle.effects.copy(heroPoison = Some(
        battle.effects.heroPoison.map(p => Poison(p.pct + PoisonPct)).getOrElse(Poison(PoisonPct))))
      Cast(battle.copy(effects = poisoned), newHp, newArmor,
        template.replace("{name}", battle.monsterName).replace("{}", damage.toString))
    }
  }

  /** Максимум брони моба — ровно его броня. Защита к броне отношения не имеет:
    * это отдельный слой, процентное снижение урона (см. BattleState.pierce). */
  def monsterMaxArmor(battle: SoloPveBattle): Long =
    battle.monsterStats.armor

  /** Урон по герою с учётом баффовой брони и текущей физической брони. Возвращает
   *  новые `hp` и `armor` героя.
   *
   *  «Каменный страж» (порог 6) режет РАСХОД брони: прикрывает она по-прежнему
   *  весь поглощённый урон, но тает при этом медленнее. */
  def applyPhysicalDamage(battle: SoloPveBattle, hero: Hero, damage: Long): (Long, Long) = {
    val buffReduct  = math.min(battle.heroBattleState.armorBonus, damage)
    val afterBuff   = damage - buffReduct
    val curArmor    = hero.fightStats.armor.max(0L)
    val armorAbsorb = math.min(curArmor, afterBuff)
    val hpDmg       = afterBuff - armorAbsorb
    val newArmor    = curArmor - hero.sets.armorSpent(armorAbsorb)
    val newHp       = (hero.fightStats.hp - hpDmg).max(0L)
    (newHp, newArmor)
  }
}
