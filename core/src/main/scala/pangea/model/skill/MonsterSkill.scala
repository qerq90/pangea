package pangea.model.skill

import enumeratum._
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.service.state.states.battle.BattleState

/**
 * Активные навыки мобов. На данный момент у всех рас одинаковый пул из 4 умений;
 * в будущем планируется развести по расам. Кулдаунов нет: моб каждый ход роллит
 * шанс «кастануть скилл» (см. [[BattleState.monsterSkillHitChance]]); если шанс
 * прокнул — равновероятно выбирается одно из применимых умений и применяется
 * ПОВЕРХ обычной атаки.
 *
 * `template` — описание эффекта с двумя плейсхолдерами: `{name}` — имя моба,
 * `{}` — числовая величина (урон/исцеление/починка).
 */
sealed abstract class MonsterSkill(val label: String, val template: String) extends EnumEntry {
  /** Может ли моб сейчас полезно применить скилл (например, heal только если hp < max). */
  def applicable(battle: SoloPveBattle): Boolean

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
                       attackerInt = battle.monsterStats.concentration)
      val damage   = ((raw * (1.0 - reduct)).toLong).max(1L)
      val (newHp, newArmor) = MonsterSkill.applyPhysicalDamage(battle, hero, damage)
      val line     = template.replace("{name}", battle.toMonster.name).replace("{}", damage.toString)
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
      val line   = template.replace("{name}", battle.toMonster.name).replace("{}", damage.toString)
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
      val heal   = math.max(1L, (maxHp * 0.2).toLong)
      val newHp  = (battle.monsterCurrentHp + heal).min(maxHp)
      val healed = newHp - battle.monsterCurrentHp
      val line   = template.replace("{name}", battle.toMonster.name).replace("{}", healed.toString)
      // Лечение отравленной цели ослабляет яд на HealCut п.п. (см. Poison.weakenedByHeal).
      val weakened = battle.effects.copy(
        monsterPoison = battle.effects.monsterPoison.flatMap(_.weakenedByHeal)
      )
      Cast(battle.copy(monsterCurrentHp = newHp, effects = weakened), hero.fightStats.hp, hero.fightStats.armor, line)
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
      val line    = template.replace("{name}", battle.toMonster.name).replace("{}", gained.toString)
      Cast(battle.copy(monsterCurrentArmor = newArm), hero.fightStats.hp, hero.fightStats.armor, line)
    }
  }

  /** Максимум брони моба: armor × defence (как при `SoloPveBattle.fromMonster`). */
  def monsterMaxArmor(battle: SoloPveBattle): Long =
    battle.monsterStats.armor * battle.monsterStats.defence.max(1L)

  /** Урон по герою с учётом баффовой брони и текущей физической брони. Возвращает
   *  новые `hp` и `armor` героя. */
  def applyPhysicalDamage(battle: SoloPveBattle, hero: Hero, damage: Long): (Long, Long) = {
    val buffReduct  = math.min(battle.heroBattleState.armorBonus, damage)
    val afterBuff   = damage - buffReduct
    val curArmor    = hero.fightStats.armor.max(0L)
    val armorAbsorb = math.min(curArmor, afterBuff)
    val hpDmg       = afterBuff - armorAbsorb
    val newArmor    = curArmor - armorAbsorb
    val newHp       = (hero.fightStats.hp - hpDmg).max(0L)
    (newHp, newArmor)
  }
}
