package pangea.model.skill

import pangea.model.battle.SoloPveBattle
import pangea.model.monster.{Race, Rarity}
import pangea.model.stats.FightStats
import pangea.test.TestFixtures
import pangea.model.user.UserId
import zio.test._

object MonsterSkillSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)

  private def hero(hp: Long, armor: Long, defence: Long = 0L, int: Long = 10L) = {
    val base = TestFixtures.hero(userId)
    base.copy(
      baseStats  = base.baseStats.copy(int = int),
      fightStats = base.fightStats.copy(hp = hp, armor = armor, defence = defence, energy = 0)
    )
  }

  private def battle(
    mobAtk:        Long = 100,
    mobArmor:      Long = 10,
    mobDefence:    Long = 5,
    mobMaxHp:      Long = 1000,
    curHp:         Long = 1000,
    curArmor:      Long = 50
  ): SoloPveBattle = SoloPveBattle(
    monsterLvl          = 1L,
    monsterRace         = Race.Human.entryName,
    monsterRarity       = Rarity.Common.entryName,
    // energy у моба не используется (0): «интеллект» в формулах берётся из atk/defence.
    monsterStats        = FightStats(atk = mobAtk, hp = mobMaxHp, armor = mobArmor, defence = mobDefence,
                                     evasion = 0, accuracy = 0, energy = 0),
    monsterCurrentHp    = curHp,
    monsterCurrentArmor = curArmor
  )

  override def spec = suite("MonsterSkill")(

    suite("applicable")(
      test("QuickStrike и CrushingStrike — всегда применимы") {
        val b = battle()
        assertTrue(MonsterSkill.QuickStrike.applicable(b)) &&
        assertTrue(MonsterSkill.CrushingStrike.applicable(b))
      },

      test("HealingFlask: применим только если hp < max") {
        val fullHp = battle(curHp = 1000, mobMaxHp = 1000)
        val low    = battle(curHp = 500,  mobMaxHp = 1000)
        assertTrue(!MonsterSkill.HealingFlask.applicable(fullHp)) &&
        assertTrue(MonsterSkill.HealingFlask.applicable(low))
      },

      test("EmergencyRepair: применим только если armor < maxArmor") {
        // maxArmor = monsterStats.armor * monsterStats.defence (с min(1))
        val maxedOut = battle(mobArmor = 10, mobDefence = 5, curArmor = 50) // max = 10*5 = 50, текущая = 50
        val damaged  = battle(mobArmor = 10, mobDefence = 5, curArmor = 20) // max = 50, текущая = 20
        assertTrue(!MonsterSkill.EmergencyRepair.applicable(maxedOut)) &&
        assertTrue(MonsterSkill.EmergencyRepair.applicable(damaged))
      }
    ),

    suite("cast")(
      test("QuickStrike: урон = 0.5*atk с учётом damageReduction, проходит по броне героя") {
        // attackerInt моба = его атака (100). У героя effectiveFightStats clamp defence/int к 1.
        // reduction = (1+1)/((1+1)+100*2) = 2/202 ≈ 0.01 → почти без снижения.
        val b = battle(mobAtk = 100)
        val h = hero(hp = 100, armor = 20, defence = 0, int = 0)
        val cast = MonsterSkill.QuickStrike.cast(b, h, 0L)
        // raw = 100*0.5 = 50; damage ≈ 49; armor 20 поглощает 20 → hp ≈ 100 - 29
        assertTrue(cast.heroArmor == 0L) && assertTrue(cast.heroHp > 60L && cast.heroHp < 80L)
      },

      test("CrushingStrike: урон = 0.3*atk прямо в hp, игнорирует броню и защиту") {
        val b = battle(mobAtk = 100)
        // high defence → damageReduction для обычной атаки была бы большой, но Crushing её игнорит
        val h = hero(hp = 100, armor = 9999, defence = 9999, int = 1000)
        val cast = MonsterSkill.CrushingStrike.cast(b, h, 0L)
        // damage = max(1, 30) = 30; hp = 100 - 30 = 70; armor НЕ тратится
        assertTrue(cast.heroHp == 70L) &&
        assertTrue(cast.heroArmor == 9999L) &&
        assertTrue(cast.line.contains("30"))
      },

      test("HealingFlask: восстанавливает 20% от max, не выше max") {
        val b      = battle(curHp = 500, mobMaxHp = 1000)
        val cast   = MonsterSkill.HealingFlask.cast(b, hero(100, 0), 0L)
        // 20% от 1000 = 200; новое hp = 700
        assertTrue(cast.battle.monsterCurrentHp == 700L) &&
        assertTrue(cast.line.contains("200"))
      },

      test("HealingFlask: clamp до max hp моба") {
        val b    = battle(curHp = 950, mobMaxHp = 1000) // лечит +200, но clamp до 1000 → восстановлено 50
        val cast = MonsterSkill.HealingFlask.cast(b, hero(100, 0), 0L)
        assertTrue(cast.battle.monsterCurrentHp == 1000L) &&
        assertTrue(cast.line.contains("50"))
      },

      test("EmergencyRepair: восстанавливает 20% от maxArmor моба") {
        // maxArmor = 10 * 5 = 50; текущая 20; ремонт +10 → 30
        val b    = battle(mobArmor = 10, mobDefence = 5, curArmor = 20)
        val cast = MonsterSkill.EmergencyRepair.cast(b, hero(100, 0), 0L)
        assertTrue(cast.battle.monsterCurrentArmor == 30L) &&
        assertTrue(cast.line.contains("10"))
      },

      test("EmergencyRepair: clamp до maxArmor") {
        val b    = battle(mobArmor = 10, mobDefence = 5, curArmor = 45) // +10 → 55, clamp 50, gained 5
        val cast = MonsterSkill.EmergencyRepair.cast(b, hero(100, 0), 0L)
        assertTrue(cast.battle.monsterCurrentArmor == 50L) &&
        assertTrue(cast.line.contains("5"))
      },

      test("Имя моба подставляется в строку") {
        val b    = battle()
        val cast = MonsterSkill.QuickStrike.cast(b, hero(100, 0), 0L)
        assertTrue(cast.line.contains(b.toMonster.name))
      }
    )
  )
}
