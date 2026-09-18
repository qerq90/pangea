package pangea.model.skill

import pangea.model.user.UserId
import pangea.test.TestFixtures
import zio.test._

/** Фиксирует числа из ТЗ по навыкам: распределение по оружию/доспеху, стоимость
  * энергии (формула от уровня), кулдауны и стартовые кд в бою.
  */
object SkillSpec extends ZIOSpecDefault {

  private val hero10 = TestFixtures.hero(UserId(1L)).copy(lvl = 10L)

  override def spec =
    suite("SkillSpec")(
      test(
        "Все 15 навыков распределены между оружием и доспехом без пересечений"
      ) {
        val weapon = Skill.weaponSkills.toSet
        val armor  = Skill.armorSkills.toSet
        assertTrue(weapon.size == 8) &&
        assertTrue(armor.size == 7) &&
        assertTrue((weapon intersect armor).isEmpty) &&
        assertTrue((weapon union armor) == Skill.values.toSet) &&
        // оружие: 1,2,3,6,8,10 + Вихрь клинка, Веерный порез
        assertTrue(
          weapon == Set[Skill](
            Skill.SweepingStrike,
            Skill.QuickStrike,
            Skill.CunningStrike,
            Skill.BloodHarvest,
            Skill.Bleeding,
            Skill.WeakSpotStrike,
            Skill.BladeWhirl,
            Skill.FanCut
          )
        ) &&
        // доспех: 4,5,7,9,11 + Боевой клич, Отбросить
        assertTrue(
          armor == Set[Skill](
            Skill.MinorHeal,
            Skill.Ram,
            Skill.Reinforcement,
            Skill.Restoration,
            Skill.Bulwark,
            Skill.BattleCry,
            Skill.Shove
          )
        )
      },
      test("Стоимость энергии считается по формуле от уровня (герой lvl 10)") {
        assertTrue(
          Skill.SweepingStrike.energyCost(hero10) == 20L
        ) && // 10 + lvl
        assertTrue(
          Skill.QuickStrike.energyCost(hero10) == 10L
        ) && // 5 + 0.5·lvl
        assertTrue(Skill.CunningStrike.energyCost(hero10) == 20L) && // 10 + lvl
        assertTrue(Skill.MinorHeal.energyCost(hero10) == 15L) && // 10 + 0.5·lvl
        assertTrue(Skill.Ram.energyCost(hero10) == 25L) &&       // 15 + lvl
        assertTrue(
          Skill.BloodHarvest.energyCost(hero10) == 13L
        ) && // 8 + 0.5·lvl
        assertTrue(Skill.Reinforcement.energyCost(hero10) == 20L) && // 10 + lvl
        assertTrue(Skill.Bleeding.energyCost(hero10) == 13L) && // 8 + 0.5·lvl
        assertTrue(Skill.Restoration.energyCost(hero10) == 35L) && // 15 + 2·lvl
        assertTrue(
          Skill.WeakSpotStrike.energyCost(hero10) == 20L
        ) &&                                                // 10 + lvl
        assertTrue(Skill.Bulwark.energyCost(hero10) == 22L) && // 12 + lvl
        assertTrue(Skill.BladeWhirl.energyCost(hero10) == 22L) && // 12 + lvl
        assertTrue(Skill.FanCut.energyCost(hero10) == 15L) &&     // 10 + 0.5·lvl
        assertTrue(Skill.BattleCry.energyCost(hero10) == 22L) &&  // 12 + lvl
        assertTrue(Skill.Shove.energyCost(hero10) == 22L)         // 12 + lvl
      },
      test("Кулдауны и стартовые кд в бою соответствуют ТЗ") {
        assertTrue(
          Skill.SweepingStrike.cooldown == 2 && Skill.SweepingStrike.initialCooldown == 0
        ) &&
        assertTrue(
          Skill.QuickStrike.cooldown == 1 && Skill.QuickStrike.initialCooldown == 0
        ) &&
        assertTrue(
          Skill.CunningStrike.cooldown == 2 && Skill.CunningStrike.initialCooldown == 0
        ) &&
        assertTrue(
          Skill.MinorHeal.cooldown == 2 && Skill.MinorHeal.initialCooldown == 0
        ) &&
        assertTrue(Skill.Ram.cooldown == 3 && Skill.Ram.initialCooldown == 0) &&
        assertTrue(
          Skill.BloodHarvest.cooldown == 3 && Skill.BloodHarvest.initialCooldown == 2
        ) &&
        assertTrue(
          Skill.Reinforcement.cooldown == 2 && Skill.Reinforcement.initialCooldown == 1
        ) &&
        assertTrue(
          Skill.Bleeding.cooldown == 2 && Skill.Bleeding.initialCooldown == 1
        ) &&
        assertTrue(
          Skill.Restoration.cooldown == 4 && Skill.Restoration.initialCooldown == 1
        ) &&
        assertTrue(
          Skill.WeakSpotStrike.cooldown == 2 && Skill.WeakSpotStrike.initialCooldown == 1
        ) &&
        assertTrue(
          Skill.Bulwark.cooldown == 3 && Skill.Bulwark.initialCooldown == 0
        ) &&
        assertTrue(Skill.BladeWhirl.cooldown == 3 && Skill.BladeWhirl.initialCooldown == 1) &&
        assertTrue(Skill.FanCut.cooldown == 3 && Skill.FanCut.initialCooldown == 1) &&
        assertTrue(Skill.BattleCry.cooldown == 4 && Skill.BattleCry.initialCooldown == 2) &&
        assertTrue(Skill.Shove.cooldown == 3 && Skill.Shove.initialCooldown == 0)
      },
      test("Формулы урона и лечения — коэффициенты из ТЗ") {
        val b   = hero10.effectiveBaseStats(0L)
        val f   = hero10.effectiveFightStats(0L)
        val hp  = hero10.fightStats.hp
        def v(s: Skill) = s.baseValue(hero10, 0L)
        assertTrue(v(Skill.SweepingStrike) == 1.0 * b.str + 4.0 * b.int + 0.4 * f.atk) &&
        assertTrue(v(Skill.QuickStrike) == 4.0 * b.agi + 0.4 * f.accuracy + 4.0 * b.int + 0.3 * f.atk) &&
        assertTrue(v(Skill.CunningStrike) == 2.0 * b.str + 4.0 * b.int + 0.3 * f.atk + 0.5 * f.accuracy) &&
        assertTrue(v(Skill.Ram) == 0.5 * f.defence + 0.75 * b.vit + 3.0 * b.int) &&
        // Жатва: 2 Силы, 0.6 Атаки и 12% текущего HP — ценой 10% текущего HP
        assertTrue(v(Skill.BloodHarvest) == 2.0 * b.str + 0.6 * f.atk + 0.12 * hp) &&
        assertTrue(Skill.BloodHarvestHpCostPct == 10L && Skill.BloodHarvestHpToDamagePct == 12L) &&
        assertTrue(v(Skill.Bleeding) == 4.0 * b.agi + 0.3 * f.accuracy + 0.3 * f.atk + 5.0 * b.int) &&
        assertTrue(v(Skill.WeakSpotStrike) == 2.0 * b.agi + 0.8 * f.accuracy + 4.0 * b.int + 0.1 * f.atk) &&
        // Размашистый: половина урона — соседям цели
        assertTrue(Skill.SweepingStrike.effect == Skill.Effect.Sweep(50)) &&
        assertTrue(Skill.Ram.describe(hero10).contains("Позволяет сменить место в бою")) &&
        // Умения против толпы
        assertTrue(v(Skill.BladeWhirl) == 1.0 * b.str + 3.0 * b.int + 0.3 * f.atk) &&
        assertTrue(v(Skill.FanCut) == 2.0 * b.agi + 3.0 * b.int + 0.2 * f.accuracy) &&
        assertTrue(Skill.FanCut.effect == Skill.Effect.FanBleed(2)) &&
        assertTrue(v(Skill.BattleCry) == 0.2 * b.int + 0.1 * f.defence) &&
        assertTrue(Skill.BattleCry.effect == Skill.Effect.WarCry(maxPct = 25, turns = 2)) &&
        assertTrue(v(Skill.Shove) == 0.5 * f.defence + 0.5 * b.vit + 2.0 * b.int) &&
        assertTrue(Skill.Shove.effect == Skill.Effect.Knockback)
      },
      test("Описание в инвентаре подставляет стоимость энергии и КД") {
        val d = Skill.Ram.describe(hero10) // Ram: cooldown = 3
        assertTrue(d.contains("расходует 25 энергии")) &&
        assertTrue(!d.contains("{}")) &&
        assertTrue(d.contains("Перезарядка: 3 хода")) &&
        // склонение: 1 → ход, 2–4 → хода
        assertTrue(Skill.QuickStrike.describe(hero10).contains("Перезарядка: 1 ход.")) &&
        assertTrue(Skill.Restoration.describe(hero10).contains("Перезарядка: 4 хода"))
      }
    )
}
