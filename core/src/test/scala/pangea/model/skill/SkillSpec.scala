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
        "Все 11 навыков распределены между оружием и доспехом без пересечений"
      ) {
        val weapon = Skill.weaponSkills.toSet
        val armor  = Skill.armorSkills.toSet
        assertTrue(weapon.size == 6) &&
        assertTrue(armor.size == 5) &&
        assertTrue((weapon intersect armor).isEmpty) &&
        assertTrue((weapon union armor) == Skill.values.toSet) &&
        // оружие: 1,2,3,6,8,10
        assertTrue(
          weapon == Set[Skill](
            Skill.SweepingStrike,
            Skill.QuickStrike,
            Skill.CunningStrike,
            Skill.BloodHarvest,
            Skill.Bleeding,
            Skill.WeakSpotStrike
          )
        ) &&
        // доспех: 4,5,7,9,11
        assertTrue(
          armor == Set[Skill](
            Skill.MinorHeal,
            Skill.Ram,
            Skill.Reinforcement,
            Skill.Restoration,
            Skill.Bulwark
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
        assertTrue(Skill.Bulwark.energyCost(hero10) == 22L) // 12 + lvl
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
        )
      },
      test("Описание в инвентаре подставляет стоимость энергии") {
        val d = Skill.Ram.describe(hero10)
        assertTrue(d.contains("расходует 25 энергии")) && assertTrue(
          !d.contains("{}")
        )
      }
    )
}
