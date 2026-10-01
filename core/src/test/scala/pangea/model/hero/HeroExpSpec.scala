package pangea.model.hero

import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.user.UserId
import pangea.test.TestFixtures
import zio.test._

object HeroExpSpec extends ZIOSpecDefault {

  override def spec =
    suite("Hero")(
      test("neededExpForLevel: 30·ур·(ур+2) — 90, 240, 450, 720, 1050, …") {
        val got      = (1L to 8L).map(Hero.neededExpForLevel).toList
        val expected = List(90L, 240L, 450L, 720L, 1050L, 1440L, 1890L, 2400L)
        assertTrue(got == expected) &&
        // нулевой уровень не даёт нулевого порога: иначе gainExp крутился бы вхолостую
        assertTrue(Hero.neededExpForLevel(0L) == Hero.neededExpForLevel(1L))
      },
      test("neededExpForLevel: ровный подъём без стены до самого потолка") {
        val xs = (1L to Hero.MaxLevel).map(Hero.neededExpForLevel)
        // квадрат, а не прогрессия: вторая разность постоянна и равна 60
        assertTrue((2 until xs.size).forall(i => xs(i) - 2 * xs(i - 1) + xs(i - 2) == 60L)) &&
        // каждый следующий уровень дороже предыдущего, но не больше чем втрое
        assertTrue((1 until xs.size).forall(i => xs(i) > xs(i - 1) && xs(i) <= 3 * xs(i - 1))) &&
        // весь путь до потолка укладывается в досягаемые числа, а не в фибоначчиевы
        assertTrue(xs.sum < 40000000L)
      },
      test("gainExp прокручивает столько уровней, сколько оплачено, и даёт очки") {
        val hero = TestFixtures.hero(UserId(1L)).copy(lvl = 1L, exp = 0L, upgradePoints = 0L)
        // 90 + 240 + 450 — ровно три уровня, и десятка сверх остаётся в остатке
        val up   = hero.gainExp(90L + 240L + 450L + 10L)
        assertTrue(up.lvl == 4L && up.exp == 10L) &&
        assertTrue(up.upgradePoints == 3L * Hero.PointsPerLevel)
      },
      test("HP нагрудника прибавляется к effectiveMaxHp") {
        val hero = TestFixtures.hero(UserId(1L))
        val chest = Item(
          1L,
          "Нагрудник",
          1L,
          Rarity.Green,
          ItemType.ChestPlate,
          attack = 0,
          accuracy = 0,
          energy = 0,
          armor = 0,
          defence = 0,
          evasion = 0,
          hp = 50
        )
        val withChest =
          hero.copy(equipment = hero.equipment.copy(chestPlate = chest))
        assertTrue(
          withChest.effectiveMaxHp(0L) == hero.effectiveMaxHp(0L) + 50L
        )
      }
    )
}
