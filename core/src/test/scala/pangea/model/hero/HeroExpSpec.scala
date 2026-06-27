package pangea.model.hero

import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.user.UserId
import pangea.test.TestFixtures
import zio.test._

object HeroExpSpec extends ZIOSpecDefault {

  override def spec =
    suite("Hero")(
      test(
        "neededExpForLevel: последовательность 100, 200, 300, 500, 800, 1300, 2100, …"
      ) {
        val got      = (1L to 8L).map(Hero.neededExpForLevel).toList
        val expected = List(100L, 200L, 300L, 500L, 800L, 1300L, 2100L, 3400L)
        assertTrue(got == expected)
      },
      test("neededExpForLevel: каждый порог = сумма двух предыдущих") {
        val xs = (1L to 12L).map(Hero.neededExpForLevel)
        assertTrue(
          (3 until xs.size).forall(i => xs(i) == xs(i - 1) + xs(i - 2))
        )
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
          concentration = 0,
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
