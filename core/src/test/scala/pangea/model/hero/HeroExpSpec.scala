package pangea.model.hero

import zio.test._

object HeroExpSpec extends ZIOSpecDefault {

  override def spec = suite("Hero.neededExpForLevel (Фибоначчи)")(

    test("последовательность 100, 200, 300, 500, 800, 1300, 2100, …") {
      val got      = (1L to 8L).map(Hero.neededExpForLevel).toList
      val expected = List(100L, 200L, 300L, 500L, 800L, 1300L, 2100L, 3400L)
      assertTrue(got == expected)
    },

    test("каждый порог = сумма двух предыдущих") {
      val xs = (1L to 12L).map(Hero.neededExpForLevel)
      assertTrue((3 until xs.size).forall(i => xs(i) == xs(i - 1) + xs(i - 2)))
    }
  )
}
