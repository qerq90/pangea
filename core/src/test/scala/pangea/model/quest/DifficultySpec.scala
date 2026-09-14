package pangea.model.quest

import zio.test._

/** Сложность заданий — римские числа своими знаками. */
object DifficultySpec extends ZIOSpecDefault {
  override def spec = suite("Difficulty")(
    test("единицы, пятёрки и вычитание — как у римлян") {
      assertTrue(Difficulty.render(1) == "🦴") &&
      assertTrue(Difficulty.render(3) == "🦴🦴🦴") &&
      assertTrue(Difficulty.render(4) == "🦴💀") &&
      assertTrue(Difficulty.render(5) == "💀") &&
      assertTrue(Difficulty.render(8) == "💀🦴🦴🦴") &&
      assertTrue(Difficulty.render(9) == "🦴⚰️") &&
      assertTrue(Difficulty.render(10) == "⚰️") &&
      assertTrue(Difficulty.render(14) == "⚰️🦴💀") &&
      assertTrue(Difficulty.render(40) == "⚰️🪦") &&
      assertTrue(Difficulty.render(50) == "🪦") &&
      assertTrue(Difficulty.render(99) == "⚰️🌑🦴⚰️") &&
      assertTrue(Difficulty.render(100) == "🌑")
    },
    test("ниже единицы — одна кость") {
      assertTrue(Difficulty.render(0) == "🦴") && assertTrue(Difficulty.render(-5) == "🦴")
    }
  )
}
