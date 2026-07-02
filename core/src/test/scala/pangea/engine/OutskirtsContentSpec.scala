package pangea.engine

import zio.ZIO
import zio.test._

/** Гарантирует, что весь yaml-контент похода за сокровищем на месте и корректно
 *  вложен — иначе `SceneContent` бросит `sys.error` только у игрока в рантайме. */
object OutskirtsContentSpec extends ZIOSpecDefault {

  override def spec = suite("Outskirts/TreasureHunt content")(

    test("экран «За городом» с кнопками отправки и возврата") {
      for {
        c <- ZIO.attempt(SceneContent.load())
        s = c.screen("outskirts.enter")
      } yield assertTrue(s.choices.map(_.id).toSet == Set("DepartTreasure", "BackToCity")) &&
        assertTrue(s.choices.find(_.id == "DepartTreasure").exists(_.color == ChoiceColor.Positive)) &&
        assertTrue(s.choices.find(_.id == "BackToCity").exists(_.color == ChoiceColor.Negative))
    },

    test("экран подтверждения с кнопками Подтвердить/Уйти") {
      for {
        c <- ZIO.attempt(SceneContent.load())
        s = c.screen("outskirts.confirm")
      } yield assertTrue(s.text.contains("10 минут")) &&
        assertTrue(s.choices.map(_.id).toSet == Set("ConfirmDepart", "CancelDepart"))
    },

    test("тексты выбора карты и её отсутствия") {
      for {
        c <- ZIO.attempt(SceneContent.load())
      } yield assertTrue(c.text("outskirts.chooseMap").nonEmpty) &&
        assertTrue(c.text("outskirts.noMaps").nonEmpty) &&
        assertTrue(c.choice("BackToOutskirts", "outskirts.back").color == ChoiceColor.Negative)
    },

    test("тексты похода: ожидание с подстановкой и финал") {
      for {
        c <- ZIO.attempt(SceneContent.load())
      } yield assertTrue(c.format("treasureHunt.enter.text", "duration" -> "10мин 0с").contains("10мин 0с")) &&
        assertTrue(c.format("treasureHunt.wait.text", "remaining" -> "5мин 0с").contains("5мин 0с")) &&
        assertTrue(c.text("treasureHunt.success").contains("выкопали спрятанное сокровище"))
    }
  )
}
