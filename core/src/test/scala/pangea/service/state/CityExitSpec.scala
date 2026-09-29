package pangea.service.state

import pangea.engine.{Choice, ChoiceColor, SceneContent, Screen, Target}
import pangea.model.state.StateType
import zio.ZIO
import zio.test._

/** «В город»: кнопка, которую получает каждая городская сцена дальше квартала.
  * Проверяем саму кнопку — что она никому не ломает раскладку и ведёт туда,
  * куда обещает. */
object CityExitSpec extends ZIOSpecDefault {

  private def content = ZIO.attempt(SceneContent.load())

  override def spec = suite("Кнопка «В город»")(

    test("встаёт в ряд последней кнопки: рядов на экране не прибавляется") {
      for {
        c <- content
        screen = Screen("текст", List(
                   Choice("A", "первая", row = Some(0)),
                   Choice("Back", "назад", color = ChoiceColor.Negative, row = Some(1))))
        out    = CityExit.on(screen, c)
      } yield assertTrue(out.choices.map(_.id) == List("A", "Back", CityExit.Action)) &&
              assertTrue(out.choices.last.row.contains(1)) &&
              assertTrue(out.choices.map(_.row).flatten.distinct.size == 2) &&
              assertTrue(out.text == screen.text)
    },

    test("экран без рядов оставляем как есть: кнопка просто последняя") {
      for {
        c <- content
        out = CityExit.on(Screen("текст", List(Choice("Back", "назад"))), c)
      } yield assertTrue(out.choices.map(_.row) == List(None, None)) &&
              assertTrue(out.choices.last.id == CityExit.Action)
    },

    test("подпись берётся из общих текстов, цвет — неброский") {
      for {
        c <- content
      } yield assertTrue(CityExit.button(c).label == c.text("common.toCity")) &&
              assertTrue(CityExit.button(c).label.contains("В город")) &&
              assertTrue(CityExit.button(c).color == ChoiceColor.Secondary)
    },

    test("маршрут ведёт на городскую площадь") {
      assertTrue(CityExit.route._1 == CityExit.Action) &&
      assertTrue(CityExit.route._2 == Target.Goto(StateType.GlobalMap))
    }
  )
}
