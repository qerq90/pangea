package pangea.service.state

import pangea.engine.{Choice, ChoiceColor, SceneContent, Screen, Target}
import pangea.model.state.StateType

/**
 * «В город» — прямой выход на городскую площадь из всего, что лежит дальше
 * квартала: из лавки Ришелье, из хранилища, из гильдейских залов и таверных
 * комнат выбираться по одному «Назад» долго, а ходят туда за одним делом.
 *
 * Кнопка есть у каждой такой сцены, кроме куба Азата: из куба уходят своим
 * экраном, чтобы не бросить начатый крафт.
 *
 * Сами кварталы и всё, что открыто прямо с городской площади, такой кнопки не
 * получают — им до города и так один шаг.
 */
object CityExit {

  val Action: String = "GoToCity"

  /** Маршрут для [[pangea.engine.Branch]]: куда бы ни зашёл герой, кнопка
    * возвращает его на городскую площадь. */
  val route: (String, Target) = Action -> Target.Goto(StateType.GlobalMap)

  def button(content: SceneContent, row: Option[Int] = None): Choice =
    Choice(Action, content.text("common.toCity"), color = ChoiceColor.Secondary, row = row)

  /** Добавить кнопку к готовому экрану, не трогая его раскладку: она встаёт в
    * тот же ряд, что и последняя кнопка (обычно это «Назад»), поэтому рядов не
    * прибавляется. Экран без рядов кладёт каждую кнопку в свой ряд — там она
    * просто становится последней. */
  def on(screen: Screen, content: SceneContent): Screen =
    screen.copy(choices = screen.choices :+ button(content, screen.choices.lastOption.flatMap(_.row)))
}
