package pangea.engine

import io.circe.Json
import zio.test._

/** Клавиатура ВК не принимает подписи длиннее [[Choice.MaxLabelLength]] —
  * экран с такой кнопкой не уходит вовсе. Проверяем весь scenes.yaml: каждую
  * кнопку в `choices` и каждый ключ `*Label`. */
object SceneButtonsSpec extends ZIOSpecDefault {

  private def load: Json = {
    val src = scala.io.Source.fromResource("scenes.yaml")
    try io.circe.yaml.parser.parse(src.mkString).fold(e => sys.error(e.message), identity)
    finally src.close()
  }

  /** Все подписи кнопок с путём до них. */
  private def labels(json: Json, path: String): List[(String, String)] =
    json.asObject.toList.flatMap(_.toList).flatMap {
      case ("choices", choices) =>
        choices.asObject.toList.flatMap(_.toList).flatMap { case (id, c) =>
          c.asString.orElse(c.hcursor.get[String]("label").toOption).map(l => s"$path.choices.$id" -> l).toList
        }
      case (key, value) if key.toLowerCase.endsWith("label") =>
        value.asString.orElse(value.hcursor.get[String]("label").toOption).map(l => s"$path.$key" -> l).toList
      case (key, value) => labels(value, s"$path.$key")
    }

  override def spec = suite("Подписи кнопок в scenes.yaml")(
    test("ни одна не длиннее лимита клавиатуры") {
      val tooLong = labels(load, "").filter(_._2.length > Choice.MaxLabelLength)
      assertTrue(tooLong.isEmpty) ?? tooLong.map { case (p, l) => s"$p (${l.length}): $l" }.mkString("; ")
    },
    test("подписей нашлось много — обход работает") {
      assertTrue(labels(load, "").size > 100)
    }
  )
}
