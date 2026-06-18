package pangea.engine

import io.circe.Json
import zio.{ZIO, ZLayer}

trait SceneContent {
  def screen(key: String): Screen
  def text(key: String): String
  def format(key: String, args: (String, String)*): String
  def beats(key: String): List[(String, Beat)]
}

object SceneContent {

  private class Live(root: Json) extends SceneContent {

    private def at(key: String): Json =
      key.split("\\.").foldLeft(root)((j, k) => j.hcursor.downField(k).focus.getOrElse(Json.Null))

    def text(key: String): String =
      at(key).asString.getOrElse(sys.error(s"SceneContent: missing string at '$key'"))

    def format(key: String, args: (String, String)*): String =
      args.foldLeft(text(key)) { case (s, (k, v)) => s.replace(s"{$k}", v) }

    def screen(key: String): Screen = {
      val node = at(key)
      val t    = node.hcursor.get[String]("text").getOrElse(sys.error(s"SceneContent: missing .text at '$key'"))
      Screen(t, choicesOf(node))
    }

    def beats(key: String): List[(String, Beat)] =
      at(key).asObject.toList.flatMap(_.toList).map { case (id, node) =>
        val t = node.hcursor.get[String]("text").getOrElse(sys.error(s"SceneContent: missing .text at '$key.$id'"))
        id -> Beat.simple(t, choicesOf(node))
      }

    private def choicesOf(node: Json): List[Choice] =
      node.hcursor.downField("choices").focus
        .flatMap(_.asObject)
        .toList
        .flatMap(_.toList)
        .map { case (id, lbl) => Choice(id, lbl.asString.getOrElse("")) }
  }

  def load(): SceneContent = {
    val src = scala.io.Source.fromResource("scenes.yaml")
    try {
      io.circe.yaml.parser.parse(src.mkString)
        .fold(e => sys.error(s"scenes.yaml: ${e.message}"), json => new Live(json))
    } finally src.close()
  }

  val live: ZLayer[Any, Throwable, SceneContent] =
    ZLayer.fromZIO(ZIO.attempt(load()))
}
