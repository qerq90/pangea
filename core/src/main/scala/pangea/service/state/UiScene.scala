package pangea.service.state

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import pangea.dao.hero.HeroDao
import pangea.model.user.UserId
import zio.Task

/**
 * Сцены экранов-«шкафов» (инвентарь, снаряжение, умения, вставка камня) в
 * `heroes.scene_data`. Каждая живёт под СВОИМ ключом и не трогает остальное
 * содержимое колонки.
 *
 * Так и должно быть: в той же колонке лежит состояние события, в котором игрок
 * сейчас стоит, — какой элементаль ждёт в логове, сколько попыток осталось на
 * осмотр. Раньше поход в инвентарь перед минибоссом затирал это состояние
 * целиком, и на выходе игрок оказывался перед новым, заново разыгранным
 * элементалем.
 */
object UiScene {

  /** Сцена из своего ключа; `empty`, если её там нет или она не разобралась. */
  def read[A: Decoder](heroDao: HeroDao, userId: UserId, key: String, empty: A): Task[A] =
    heroDao.readSceneData(userId).map(
      _.flatMap(_.hcursor.get[A](key).toOption).getOrElse(empty))

  /** Кладёт сцену в свой ключ, сохраняя всё, что лежало в колонке рядом. */
  def write[A: Encoder](heroDao: HeroDao, userId: UserId, key: String, scene: A): Task[Unit] =
    for {
      cur <- heroDao.readSceneData(userId)
      base = cur.filter(_.isObject).getOrElse(Json.obj())
      _   <- heroDao.writeSceneData(userId, base.deepMerge(Json.obj(key -> scene.asJson)))
    } yield ()

  /** Убирает только свой ключ — состояние события в колонке остаётся жить. */
  def clear(heroDao: HeroDao, userId: UserId, key: String): Task[Unit] =
    for {
      cur <- heroDao.readSceneData(userId)
      rest = cur.flatMap(_.asObject).map(o => Json.fromJsonObject(o.remove(key)))
      _   <- heroDao.writeSceneData(userId, rest.filter(_.asObject.exists(_.nonEmpty)).getOrElse(Json.Null))
    } yield ()

  val Inventory: String = "inventoryScene"
  val Equipment: String = "equipmentScene"
  val Skills:    String = "skillsScene"
  val Socketing: String = "socketingScene"
}
