package pangea.model.hero

import enumeratum._
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.item.Item

/** Статус куба Азата у героя. */
sealed trait CubeStatus extends EnumEntry
object CubeStatus extends Enum[CubeStatus] {
  val values: IndexedSeq[CubeStatus] = findValues

  /** Куба нет — можно купить в храме или найти с первого легендарного моба. */
  case object None extends CubeStatus
  /** Найден, но не активирован — требует ритуала (20 дублонов + 10000 серебра). */
  case object FoundInactive extends CubeStatus
  /** Активен — можно крафтить и пополнять заряды. */
  case object Active extends CubeStatus

  implicit val encoder: Encoder[CubeStatus] = (s: CubeStatus) => s.entryName.asJson
  implicit val decoder: Decoder[CubeStatus] = (c: HCursor) => c.as[String].map(CubeStatus.withName)
}

/** Durable-состояние храма Азата у героя (JSONB-колонка `azat_data`). Хранит
 *  владение кубом, его заряды и содержимое, срок благословения и остаток мгновенных
 *  отдыхов. Записывается целиком; чтение — с дефолтом [[AzatState.empty]]. */
case class AzatState(
  cube:          CubeStatus   = CubeStatus.None,
  cubeCharges:   Int          = 0,
  cubeItems:     List[Item]   = Nil,
  blessingUntil: Option[Long] = scala.None,
  instantRests:  Int          = 0,
  // Когда благословение в последний раз выдало суточную порцию отдыхов. Ставится
  // при покупке, чтобы за прошедшие до неё сутки ничего не начислилось.
  restsGrantedAt: Option[Long] = scala.None
) {
  def hasCube: Boolean    = cube == CubeStatus.Active
  def cubeFound: Boolean  = cube == CubeStatus.FoundInactive
  def cubeAbsent: Boolean = cube == CubeStatus.None

  def blessingActive(nowMs: Long): Boolean = blessingUntil.exists(_ > nowMs)

  /** Доначисляет мгновенные отдыхи за каждые прошедшие сутки благословения.
   *  Начисление ЛЕНИВОЕ: полуночи считаются от последней выдачи до «сейчас» (но
   *  не дальше конца благословения), поэтому отдыхи не теряются, даже если игрок
   *  в полночь не заходил. Полночь — московская.
   *
   *  Возвращает состояние как есть, если благословения нет или новых суток не
   *  набралось — вызывающему достаточно сравнить с исходным. */
  def withDailyRests(nowMs: Long): AzatState = blessingUntil match {
    case Some(until) =>
      val from = restsGrantedAt.getOrElse(nowMs)
      val to   = nowMs.min(until)
      val days = AzatState.midnightsBetween(from, to)
      if (days <= 0L) this
      else copy(
        instantRests   = instantRests + (days * AzatState.BlessingDailyRests).toInt,
        restsGrantedAt = Some(to)
      )
    case scala.None => this
  }

  /** Осталось времени благословения в человекочитаемом виде (или None, если нет). */
  def blessingRemaining(nowMs: Long): Option[String] =
    blessingUntil.filter(_ > nowMs).map { until =>
      val secs  = (until - nowMs) / 1000L
      val days  = secs / 86400
      val hours = (secs % 86400) / 3600
      s"${days}д ${hours}ч"
    }
}

object AzatState {
  val empty: AzatState = AzatState()

  /** Вместимость куба (сколько ингредиентов можно заложить). */
  val CubeCapacity: Int = 9
  /** Максимум зарядов куба. */
  val MaxCharges: Int = 50
  /** Длительность благословения (7 суток) в миллисекундах. */
  val BlessingDurationMs: Long = 7L * 24L * 60L * 60L * 1000L
  /** Сколько мгновенных отдыхов даёт благословение сразу при покупке. */
  val BlessingInstantRests: Int = 250
  /** Сколько мгновенных отдыхов оно доначисляет каждые сутки в 00:00. */
  val BlessingDailyRests: Int = 50
  /** Бонус благословения (в %): к опыту, репутации, серебру и редкости добычи. */
  val BlessingBonusPct: Long = 10L
  /** Шанс (в %) дополнительной добычи после боя при благословении. */
  val BlessingExtraDropPct: Long = 5L

  /** Сколько московских полуночей прошло между двумя моментами. Часы игры идут
   *  по МСК: сутки закрываются в 00:00 московского времени, а не UTC. */
  def midnightsBetween(fromMs: Long, toMs: Long): Long =
    if (toMs <= fromMs) 0L
    else (toMs + MoscowOffsetMs) / DayMs - (fromMs + MoscowOffsetMs) / DayMs

  private val DayMs: Long = 24L * 60L * 60L * 1000L
  /** Москва — UTC+3 круглый год (перевода часов нет с 2014-го). */
  val MoscowOffsetMs: Long = 3L * 60L * 60L * 1000L

  implicit val encoder: Encoder[AzatState] = deriveEncoder[AzatState]

  /** Декодер рукописный, каждое поле — с запасным значением. Производный требует
    * в записи все поля разом, поэтому добавление нового (так было с
    * `restsGrantedAt`) роняет разбор старых записей целиком — и герой теряет
    * купленное: куб, его заряды и содержимое, срок благословения, отдыхи. */
  implicit val decoder: Decoder[AzatState] = (c: HCursor) =>
    for {
      cube           <- c.getOrElse[CubeStatus]("cube")(CubeStatus.None)
      cubeCharges    <- c.getOrElse[Int]("cubeCharges")(0)
      cubeItems      <- c.getOrElse[List[Item]]("cubeItems")(Nil)
      blessingUntil  <- c.getOrElse[Option[Long]]("blessingUntil")(scala.None)
      instantRests   <- c.getOrElse[Int]("instantRests")(0)
      restsGrantedAt <- c.getOrElse[Option[Long]]("restsGrantedAt")(scala.None)
    } yield AzatState(cube, cubeCharges, cubeItems, blessingUntil, instantRests, restsGrantedAt)
}
