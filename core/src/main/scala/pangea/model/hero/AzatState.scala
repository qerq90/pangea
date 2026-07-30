package pangea.model.hero

import enumeratum._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
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
  instantRests:  Int          = 0
) {
  def hasCube: Boolean    = cube == CubeStatus.Active
  def cubeFound: Boolean  = cube == CubeStatus.FoundInactive
  def cubeAbsent: Boolean = cube == CubeStatus.None

  def blessingActive(nowMs: Long): Boolean = blessingUntil.exists(_ > nowMs)

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
  /** Сколько мгновенных отдыхов даёт благословение. */
  val BlessingInstantRests: Int = 250
  /** Бонус благословения (в %): к опыту, репутации, золоту и редкости добычи. */
  val BlessingBonusPct: Long = 10L
  /** Шанс (в %) дополнительной добычи после боя при благословении. */
  val BlessingExtraDropPct: Long = 5L

  implicit val encoder: Encoder[AzatState] = deriveEncoder[AzatState]
  implicit val decoder: Decoder[AzatState] = deriveDecoder[AzatState]
}
