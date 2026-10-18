package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

/** Хват оружия: в одну руку или в обе. Одноручное занимает одну руку из двух и
  * несёт лишь часть своих статов; двуручное держат обеими руками, и вторую руку
  * оно занимает целиком (см. `Equipment`). Лук и арбалет — стрелковое
  * двуручное: бьют дальше, но вблизи слабее (см. [[GripRates]]). Хват выпадает
  * при создании оружия (см. `ItemGenerator`). У оружия, выкованного до
  * разделения рук, хвата нет — оно считается одноручным со статами как были. */
sealed abstract class Grip(val twoHanded: Boolean, val ranged: Boolean) extends EnumEntry {

  /** Сколько процентов своих статов несёт оружие с этим хватом. */
  def statPct: Long = if (twoHanded) GripRates.TwoHandedPct else GripRates.OneHandedPct
}

/** Числа хвата — отдельно от компаньона [[Grip]], как [[SetRates]]: вариант enum
  * не должен читать значения своего же компаньона, пока тот собирает `values`. */
object GripRates {

  /** Одноручное несёт 60% статов: было бы 100 атаки — станет 60. */
  val OneHandedPct: Long = 60L

  /** Двуручное — все сто. */
  val TwoHandedPct: Long = 100L

  /** Двуручному — на гнездо больше, чем выпало бы по таблице оружия… */
  val TwoHandedExtraSockets: Int = 1

  /** …но не больше четырёх. */
  val TwoHandedMaxSockets: Int = 4

  /** Стрелковое бьёт на три места в обе стороны: с четвёртого места — по
    * местам с первого по седьмое. Прочее оружие достаёт только соседей. */
  val RangedReach: Int = 3

  /** Вблизи — по тому, кто напротив, и по соседям — стрелковое бьёт на пятую
    * часть слабее. */
  val RangedClosePenaltyPct: Long = 20L
}

object Grip extends Enum[Grip] {

  val values: IndexedSeq[Grip] = findValues

  // Без значений по умолчанию в конструкторе: дефолт параметра живёт в
  // компаньоне, а вариант не должен трогать компаньон, пока тот собирает values.
  case object OneHanded extends Grip(twoHanded = false, ranged = false)
  case object TwoHanded extends Grip(twoHanded = true, ranged = false)

  /** Лук и арбалет: стрелковое двуручное со всеми ста процентами статов. Пока
    * они друг от друга ничем не отличаются. */
  case object Bow      extends Grip(twoHanded = true, ranged = true)
  case object Crossbow extends Grip(twoHanded = true, ranged = true)

  implicit val encoder: Encoder[Grip] = (g: Grip) => g.entryName.asJson
  implicit val decoder: Decoder[Grip] = (c: HCursor) => c.as[String].map(Grip.withName)
}
