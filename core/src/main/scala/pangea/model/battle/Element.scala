package pangea.model.battle

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.item.GemKind

/** Боевая стихия оружия (даётся камнем в оружии — см. [[GemKind.element]]). Все
 *  числовые ставки стихии живут ЗДЕСЬ, на варианте; бой ([[pangea.service.state.states.battle.BattleState]])
 *  только читает их. Проки (30%) роллятся при нанесении урона этим оружием —
 *  обычной атакой или уроном навыка.
 *
 *  Модификаторы урона:
 *   - `armorMult` — множитель урона ПО БРОНЕ цели;
 *   - `hpMult` — множитель урона ПО HP цели;
 *  оба комбинируются мультипликативно при нескольких стихиях. */
sealed abstract class Element(
  val emoji:      String,
  val armorMult:  Double,
  val hpMult:     Double,
  val procText:   String
) extends EnumEntry

object Element extends Enum[Element] {

  val values: IndexedSeq[Element] = findValues

  /** Шанс прока стихии при нанесении урона оружием (в %). Общий для всех стихий. */
  val ProcChancePct: Long = 30L

  // ── Холод ────────────────────────────────────────────────────────────────────
  case object Cold extends Element(
    emoji     = "❄️",
    armorMult = 1.10, // +10% урона по броне
    hpMult    = 1.0,
    procText  = "❄️ Холод сковал защиту противника! -10% Защиты."
  ) {
    /** Снижение итогового %-снижения урона цели за прок (в п.п.), стакается. */
    val DefenceReductionCut: Int = 10
  }

  // ── Огонь ────────────────────────────────────────────────────────────────────
  case object Fire extends Element(
    emoji     = "🔥",
    armorMult = 0.80, // -20% урона по броне
    hpMult    = 1.10, // +10% урона по HP
    procText  = "🔥 Враг объят ярким пламенем."
  )

  // ── Молния ────────────────────────────────────────────────────────────────────
  case object Lightning extends Element(
    emoji     = "⚡",
    armorMult = 0.80, // -20% урона по броне
    hpMult    = 0.90, // -10% урона по HP
    procText  = "⚡ Молния, прошедшая по телу, выжгла энергию врага."
  ) {
    /** Доля урона по броне, которая дополнительно наносится по HP (всегда, не прок). */
    val ArmorToHpFrac: Double = 0.20
  }

  // ── Воздух ────────────────────────────────────────────────────────────────────
  case object Air extends Element(
    emoji     = "💨",
    armorMult = 0.90, // -10% урона по всему
    hpMult    = 0.90,
    procText  = "💨 Шквальный ветер маскирует движения и помогает вам точнее атаковать."
  ) {
    /** Постоянный бонус к итоговым уклонению/точности владельца (в %), пока в оружии есть воздух. */
    val AlwaysBonusPct: Long = 5L
    /** Дополнительный бонус к уклонению/точности за прок (в %) на [[ProcTurns]] ходов. */
    val ProcBonusPct: Long = 10L
    val ProcTurns: Int     = 3
  }

  /** Стихия камня (None у нестихийных камней — череп/аметист/изумруд). */
  def of(kind: GemKind): Option[Element] = kind match {
    case GemKind.Sapphire => Some(Cold)
    case GemKind.Ruby     => Some(Fire)
    case GemKind.Topaz    => Some(Lightning)
    case GemKind.Diamond  => Some(Air)
    case _                => None
  }

  implicit val encoder: Encoder[Element] = (e: Element) => e.entryName.asJson
  implicit val decoder: Decoder[Element] = (c: HCursor) => c.as[String].map(Element.withName)
}
