package pangea.model.rune

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.model.item.{PassiveKind, Rarity}
import pangea.model.skill.Skill

/** Руны героя (durable, `heroes.rune_data`):
  *
  *  - `active` / `passive` — клейма на теле: ключи рун ([[Rune.key]]), не больше
  *    [[Rune.ActiveSlots]] и [[Rune.PassiveSlots]]. Клеймо даёт силу руны без
  *    вещи; переклеймование снимает узор, но понимание остаётся;
  *  - `understanding` — понимание по рунам (ключ → очки), общее для клейма и
  *    надетой вещи; растёт от сожжённых у Казимира вещей, не выше
  *    [[Rune.cap]] по уровню героя; при сведении не теряется;
  *  - `burnRarities` — какие редкости уходят по «Сдать всё» (как настройка
  *    продажи хлама; ключи — `Rarity.entryName`). */
final case class RuneData(
  active:        List[String]      = Nil,
  passive:       List[String]      = Nil,
  understanding: Map[String, Long] = Map.empty,
  burnRarities:  Set[String]       = RuneData.DefaultBurnRarities
) {
  def brandedActives: List[Skill]        = active.flatMap(Skill.withNameOption)
  def brandedPassives: List[PassiveKind] = passive.flatMap(PassiveKind.withNameOption)

  /** Все клейма, боевые первыми. */
  def branded: List[Rune] = brandedActives.map(Rune.Active(_)) ++ brandedPassives.map(Rune.Passive(_))

  def isBranded(rune: Rune): Boolean = rune match {
    case Rune.Active(s)  => active.contains(s.entryName)
    case Rune.Passive(k) => passive.contains(k.entryName)
  }

  /** Есть ли место под клеймо такого вида. */
  def hasRoomFor(rune: Rune): Boolean =
    if (rune.isActive) active.size < Rune.ActiveSlots else passive.size < Rune.PassiveSlots

  def brandedCount: Int = active.size + passive.size

  /** Сколько стоит следующее клеймо. */
  def nextCost: Long = Rune.CostBase * (1L + brandedCount.toLong)

  def brand(rune: Rune): RuneData = rune match {
    case Rune.Active(s)  => if (active.contains(s.entryName)) this else copy(active = active :+ s.entryName)
    case Rune.Passive(k) => if (passive.contains(k.entryName)) this else copy(passive = passive :+ k.entryName)
  }

  def unbrand(rune: Rune): RuneData = rune match {
    case Rune.Active(s)  => copy(active = active.filterNot(_ == s.entryName))
    case Rune.Passive(k) => copy(passive = passive.filterNot(_ == k.entryName))
  }

  def understandingOf(rune: Rune): Long = understanding.getOrElse(rune.key, 0L)

  def strengthPct(rune: Rune): Double = Rune.strengthPct(understandingOf(rune))

  /** Множитель к числу руны — и на клейме, и на надетой вещи. */
  def mult(rune: Rune): Double = Rune.mult(understandingOf(rune))

  /** Углубить понимание на `points`, не выше `cap`. Возвращает данные и
    * сколько реально прибавилось. */
  def deepen(rune: Rune, points: Long, cap: Long): (RuneData, Long) = {
    val cur    = understandingOf(rune)
    val gained = (cap - cur).max(0L).min(points.max(0L))
    if (gained == 0L) (this, 0L)
    else (copy(understanding = understanding.updated(rune.key, cur + gained)), gained)
  }

  def burns(rarity: Rarity): Boolean = burnRarities.contains(rarity.entryName)

  def toggleBurn(rarities: List[Rarity]): RuneData = {
    val keys = rarities.map(_.entryName).toSet
    if (keys.forall(burnRarities.contains)) copy(burnRarities = burnRarities -- keys)
    else copy(burnRarities = burnRarities ++ keys)
  }
}

object RuneData {
  /** По умолчанию в огонь идут только серые и белые — как у продажи хлама.
    * Объявлено раньше `empty`: значение по умолчанию конструктора читает его
    * при инициализации объекта, и обратный порядок дал бы null. */
  val DefaultBurnRarities: Set[String] = Set(Rarity.Gray, Rarity.White).map(_.entryName)

  val empty: RuneData = RuneData()

  implicit val encoder: Encoder[RuneData] = (r: RuneData) => Json.obj(
    "active"        -> r.active.asJson,
    "passive"       -> r.passive.asJson,
    "understanding" -> r.understanding.asJson,
    "burnRarities"  -> r.burnRarities.toList.sorted.asJson
  )

  // Декодер рукописный, каждое поле с запасным значением: новое поле не должно
  // обнулять уже выжженные клейма (см. память о производных декодерах).
  implicit val decoder: Decoder[RuneData] = (c: HCursor) =>
    for {
      active  <- c.getOrElse[List[String]]("active")(Nil)
      passive <- c.getOrElse[List[String]]("passive")(Nil)
      und     <- c.getOrElse[Map[String, Long]]("understanding")(Map.empty)
      burn    <- c.getOrElse[Option[List[String]]]("burnRarities")(None)
    } yield RuneData(active, passive, und, burn.map(_.toSet).getOrElse(DefaultBurnRarities))

  implicit val meta: Meta[RuneData] = new Meta(pgDecoderGet, pgEncoderPut)
}
