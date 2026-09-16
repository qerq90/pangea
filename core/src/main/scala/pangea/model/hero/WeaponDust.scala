package pangea.model.hero

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.battle.Element
import pangea.model.item.{Gem, MaterialKind}

/** Пыль, которой посыпано оружие. Каждый слой работает как вставленный камень
  * своего вида грейда 1 — с одной оговоркой: стихийная пыль НЕ поднимает
  * эффективность стихийного урона (у пыли нет грейда, ей нечего усиливать).
  *
  * Держится ровно один бой: и слои, и штраф снимаются, когда бой кончился —
  * победой, смертью или бегством (см. `BattleState.commit`).
  *
  * Слоёв не больше трёх. Четвёртая горсть, чем бы её ни сыпали, сбивает всё
  * покрытие и оставляет тот же штраф, что и всполох магии, — оружие идёт в бой
  * с уроном на четверть меньше.
  *
  * Хранится в `heroes.weapon_dust`. */
final case class WeaponDust(
  layers:  List[MaterialKind] = Nil,
  penalty: Boolean            = false,
  // Смазка из отвара: на ближайший бой удары по HP травят или пускают кровь.
  // Сходит вместе с пылью, когда бой кончился.
  coat:    Option[WeaponCoat] = None
) {

  /** Слои как камни грейда 1 — в этом виде их читает [[HeroGems]]. */
  def gems: List[Gem] = layers.flatMap(_.gem).map(Gem(_, Gem.MinGrade))

  /** Стихии, которые пыль дала оружию. */
  def elements: Set[Element] = gems.flatMap(g => Element.of(g.kind)).toSet

  /** Множитель урона героя: всполох магии и перебор с покрытием стоят четверти. */
  def damageMult: Double = if (penalty) 1.0 - WeaponDust.PenaltyPct / 100.0 else 1.0

  def isEmpty: Boolean = layers.isEmpty && !penalty && coat.isEmpty

  def poisonCoated: Boolean = coat.contains(WeaponCoat.Poison)
  def bleedCoated: Boolean  = coat.contains(WeaponCoat.Bleed)
}

/** Чем смазано оружие (см. BrewEffect.Coat). */
sealed trait WeaponCoat extends enumeratum.EnumEntry
object WeaponCoat extends enumeratum.Enum[WeaponCoat] {
  case object Poison extends WeaponCoat
  case object Bleed  extends WeaponCoat
  val values: IndexedSeq[WeaponCoat] = findValues

  implicit val encoder: Encoder[WeaponCoat] = (c: WeaponCoat) => io.circe.Json.fromString(c.entryName)
  implicit val decoder: Decoder[WeaponCoat] = (c: HCursor) => c.as[String].map(WeaponCoat.withName)
}

object WeaponDust {

  val empty: WeaponDust = WeaponDust()

  /** Сколько слоёв держит оружие. */
  val MaxLayers: Int = 3

  /** На сколько процентов падает урон после всполоха или четвёртой горсти. */
  val PenaltyPct: Long = 25L

  /** Чем кончилась попытка посыпать оружие. */
  sealed trait Outcome { def next: WeaponDust }
  object Outcome {
    /** Слой лёг: оружие получило грань этой пыли на ближайший бой. */
    final case class Applied(next: WeaponDust) extends Outcome
    /** Всполох: пыль столкнулась с противоположной стихией. Слой не встал. */
    final case class Clash(next: WeaponDust) extends Outcome
    /** Четвёртая горсть: покрытие сбито целиком. */
    final case class Overload(next: WeaponDust) extends Outcome
  }

  /** Огонь и Холод друг друга не терпят — то же правило, что у камней в гнёздах
    * (см. `SocketingState`). У прочих стихий пары нет. */
  private def opposite(e: Element): Option[Element] = e match {
    case Element.Fire => Some(Element.Cold)
    case Element.Cold => Some(Element.Fire)
    case _            => None
  }

  /** Посыпать оружие горстью пыли.
    *
    * `weaponGems` — камни, уже вставленные в оружие: рубин в гнезде ссорится с
    * сапфировой пылью ровно так же, как рубиновая пыль, насыпанная раньше. */
  def sprinkle(dust: MaterialKind, current: WeaponDust, weaponGems: List[Gem]): Outcome =
    // Перебор сбивает пыль, но не смазку: она въелась в сталь.
    if (current.layers.sizeIs >= MaxLayers) Outcome.Overload(WeaponDust(Nil, penalty = true, coat = current.coat))
    else {
      val incoming = dust.gem.flatMap(Element.of)
      val onWeapon = (weaponGems ++ current.gems).flatMap(g => Element.of(g.kind)).toSet
      val clashes  = incoming.flatMap(opposite).exists(onWeapon.contains)
      if (clashes) Outcome.Clash(current.copy(penalty = true))
      else Outcome.Applied(current.copy(layers = current.layers :+ dust))
    }

  implicit val encoder: Encoder[WeaponDust] = deriveEncoder

  /** Декодер рукописный, каждое поле с запасным значением: производный требует их
    * все разом и уронил бы разбор старых записей при добавлении нового поля. */
  implicit val decoder: Decoder[WeaponDust] = (c: HCursor) =>
    for {
      layers  <- c.getOrElse[List[MaterialKind]]("layers")(Nil)
      penalty <- c.getOrElse[Boolean]("penalty")(false)
      coat    <- c.getOrElse[Option[WeaponCoat]]("coat")(None)
    } yield WeaponDust(layers, penalty, coat)

  implicit val meta: Meta[WeaponDust] = new Meta(pgDecoderGet, pgEncoderPut)
}
