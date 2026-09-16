package pangea.generator.item

import pangea.domain.Rng
import pangea.model.item.{FlaskKind, Item, ItemDetails, ItemType, Rarity}

import scala.annotation.tailrec

/** Случайная фляга для дропа: редкость по [[FlaskKind.rarityWeights]], семья
  * поровну из [[FlaskKind.families]] (стихийная — одна семья, стихия внутри неё
  * тоже поровну), заряды по редкости. Уровень у фляги всегда 1: к этажу она не
  * привязана, статов у неё нет. */
object FlaskGenerator {

  def roll(rng: Rng): (Item, Rng) = {
    val (rarity, r1) = pickWeighted(FlaskKind.rarityWeights, rng)
    val (kind, r2)   = rollKind(r1)
    (item(kind, rarity), r2)
  }

  /** Вид: сперва семья, потом — внутри семьи (для стихийных). */
  def rollKind(rng: Rng): (FlaskKind, Rng) = {
    val (family, r1) = pickHigh(FlaskKind.families, rng)
    if (family.sizeIs == 1) (family.head, r1) else pickHigh(family, r1)
  }

  /** Выбор по СТАРШИМ битам генератора. Два подряд `Rng.pick` по остатку от
    * маленьких чисел (8, потом 4) у линейного конгруэнтного генератора
    * сцеплены младшими битами: стихия выходила бы всегда одна и та же. */
  private def pickHigh[A](list: List[A], rng: Rng): (A, Rng) = {
    val (l, next) = rng.nextLong
    (list(((l >>> 33) % list.length).toInt), next)
  }

  /** Фляга данного вида и редкости, полная. */
  def item(kind: FlaskKind, rarity: Rarity): Item = {
    val cap = FlaskKind.chargesFor(rarity)
    Item(
      id = -1L,
      name = kind.itemName(rarity),
      lvl = 1L,
      rarity = rarity,
      itemType = ItemType.Flask,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Flask(kind.effect, cap, cap)
    )
  }

  /** Пересобранная в кубе фляга: вид заново, заряды по её редкости. */
  def reroll(base: Item, rng: Rng): (Item, Rng) = {
    val (kind, r1) = rollKind(rng)
    (item(kind, base.rarity).copy(id = base.id, lvl = base.lvl), r1)
  }

  private def pickWeighted[A](weights: List[(A, Int)], rng: Rng): (A, Rng) = {
    val total        = weights.map(_._2).sum.max(1)
    val (roll, next) = rng.between(0L, total.toLong)
    @tailrec
    def walk(rem: List[(A, Int)], acc: Long): A =
      rem match {
        case (a, _) :: Nil  => a
        case (a, w) :: tail => if (roll < acc + w) a else walk(tail, acc + w)
        case Nil            => weights.head._1
      }
    (walk(weights, 0L), next)
  }
}
