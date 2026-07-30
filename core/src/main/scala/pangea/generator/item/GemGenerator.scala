package pangea.generator.item

import pangea.domain.Rng
import pangea.model.item.{Gem, GemKind, Item, ItemDetails, ItemType, Rarity}

/** Чистое ядро генерации камней-усилителей как предметов инвентаря. Камни не
 *  масштабируются по уровню (эффект зависит только от грейда), поэтому уровень у
 *  предмета-камня формальный (1). */
object GemGenerator {

  /** Предмет-камень заданного вида и грейда. */
  def item(kind: GemKind, grade: Int): Item =
    Item(
      id = -1L,
      name = Gem.gradeName(kind, grade),
      lvl = 1L,
      rarity = Rarity.Gray,
      itemType = ItemType.Gem,
      attack = 0,
      accuracy = 0,
      energy = 0,
      armor = 0,
      defence = 0,
      evasion = 0,
      details = ItemDetails.Gem(Gem(kind, grade))
    )

  /** Случайный вид камня (равновероятно из 7). */
  def randomKind(rng: Rng): (GemKind, Rng) = rng.pick(GemKind.values.toList)

  /** Один случайный камень заданного грейда. */
  def randomGem(grade: Int, rng: Rng): (Item, Rng) = {
    val (kind, next) = randomKind(rng)
    (item(kind, grade), next)
  }

  /** `n` случайных камней заданного грейда (виды независимы). */
  def randomGems(n: Int, grade: Int, rng: Rng): (List[Item], Rng) =
    (0 until n).foldLeft((List.empty[Item], rng)) { case ((acc, r), _) =>
      val (g, r1) = randomGem(grade, r)
      (acc :+ g, r1)
    }
}
