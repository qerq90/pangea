package pangea.generator.loot

import pangea.domain.Rng
import pangea.generator.item.ItemGenerator
import pangea.model.item.{Item, Rarity => ItemRarity}

import scala.annotation.tailrec

/** Чистое ядро добычи похода за сокровищем по карте клада. Уровень предметов и
  * золота масштабируется по уровню карты.
  *
  *   - снаряжение: 2 экземпляра гарантированно, 3-й — с шансом 50%, и только
  *     если он выпал, ролится 4-й (тоже 50%) → итог 2–4 предмета; редкость
  *     каждого: Green 40 · Blue 42 · Purple 11 · Violet 5 · Orange 2 (сумма = 100);
  *   - один дополнительный ролл сверху: 50% ничего · 30% золото (`lvl×12 ±20%`) ·
  *     20% дублоны (30–120).
  */
object TreasureHuntGenerator {

  final case class Reward(items: List[Item], gold: Long, doubloons: Long)

  // Редкость снаряжения (в %, сумма = 100). Серой/белой не бывает.
  private val gearRarityWeights: List[(ItemRarity, Int)] =
    List(
      ItemRarity.Green  -> 40,
      ItemRarity.Blue   -> 42,
      ItemRarity.Purple -> 11,
      ItemRarity.Violet -> 5,
      ItemRarity.Orange -> 2
    )

  def roll(mapLevel: Long, rng: Rng): (Reward, Rng) = {
    val (gearCount, r1)   = rollGearCount(rng)    // 2..4
    val (items, r2)       = rollGear(gearCount, mapLevel, Nil, rng = r1)
    val (bonusRoll, r3)   = r2.between(0L, 100L)
    val (gold, doubloons, r4) =
      if (bonusRoll < 50) (0L, 0L, r3)                             // 50% — ничего
      else if (bonusRoll < 80) {                                   // 30% — золото
        val (g, rr) = rollGold(mapLevel, r3)
        (g, 0L, rr)
      } else {                                                     // 20% — дублоны
        val (d, rr) = r3.between(30L, 121L)                        // 30..120
        (0L, d, rr)
      }
    (Reward(items, gold, doubloons), r4)
  }

  // 2 гарантированно; 3-й — 50%; и лишь если он выпал, 4-й — тоже 50%.
  // Итог: 2 (50%), 3 (25%), 4 (25%).
  private def rollGearCount(rng: Rng): (Int, Rng) = {
    val (third, r1) = rng.between(0L, 100L)
    if (third >= 50) (2, r1)
    else {
      val (fourth, r2) = r1.between(0L, 100L)
      if (fourth >= 50) (3, r2) else (4, r2)
    }
  }

  @tailrec
  private def rollGear(n: Int, mapLevel: Long, acc: List[Item], rng: Rng): (List[Item], Rng) =
    if (n <= 0) (acc.reverse, rng)
    else {
      val (rarity, r1) = pickWeighted(gearRarityWeights, rng)
      val (item, r2)   = ItemGenerator.createItem(mapLevel, rarity, r1)
      rollGear(n - 1, mapLevel, item :: acc, r2)
    }

  // Золото: базис lvl×12 с разбросом ±20%, минимум 1.
  private def rollGold(mapLevel: Long, rng: Rng): (Long, Rng) = {
    val base        = mapLevel.max(1L) * 12L
    val (pct, next) = rng.between(80L, 121L) // 80..120 %
    ((base * pct / 100L).max(1L), next)
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
