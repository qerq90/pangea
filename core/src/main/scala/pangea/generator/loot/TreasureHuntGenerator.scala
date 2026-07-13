package pangea.generator.loot

import pangea.domain.Rng
import pangea.generator.item.ItemGenerator
import pangea.model.item.{Item, MapZone, Rarity => ItemRarity}

import scala.annotation.tailrec

/** Чистое ядро добычи похода за сокровищем по карте клада. Уровень у карты не
  * хранится: диапазон задаёт зона ([[MapZone.levels]], напр. Кинэт 1–25) — каждый
  * предмет и золото катаются на случайный уровень внутри этого диапазона.
  *
  *   - снаряжение: 2 экземпляра гарантированно, 3-й — с шансом 50%, и только
  *     если он выпал, ролится 4-й (тоже 50%) → итог 2–4 предмета; редкость
  *     каждого: Blue 20 · Purple 35 · Violet 31 · Orange 14 (сумма = 100);
  *   - золото выпадает гарантированно (`lvl×12×100 ±20%`);
  *   - дублоны — с шансом 80% (30–70), сверх золота.
  */
object TreasureHuntGenerator {

  final case class Reward(items: List[Item], gold: Long, doubloons: Long)

  // Редкость снаряжения (в %, сумма = 100). Ниже синей не бывает.
  private val gearRarityWeights: List[(ItemRarity, Int)] =
    List(
      ItemRarity.Blue   -> 20,
      ItemRarity.Purple -> 35, // фиолетовая
      ItemRarity.Violet -> 31, // пурпурная
      ItemRarity.Orange -> 14
    )

  def roll(zone: MapZone, rng: Rng): (Reward, Rng) = {
    val (gearCount, r1)   = rollGearCount(rng)    // 2..4
    val (items, r2)       = rollGear(gearCount, zone, Nil, rng = r1)
    val (gold, r3)        = rollGold(zone, r2)                     // гарантированно
    val (doubloonRoll, r4) = r3.between(0L, 100L)
    val (doubloons, r5) =
      if (doubloonRoll < 80) r4.between(30L, 71L)                  // 80% — 30..70
      else                   (0L, r4)                              // 20% — без дублонов
    (Reward(items, gold, doubloons), r5)
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
  private def rollGear(n: Int, zone: MapZone, acc: List[Item], rng: Rng): (List[Item], Rng) =
    if (n <= 0) (acc.reverse, rng)
    else {
      val (rarity, r1) = pickWeighted(gearRarityWeights, rng)
      val (lvl, r2)    = levelIn(zone, r1)
      val (item, r3)   = ItemGenerator.createItemAtLevel(lvl, rarity, r2)
      rollGear(n - 1, zone, item :: acc, r3)
    }

  // Золото: базис lvl×12×100 (уровень — из диапазона зоны) с разбросом ±20%, минимум 1.
  private def rollGold(zone: MapZone, rng: Rng): (Long, Rng) = {
    val (lvl, r1)   = levelIn(zone, rng)
    val base        = lvl.max(1L) * 12L * 100L
    val (pct, next) = r1.between(80L, 121L) // 80..120 %
    ((base * pct / 100L).max(1L), next)
  }

  // Случайный уровень в диапазоне зоны (включительно).
  private def levelIn(zone: MapZone, rng: Rng): (Long, Rng) =
    rng.between(zone.levels.min.toLong, zone.levels.max.toLong + 1L)

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
