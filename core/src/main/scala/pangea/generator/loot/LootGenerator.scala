package pangea.generator.loot

import pangea.domain.Rng
import pangea.generator.item.ItemGenerator
import pangea.model.item.{Item, ItemType}
import pangea.model.monster.{Race, Rarity => MobRarity}
import pangea.model.item.{Rarity => ItemRarity}

import scala.annotation.tailrec

/**
 * Чистое ядро дропа лута при победе (§20 ARCHITECTURE). Решает «что выпало»
 * детерминированно по seed; персист/показ — забота shell (BattleState/LootState).
 *
 * Алгоритм:
 *  1. По тиру моба катаем несколько слотов дропа (см. `dropChances`). Каждый слот
 *     срабатывает независимо со своим шансом.
 *  2. Для сработавшего слота взвешенно выбираем категорию (см. `categoryWeights`).
 *     Сумма весов < 100 — остаток это «пусто» (слот ничего не даёт). Уже выпавшая
 *     категория исключается из следующих слотов (без повторов, ответ 4).
 *  3. Экипировка: редкость по тиру моба (`gearRarityWeights`), уровень — с разбросом
 *     из `ItemGenerator` (ответ 5). Трофей: тип по тиру (`trophyWeights`) + раса/уровень.
 *     Золото: `lvl×4 ±20%` (и «груда», и обычное — одна формула).
 */
object LootGenerator {

  sealed trait LootDrop
  object LootDrop {
    final case class Gear(item: Item)               extends LootDrop
    final case class Trophy(item: Item)             extends LootDrop
    final case class Gold(amount: Long, pile: Boolean) extends LootDrop
  }

  sealed trait Category
  object Category {
    case object Gear      extends Category
    case object Trophy    extends Category
    case object GoldPile  extends Category
    case object GoldSmall extends Category
  }

  // Сколько слотов дропа и шанс каждого (в %), по тиру моба.
  private def dropChances(tier: MobRarity): List[Int] = tier match {
    case MobRarity.Common    => List(30)
    case MobRarity.Uncommon  => List(60, 30)
    case MobRarity.Rare      => List(100, 60)
    case MobRarity.Mythical  => List(100, 60)
    case MobRarity.Legendary => List(100, 100, 60)
  }

  // Веса категорий (в %); сумма < 100 → остаток = «пусто».
  private def categoryWeights(tier: MobRarity): List[(Category, Int)] = tier match {
    case MobRarity.Common =>
      List(Category.Gear -> 25, Category.Trophy -> 50) // empty 25
    case _ =>
      List(Category.Gear -> 35, Category.Trophy -> 39,
           Category.GoldPile -> 15, Category.GoldSmall -> 10) // empty 1
  }

  // Редкость выпавшей экипировки, веса в долях 1/1_000_000 (сумма = 1_000_000).
  // Числа из §20 с исправленными суммами до 100% (Green добирает остаток).
  private def gearRarityWeights(tier: MobRarity): List[(ItemRarity, Long)] = tier match {
    case MobRarity.Common =>
      List(ItemRarity.Gray -> 450000L, ItemRarity.White -> 350000L, ItemRarity.Green -> 164995L,
           ItemRarity.Blue -> 20000L, ItemRarity.Purple -> 10000L, ItemRarity.Violet -> 5000L,
           ItemRarity.Orange -> 5L)
    case MobRarity.Uncommon =>
      List(ItemRarity.Gray -> 300000L, ItemRarity.White -> 450000L, ItemRarity.Green -> 194995L,
           ItemRarity.Blue -> 30000L, ItemRarity.Purple -> 20000L, ItemRarity.Violet -> 5000L,
           ItemRarity.Orange -> 5L)
    case MobRarity.Rare | MobRarity.Mythical =>
      List(ItemRarity.Gray -> 200000L, ItemRarity.White -> 400000L, ItemRarity.Green -> 310000L,
           ItemRarity.Blue -> 50000L, ItemRarity.Purple -> 30000L, ItemRarity.Violet -> 5000L,
           ItemRarity.Orange -> 5000L)
    case MobRarity.Legendary =>
      List(ItemRarity.Gray -> 0L, ItemRarity.White -> 200000L, ItemRarity.Green -> 310000L,
           ItemRarity.Blue -> 250000L, ItemRarity.Purple -> 130000L, ItemRarity.Violet -> 95000L,
           ItemRarity.Orange -> 15000L)
  }

  // Тип трофея (название) и веса (в %, сумма = 100) по тиру моба.
  private def trophyWeights(tier: MobRarity): List[(String, Int)] = tier match {
    case MobRarity.Common    => List("Реликвия" -> 1,  "Талисман" -> 9,  "Голова" -> 20, "Мешок с пожитками" -> 70)
    case MobRarity.Uncommon  => List("Реликвия" -> 5,  "Талисман" -> 15, "Голова" -> 20, "Мешок с пожитками" -> 60)
    case MobRarity.Rare      => List("Реликвия" -> 15, "Талисман" -> 20, "Голова" -> 25, "Мешок с пожитками" -> 40)
    case MobRarity.Mythical  => List("Реликвия" -> 15, "Талисман" -> 25, "Голова" -> 30, "Мешок с пожитками" -> 30)
    case MobRarity.Legendary => List("Реликвия" -> 25, "Талисман" -> 45, "Голова" -> 20, "Мешок с пожитками" -> 10)
  }

  def roll(tier: MobRarity, race: Race, killLevel: Long, rng: Rng): (List[LootDrop], Rng) = {
    @tailrec
    def loop(slots: List[Int], used: Set[Category], acc: List[LootDrop], r: Rng): (List[LootDrop], Rng) =
      slots match {
        case Nil => (acc.reverse, r)
        case chance :: rest =>
          val (roll, r1) = r.between(0L, 100L)
          if (roll >= chance) loop(rest, used, acc, r1)
          else {
            val (catOpt, r2) = pickCategory(categoryWeights(tier), used, r1)
            catOpt match {
              case None => loop(rest, used, acc, r2)
              case Some(cat) =>
                val (drop, r3) = makeDrop(cat, tier, race, killLevel, r2)
                loop(rest, used + cat, drop :: acc, r3)
            }
          }
      }
    loop(dropChances(tier), Set.empty, Nil, rng)
  }

  // Взвешенный выбор категории среди ещё не выпавших; остаток до 100 — «пусто» (None).
  private def pickCategory(weights: List[(Category, Int)], used: Set[Category], rng: Rng): (Option[Category], Rng) = {
    val active       = weights.filterNot { case (c, _) => used.contains(c) }
    val (roll, next) = rng.between(0L, 100L)
    @tailrec
    def walk(rem: List[(Category, Int)], acc: Long): Option[Category] = rem match {
      case Nil => None
      case (c, w) :: tail =>
        val upper = acc + w
        if (roll < upper) Some(c) else walk(tail, upper)
    }
    (walk(active, 0L), next)
  }

  private def makeDrop(cat: Category, tier: MobRarity, race: Race, killLevel: Long, rng: Rng): (LootDrop, Rng) =
    cat match {
      case Category.Gear =>
        val (rarity, r1) = pickGearRarity(gearRarityWeights(tier), rng)
        val (item, r2)   = ItemGenerator.createItem(killLevel, rarity, r1)
        (LootDrop.Gear(item), r2)

      case Category.Trophy =>
        val (name, r1) = pickWeighted(trophyWeights(tier), rng)
        val trophy = Item(
          id = -1L, name = s"«$name» (${race.toString}) ур-$killLevel",
          lvl = killLevel.max(1L), rarity = ItemRarity.Gray, itemType = ItemType.Trophy,
          attack = 0, accuracy = 0, concentration = 0, armor = 0, defence = 0, evasion = 0,
          race = Some(race.entryName)
        )
        (LootDrop.Trophy(trophy), r1)

      case Category.GoldPile =>
        val (amount, r1) = rollGold(killLevel, rng)
        (LootDrop.Gold(amount, pile = true), r1)

      case Category.GoldSmall =>
        val (amount, r1) = rollGold(killLevel, rng)
        (LootDrop.Gold(amount, pile = false), r1)
    }

  // Золото: базис lvl×4 с разбросом ±20%, минимум 1.
  private def rollGold(killLevel: Long, rng: Rng): (Long, Rng) = {
    val base         = killLevel.max(1L) * 4L
    val (pct, next)  = rng.between(80L, 121L) // 80..120 %
    ((base * pct / 100L).max(1L), next)
  }

  private def pickWeighted[A](weights: List[(A, Int)], rng: Rng): (A, Rng) = {
    val total        = weights.map(_._2).sum.max(1)
    val (roll, next) = rng.between(0L, total.toLong)
    @tailrec
    def walk(rem: List[(A, Int)], acc: Long): A = rem match {
      case (a, _) :: Nil  => a
      case (a, w) :: tail => if (roll < acc + w) a else walk(tail, acc + w)
      case Nil            => weights.head._1
    }
    (walk(weights, 0L), next)
  }

  private def pickGearRarity(weights: List[(ItemRarity, Long)], rng: Rng): (ItemRarity, Rng) = {
    val total        = weights.map(_._2).sum.max(1L)
    val (roll, next) = rng.between(0L, total)
    @tailrec
    def walk(rem: List[(ItemRarity, Long)], acc: Long): ItemRarity = rem match {
      case (a, _) :: Nil  => a
      case (a, w) :: tail => if (roll < acc + w) a else walk(tail, acc + w)
      case Nil            => weights.head._1
    }
    (walk(weights, 0L), next)
  }
}
