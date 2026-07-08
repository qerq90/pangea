package pangea.generator.loot

import pangea.domain.Rng
import pangea.generator.item.{ItemGenerator, TreasureMapGenerator}
import pangea.model.item.{Item, ItemDetails, ItemType, TrophyKind}
import pangea.model.monster.Race
import pangea.model.item.{Rarity => ItemRarity}

import scala.annotation.tailrec

/** Чистое ядро дропа «схрона» — общей добычи для событий-сокровищ (цепочка мобов
  * и прикопанный схрон). Отличается от обычного [[LootGenerator]] другим пулом:
  *
  *   - 2 слота: первый срабатывает на 100%, второй — на 60% (как «гарантированно
  *     что-то + 60% ещё одно, но не той же категории»);
  *   - категории слота: Экипировка 35% · Трофей 35% · Золото 27% · Половинка
  *     карты сокровищ 3% (сумма = 100; половинка забрала свои 3% у золота);
  *   - редкость экипировки: Green 50 · Blue 42 · Purple 5 · Violet 2 · Orange 1;
  *   - трофей: Реликвия 35 · Талисман 65 (всегда ровно один);
  *   - золото: `lvl × 8 ± 20%`, плюс дублоны в диапазоне события.
  *
  * Дублоны выдаются только вместе с золотой категорией. Раса трофея — раса мобов
  * события (для цепочки) или случайная (для прикопанного схрона).
  */
object SchronGenerator {

  final case class Reward(items: List[Item], gold: Long, doubloons: Long)

  private sealed trait Category
  private object Category {
    case object Gear    extends Category
    case object Trophy  extends Category
    case object Gold    extends Category
    case object MapHalf extends Category
  }

  // Слоты схрона и шанс каждого (в %): первый гарантированный, второй — 60%.
  private val slots: List[Int] = List(100, 60)

  // Веса категорий (в %, сумма = 100). На втором слоте уже выпавшая категория
  // исключается — суммарный вес активных падает, появляется доля «пусто».
  // Половинка карты сокровищ (MapHalf) — 3%, забранные у золота (Gold: 30 → 27).
  private val categoryWeights: List[(Category, Int)] =
    List(Category.Gear -> 35, Category.Trophy -> 35, Category.Gold -> 27, Category.MapHalf -> 3)

  // Редкость выпавшей экипировки (в %, сумма = 100). Без серой/белой.
  private val gearRarityWeights: List[(ItemRarity, Int)] =
    List(
      ItemRarity.Green  -> 50,
      ItemRarity.Blue   -> 42,
      ItemRarity.Purple -> 5,
      ItemRarity.Violet -> 2,
      ItemRarity.Orange -> 1
    )

  // Трофеи схрона: Реликвия 35 · Талисман 65 (всегда ровно один).
  private val trophyWeights: List[(TrophyKind, Int)] =
    List(TrophyKind.Relic -> 35, TrophyKind.Talisman -> 65)

  /** Прокатать схрон.
    *
    * @param race        раса трофея
    * @param killLevel   уровень для масштабирования предметов/золота
    * @param doubloonMin минимум дублонов вместе с золотом
    * @param doubloonMax максимум дублонов вместе с золотом
    */
  def roll(
      race: Race,
      killLevel: Long,
      doubloonMin: Int,
      doubloonMax: Int,
      rng: Rng
  ): (Reward, Rng) = {
    @tailrec
    def loop(
        rest: List[Int],
        used: Set[Category],
        items: List[Item],
        gold: Long,
        doubloons: Long,
        r: Rng
    ): (Reward, Rng) =
      rest match {
        case Nil => (Reward(items.reverse, gold, doubloons), r)
        case chance :: tail =>
          val (hit, r1) = r.between(0L, 100L)
          if (hit >= chance) loop(tail, used, items, gold, doubloons, r1)
          else
            pickCategory(used, r1) match {
              case (None, r2) => loop(tail, used, items, gold, doubloons, r2)
              case (Some(cat), r2) =>
                cat match {
                  case Category.Gear =>
                    val (rarity, r3) = pickWeighted(gearRarityWeights, r2)
                    val (item, r4)   = ItemGenerator.createItem(killLevel, rarity, r3)
                    loop(tail, used + cat, item :: items, gold, doubloons, r4)
                  case Category.Trophy =>
                    val (kind, r3) = pickWeighted(trophyWeights, r2)
                    loop(tail, used + cat, trophy(kind, race, killLevel) :: items, gold, doubloons, r3)
                  case Category.Gold =>
                    val (g, r3) = rollGold(killLevel, r2)
                    val (d, r4) = r3.between(doubloonMin.toLong, doubloonMax.toLong + 1L)
                    loop(tail, used + cat, items, gold + g, doubloons + d, r4)
                  case Category.MapHalf =>
                    // половинка карты по уровню схрона; RNG не тратит
                    val half = TreasureMapGenerator.create(killLevel, half = true)
                    loop(tail, used + cat, half :: items, gold, doubloons, r2)
                }
            }
      }
    loop(slots, Set.empty, Nil, 0L, 0L, rng)
  }

  // Взвешенный выбор категории среди ещё не выпавших; остаток до 100 — «пусто».
  private def pickCategory(used: Set[Category], rng: Rng): (Option[Category], Rng) = {
    val active       = categoryWeights.filterNot { case (c, _) => used.contains(c) }
    val (roll, next) = rng.between(0L, 100L)
    @tailrec
    def walk(rem: List[(Category, Int)], acc: Long): Option[Category] =
      rem match {
        case Nil => None
        case (c, w) :: t =>
          val upper = acc + w
          if (roll < upper) Some(c) else walk(t, upper)
      }
    (walk(active, 0L), next)
  }

  // Золото: базис lvl×8 с разбросом ±20%, минимум 1.
  private def rollGold(killLevel: Long, rng: Rng): (Long, Rng) = {
    val base        = killLevel.max(1L) * 8L
    val (pct, next) = rng.between(80L, 121L) // 80..120 %
    ((base * pct / 100L).max(1L), next)
  }

  private def trophy(kind: TrophyKind, race: Race, killLevel: Long): Item =
    Item(
      id = -1L,
      name = s"${kind.displayName} (${race.toString})",
      lvl = killLevel.max(1L),
      rarity = ItemRarity.Gray,
      itemType = ItemType.Trophy,
      attack = 0,
      accuracy = 0,
      energy = 0,
      armor = 0,
      defence = 0,
      evasion = 0,
      details = ItemDetails.Trophy(race.entryName, kind)
    )

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
