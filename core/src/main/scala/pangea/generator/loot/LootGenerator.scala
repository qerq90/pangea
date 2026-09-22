package pangea.generator.loot

import pangea.domain.Rng
import pangea.generator.item.{FlaskGenerator, GemGenerator, ItemGenerator, ItemNameGenerator, MaterialGenerator, TreasureMapGenerator}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails, ItemType, MaterialKind, TrophyKind}
import pangea.model.monster.{MiniBoss, Race, Rarity => MobRarity}
import pangea.model.rune.{RuneStone, RuneStoneSize}
import pangea.model.item.{Gem => GemModel, Rarity => ItemRarity}

import scala.annotation.tailrec

/** Чистое ядро дропа лута при победе (§20 ARCHITECTURE). Решает «что выпало»
  * детерминированно по seed; персист/показ — забота shell
  * (BattleState/LootState).
  *
  * Алгоритм:
  *   1. По тиру моба катаем несколько слотов дропа (см. `dropChances`). Каждый
  *      слот срабатывает независимо со своим шансом. 2. Для сработавшего слота
  *      взвешенно выбираем категорию (см. `categoryWeights`). Сумма весов < 100
  *      — остаток это «пусто» (слот ничего не даёт). Уже выпавшая категория
  *      исключается из следующих слотов (без повторов, ответ 4). 3. Экипировка:
  *      редкость по тиру моба (`gearRarityWeights`), уровень — с разбросом из
  *      `ItemGenerator` (ответ 5). Трофей: тип по тиру (`trophyWeights`) +
  *      раса/уровень. Серебро: `lvl×4 ±20%` (и «груда», и обычное — одна
  *      формула).
  */
object LootGenerator {

  sealed trait LootDrop {

    /** Предмет этого дропа, если дроп предметный (у серебра предмета нет).
      * Единая точка для экранов добычи: раньше каждый из них перечислял виды
      * дропа своим `collect`, и новый вид молча терялся бы в обоих. */
    def itemOpt: Option[Item] = this match {
      case LootDrop.Gear(i)      => Some(i)
      case LootDrop.Trophy(i)    => Some(i)
      case LootDrop.MapHalf(i)   => Some(i)
      case LootDrop.Gem(i)       => Some(i)
      case LootDrop.Flask(i)     => Some(i)
      case LootDrop.Rune(i)      => Some(i)
      case LootDrop.Silver(_, _) => None
      case LootDrop.Doubloons(_) => None
    }
  }
  object LootDrop {
    final case class Gear(item: Item)                    extends LootDrop
    final case class Trophy(item: Item)                  extends LootDrop
    final case class MapHalf(item: Item)                 extends LootDrop
    final case class Gem(item: Item)                     extends LootDrop
    final case class Flask(item: Item)                   extends LootDrop
    final case class Rune(item: Item)                    extends LootDrop
    final case class Silver(amount: Long, pile: Boolean) extends LootDrop
    final case class Doubloons(amount: Long)             extends LootDrop
  }

  sealed trait Category
  object Category {
    case object Gear       extends Category
    case object Trophy     extends Category
    case object SilverPile extends Category
    case object MapHalf    extends Category
    case object Gem        extends Category
    case object Flask      extends Category
    case object Rune       extends Category
    case object RuneSmall  extends Category
  }

  // Сколько слотов дропа и шанс каждого (в %), по тиру моба.
  private def dropChances(tier: MobRarity): List[Int] =
    tier match {
      case MobRarity.Common    => List(30)
      case MobRarity.Uncommon  => List(60, 30)
      case MobRarity.Rare      => List(100, 60)
      case MobRarity.Mythical  => List(100, 60)
      case MobRarity.Legendary => List(100, 100, 60)
    }

  // Веса категорий (в %); сумма = 100 → «пусто» нет (для первого слота). На
  // последующих слотах уже выпавшая категория исключается, суммарный вес активных
  // падает, и появляется доля «пусто». У мифических и легендарных мобов 1% забран
  // у серебра под половинку карты сокровищ (MapHalf).
  //
  // Камень-усилитель (Gem) забран у уже существующих категорий, а не добавлен
  // сверху, чтобы сумма осталась 100:
  //   Редкие и мифические — 1% у трофея;
  //   Легендарные        — 5%: 2% у трофея и 3% у серебра.
  // Фляга (Flask) — 1% у всех тиров, забранный у экипировки (Gear: 35 → 34).
  // Рунные камни — 5% у всех тиров, забранные у экипировки (34 → 29), и эти
  // пять поделены между большой руной (1%) и малой (4%).
  private def categoryWeights(tier: MobRarity): List[(Category, Int)] =
    tier match {
      case MobRarity.Rare =>
        List(Category.Gear -> 29, Category.Trophy -> 38, Category.SilverPile -> 26, Category.Gem -> 1,
             Category.Flask -> 1, Category.Rune -> 1, Category.RuneSmall -> 4)
      case MobRarity.Mythical =>
        List(Category.Gear -> 29, Category.Trophy -> 38, Category.SilverPile -> 25, Category.MapHalf -> 1,
             Category.Gem -> 1, Category.Flask -> 1, Category.Rune -> 1, Category.RuneSmall -> 4)
      case MobRarity.Legendary =>
        List(Category.Gear -> 29, Category.Trophy -> 37, Category.SilverPile -> 22, Category.MapHalf -> 1,
             Category.Gem -> 5, Category.Flask -> 1, Category.Rune -> 1, Category.RuneSmall -> 4)
      case _ =>
        List(Category.Gear -> 29, Category.Trophy -> 39, Category.SilverPile -> 26, Category.Flask -> 1,
             Category.Rune -> 1, Category.RuneSmall -> 4)
    }

  // Редкость выпавшей экипировки, веса в долях 1/1_000_000 (сумма = 1_000_000).
  // Числа из §20 с исправленными суммами до 100% (Green добирает остаток).
  private def gearRarityWeights(tier: MobRarity): List[(ItemRarity, Long)] =
    tier match {
      case MobRarity.Common =>
        List(
          ItemRarity.Gray   -> 450000L,
          ItemRarity.White  -> 350000L,
          ItemRarity.Green  -> 164995L,
          ItemRarity.Blue   -> 20000L,
          ItemRarity.Purple -> 10000L,
          ItemRarity.Violet -> 5000L,
          ItemRarity.Orange -> 5L
        )
      case MobRarity.Uncommon =>
        List(
          ItemRarity.Gray   -> 300000L,
          ItemRarity.White  -> 450000L,
          ItemRarity.Green  -> 194995L,
          ItemRarity.Blue   -> 30000L,
          ItemRarity.Purple -> 20000L,
          ItemRarity.Violet -> 5000L,
          ItemRarity.Orange -> 5L
        )
      case MobRarity.Rare | MobRarity.Mythical =>
        List(
          ItemRarity.Gray   -> 190000L,
          ItemRarity.White  -> 400000L,
          ItemRarity.Green  -> 310000L,
          ItemRarity.Blue   -> 50000L,
          ItemRarity.Purple -> 30000L,
          ItemRarity.Violet -> 5000L,
          ItemRarity.Orange -> 15000L
        )
      case MobRarity.Legendary =>
        List(
          ItemRarity.Gray   -> 0L,
          ItemRarity.White  -> 185000L,
          ItemRarity.Green  -> 310000L,
          ItemRarity.Blue   -> 250000L,
          ItemRarity.Purple -> 130000L,
          ItemRarity.Violet -> 95000L,
          ItemRarity.Orange -> 30000L
        )
    }

  // Вид трофея и веса (в %, сумма = 100) по тиру моба.
  private def trophyWeights(tier: MobRarity): List[(TrophyKind, Int)] =
    tier match {
      case MobRarity.Common =>
        List(
          TrophyKind.Relic    -> 1,
          TrophyKind.Talisman -> 9,
          TrophyKind.Head     -> 20,
          TrophyKind.Sack     -> 70
        )
      case MobRarity.Uncommon =>
        List(
          TrophyKind.Relic    -> 5,
          TrophyKind.Talisman -> 15,
          TrophyKind.Head     -> 20,
          TrophyKind.Sack     -> 60
        )
      case MobRarity.Rare =>
        List(
          TrophyKind.Relic    -> 15,
          TrophyKind.Talisman -> 20,
          TrophyKind.Head     -> 25,
          TrophyKind.Sack     -> 40
        )
      case MobRarity.Mythical =>
        List(
          TrophyKind.Relic    -> 15,
          TrophyKind.Talisman -> 25,
          TrophyKind.Head     -> 30,
          TrophyKind.Sack     -> 30
        )
      case MobRarity.Legendary =>
        List(
          TrophyKind.Relic    -> 25,
          TrophyKind.Talisman -> 45,
          TrophyKind.Head     -> 20,
          TrophyKind.Sack     -> 10
        )
    }

  def roll(
      tier: MobRarity,
      race: Race,
      killLevel: Long,
      rng: Rng,
      gearChanceBonusPct: Long = 0L,
      rarityBumpPct: Long = 0L
  ): (List[LootDrop], Rng) = {
    // Бонус к шансу экипировки (топаз в снаряжении) добавляется к весу категории Gear.
    val weights = categoryWeights(tier).map {
      case (Category.Gear, w) => Category.Gear -> (w + gearChanceBonusPct.toInt)
      case other              => other
    }
    @tailrec
    def loop(
        slots: List[Int],
        used: Set[Category],
        acc: List[LootDrop],
        r: Rng
    ): (List[LootDrop], Rng) =
      slots match {
        case Nil => (acc.reverse, r)
        case chance :: rest =>
          val (roll, r1) = r.between(0L, 100L)
          if (roll >= chance) loop(rest, used, acc, r1)
          else {
            val (catOpt, r2) = pickCategory(weights, used, r1)
            catOpt match {
              case None => loop(rest, used, acc, r2)
              case Some(cat) =>
                val (drop, r3) = makeDrop(cat, tier, race, killLevel, r2, rarityBumpPct)
                loop(rest, used + cat, drop :: acc, r3)
            }
          }
      }
    loop(dropChances(tier), Set.empty, Nil, rng)
  }

  /** Дроп с минибосса. В отличие от обычного лута он есть ВСЕГДА и не зависит
    * от таблицы категорий: выпадает `0..1 + BossLvL` предметов, каждый — по
    * своей раскладке босса (элементаль, Джо, волк).
    *
    * Уровень вещи берётся от уровня ГЕРОЯ с разбросом ±1: у босса свой BossLvL
    * (1..10), и вещь по нему была бы мусором. `floorLvl` — этаж встречи: по нему
    * считается клык волка. Шкура волка падает не больше одной с одного волка. */
  def rollMiniBoss(
      boss: MiniBoss,
      bossLvl: Long,
      heroLvl: Long,
      rng: Rng,
      floorLvl: Long = 1L
  ): (List[LootDrop], Rng) = {
    val (extra, r0) = rng.between(0L, 2L) // 0 или 1 сверх BossLvL
    val count       = (extra + bossLvl).toInt.max(1)
    (0 until count).foldLeft((List.empty[LootDrop], r0, true)) { case ((acc, r, hide), _) =>
      if (boss == MiniBoss.WhiteWolf) {
        val (drop, r2) = wolfDrop(boss, bossLvl, heroLvl, floorLvl, hide, r)
        (acc :+ drop, r2, hide && !isHide(drop))
      } else {
        val (roll, r1) = r.between(0L, 100L)
        val (drop, r2) =
          if (boss == MiniBoss.RottenJoe) joeDrop(boss, roll, bossLvl, heroLvl, r1)
          else elementalDrop(boss, roll, heroLvl, r1)
        (acc :+ drop, r2, hide)
      }
    } match { case (drops, r, _) => (drops, r) }
  }

  /** Шкура волка ли это. */
  def isHide(drop: LootDrop): Boolean =
    drop.itemOpt.exists(_.material.contains(MaterialKind.WhiteWolfHide))

  /** С Белого волка поровну четыре вещи: клык, фиолетовая и синяя вещи
    * «Охотника» и шкура. Если шкура с этого волка уже выпала, её четверть
    * делится между остальными тремя — бросок идёт по укороченной шкале. */
  private def wolfDrop(
      boss: MiniBoss,
      bossLvl: Long,
      heroLvl: Long,
      floorLvl: Long,
      hideAvailable: Boolean,
      rng: Rng
  ): (LootDrop, Rng) = {
    val (roll, r1) = rng.between(0L, if (hideAvailable) 100L else WolfHideFrom)
    if (roll < WolfFangUntil) (LootDrop.Trophy(wolfFang(bossLvl, floorLvl)), r1)
    else if (roll < WolfPurpleUntil) setGear(boss, heroLvl, ItemRarity.Purple, r1)
    else if (roll < WolfHideFrom) setGear(boss, heroLvl, ItemRarity.Blue, r1)
    else (LootDrop.Gear(MaterialGenerator.item(boss.ingredient)), r1)
  }

  /** Клык Белого волка: трофей расы «Животное», уровень — этаж встречи,
    * коэффициент — 6 за каждый BossLvL (лежит на самом предмете). */
  def wolfFang(bossLvl: Long, floorLvl: Long): Item = Item(
    id = -1L,
    name = TrophyKind.Fang.displayName,
    lvl = floorLvl.max(1L),
    rarity = ItemRarity.Gray,
    itemType = ItemType.Trophy,
    attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
    details = ItemDetails.Trophy(Race.Animal.entryName, TrophyKind.Fang,
      Some(MiniBoss.WhiteWolf.FangCoefPerLvl * bossLvl))
  )

  /** Границы роллов волка (0..99): до 25 — клык, до 50 — фиолетовая вещь, до 75 —
    * синяя, дальше — шкура. */
  val WolfFangUntil: Long   = 25L
  val WolfPurpleUntil: Long = 50L
  val WolfHideFrom: Long    = 75L

  /** С элементаля половину роллов забирает ингредиент, а вторую делят пополам
    * фиолетовая и синяя вещи его набора. */
  private def elementalDrop(boss: MiniBoss, roll: Long, heroLvl: Long, rng: Rng): (LootDrop, Rng) =
    if (roll < ElementalIngredientChancePct) (LootDrop.Gear(MaterialGenerator.item(boss.ingredient)), rng)
    else if (roll < ElementalIngredientChancePct + ElementalPurpleChancePct)
      setGear(boss, heroLvl, ItemRarity.Purple, rng)
    else setGear(boss, heroLvl, ItemRarity.Blue, rng)

  /** С Гнилого Джо падает поровну четыре вещи: кожа упыря, расколотый усилитель,
    * горсть дублонов и вещь «Упыря» — последнюю четверть делят пополам фиолетовая
    * и синяя, как и у элементалей. */
  private def joeDrop(boss: MiniBoss, roll: Long, bossLvl: Long, heroLvl: Long, rng: Rng): (LootDrop, Rng) =
    if (roll < 25L) (LootDrop.Gear(MaterialGenerator.item(boss.ingredient)), rng)
    else if (roll < 50L) {
      // Усилитель — любой камень или череп, но всегда самого низкого качества
      // («расколотый»), тем же роллом, что и обычный дроп камня.
      val (gem, r1) = GemGenerator.randomGem(GemModel.MinGrade, rng)
      (LootDrop.Gem(gem), r1)
    } else if (roll < 75L) {
      // Дублоны: 2 × BossLvL с разбросом ±20%, но не меньше одного.
      val base       = JoeDoubloonsPerLvl * bossLvl
      val (pct, r1)  = rng.between(100L - JoeDoubloonSpreadPct, 100L + JoeDoubloonSpreadPct + 1L)
      (LootDrop.Doubloons((base * pct / 100L).max(1L)), r1)
    } else if (roll < JoePurpleUntil) setGear(boss, heroLvl, ItemRarity.Purple, rng)
    else setGear(boss, heroLvl, ItemRarity.Blue, rng)

  /** Вещь набора этого босса заданной редкости: уровень героя ±1, но строго в
    * границах игры — на первом уровне разброс не уводит вещь в нулевой, на
    * последнем в 151-й. Имя перекатываем как сетовое: имя набора встаёт вместо
    * титула. */
  private def setGear(boss: MiniBoss, heroLvl: Long, rarity: ItemRarity, rng: Rng): (LootDrop, Rng) = {
    val (delta, r2) = rng.between(-1L, 2L)
    val lvl         = (heroLvl + delta).max(1L).min(Hero.MaxLevel)
    val (item, r3)  = ItemGenerator.createItemAtLevel(lvl, rarity, r2)
    val (name, r4)  = ItemNameGenerator.setName(item.itemType, item.rarity, boss.set, r3)
    (LootDrop.Gear(item.copy(name = name, set = Some(boss.set))), r4)
  }

  /** Шанс (в %), что предмет с элементаля окажется ингредиентом, а не вещью набора. */
  val ElementalIngredientChancePct: Long = 50L

  /** Из оставшейся половины столько процентов приходится на фиолетовую вещь; всё,
    * что не выпало ингредиентом и не фиолетовым, — синяя вещь того же набора. */
  val ElementalPurpleChancePct: Long = 25L

  /** Граница внутри последней четверти роллов Джо (75..99): до неё — фиолетовая
    * вещь набора, после — синяя. Нечётный остаток достаётся фиолетовой. */
  val JoePurpleUntil: Long = 88L

  /** Сколько дублонов за уровень босса роняет Джо и с каким разбросом (в %). */
  val JoeDoubloonsPerLvl: Long   = 2L
  val JoeDoubloonSpreadPct: Long = 20L

  /** Доп. дропы от пассивок героя, независимые от основного ролла [[roll]] (каждый
    * со своим шансом): «Таксидермист» — шанс на лишний трофей, «Ювелир» — на
    * отдельную груду серебра по обычной формуле дропа. Шансы в процентах (0 —
    * пассивки нет; понимание руны их поднимает, см. HeroPassives). Чистое ядро:
    * числа, не Hero. */
  def rollPassiveDrops(
      trophyChancePct: Long,
      silverChancePct: Long,
      tier: MobRarity,
      race: Race,
      killLevel: Long,
      rng: Rng
  ): (List[LootDrop], Rng) = {
    val (trophy, r1) =
      if (trophyChancePct > 0L) rollChance(trophyChancePct, rng)(makeDrop(Category.Trophy, tier, race, killLevel, _))
      else (None, rng)
    val (silver, r2) =
      if (silverChancePct > 0L) rollChance(silverChancePct, r1) { r =>
        val (amount, rr) = rollSilver(killLevel, r)
        (LootDrop.Silver(amount, pile = false), rr)
      }
      else (None, r1)
    (List(trophy, silver).flatten, r2)
  }

  /** Благословение Азата: с шансом `chancePct`% — дополнительная экипировка (редкость
    * по тиру моба). RNG тратится всегда. */
  def rollBlessingExtraGear(chancePct: Long, tier: MobRarity, killLevel: Long, rng: Rng): (Option[Item], Rng) = {
    val (roll, r1) = rng.between(0L, 100L)
    if (roll < chancePct) {
      val (rarity, r2) = pickGearRarity(gearRarityWeights(tier), r1)
      val (item, r3)   = ItemGenerator.createItem(killLevel, rarity, r2)
      (Some(item), r3)
    } else (None, r1)
  }


  // С шансом `pct`% выполнить `make` (даёт дроп), иначе None. RNG тратится всегда.
  private def rollChance(pct: Long, rng: Rng)(make: Rng => (LootDrop, Rng)): (Option[LootDrop], Rng) = {
    val (roll, r1) = rng.between(0L, 100L)
    if (roll < pct) { val (drop, r2) = make(r1); (Some(drop), r2) }
    else (None, r1)
  }

  // Взвешенный выбор категории среди ещё не выпавших; остаток до 100 — «пусто» (None).
  private def pickCategory(
      weights: List[(Category, Int)],
      used: Set[Category],
      rng: Rng
  ): (Option[Category], Rng) = {
    val active       = weights.filterNot { case (c, _) => used.contains(c) }
    val (roll, next) = rng.between(0L, 100L)
    @tailrec
    def walk(rem: List[(Category, Int)], acc: Long): Option[Category] =
      rem match {
        case Nil => None
        case (c, w) :: tail =>
          val upper = acc + w
          if (roll < upper) Some(c) else walk(tail, upper)
      }
    (walk(active, 0L), next)
  }

  private def makeDrop(
      cat: Category,
      tier: MobRarity,
      race: Race,
      killLevel: Long,
      rng: Rng,
      rarityBumpPct: Long = 0L
  ): (LootDrop, Rng) =
    cat match {
      case Category.Gear =>
        val (rarity0, r1) = pickGearRarity(gearRarityWeights(tier), rng)
        // Благословение Азата: с шансом rarityBumpPct поднять редкость на тир.
        val (rarity, r1b) = bumpRarity(rarity0, rarityBumpPct, r1)
        val (item, r2)    = ItemGenerator.createItem(killLevel, rarity, r1b)
        (LootDrop.Gear(item), r2)

      case Category.Trophy =>
        val (kind, r1) = pickWeighted(trophyWeights(tier), rng)
        val trophy = Item(
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
        (LootDrop.Trophy(trophy), r1)

      case Category.SilverPile =>
        val (amount, r1) = rollSilver(killLevel, rng)
        (LootDrop.Silver(amount, pile = true), r1)

      case Category.MapHalf =>
        // половинка карты сокровищ — по уровню убитого моба; RNG не тратит
        (LootDrop.MapHalf(TreasureMapGenerator.create(killLevel, half = true)), rng)

      case Category.Flask =>
        // Фляга: редкость и вид по своим таблицам, к этажу не привязана.
        val (flask, r1) = FlaskGenerator.roll(rng)
        (LootDrop.Flask(flask), r1)

      case Category.Gem =>
        // Камень-усилитель 1-го тира («надколотый»). Вид равновероятен среди всех
        // семи, черепа в том числе — в отличие от серебряной жилы, где череп исключён.
        val (gem, r1) = GemGenerator.randomGem(GemModel.MinGrade, rng)
        (LootDrop.Gem(gem), r1)

      case Category.Rune =>
        // Большая руна: вид равновероятен среди всех, боевых и пассивных.
        val (rune, r1) = rng.pick(RuneStone.all)
        (LootDrop.Rune(RuneStone.item(rune, RuneStoneSize.Big)), r1)

      case Category.RuneSmall =>
        val (rune, r1) = rng.pick(RuneStone.all)
        (LootDrop.Rune(RuneStone.item(rune, RuneStoneSize.Small)), r1)
    }

  // Серебро: базис lvl×4 с разбросом ±20%, минимум 1.
  private def rollSilver(killLevel: Long, rng: Rng): (Long, Rng) = {
    val base        = killLevel.max(1L) * 4L
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

  // Лестница редкостей для повышения тира благословением.
  private val rarityLadder: List[ItemRarity] =
    List(ItemRarity.Gray, ItemRarity.White, ItemRarity.Green, ItemRarity.Blue,
      ItemRarity.Purple, ItemRarity.Violet, ItemRarity.Orange)

  // С шансом `pct`% поднимает редкость на один тир (RNG тратится только если pct>0).
  private def bumpRarity(r: ItemRarity, pct: Long, rng: Rng): (ItemRarity, Rng) =
    if (pct <= 0L) (r, rng)
    else {
      val (roll, next) = rng.between(0L, 100L)
      if (roll < pct) {
        val i = rarityLadder.indexOf(r)
        (if (i >= 0 && i < rarityLadder.size - 1) rarityLadder(i + 1) else r, next)
      } else (r, next)
    }

  private def pickGearRarity(
      weights: List[(ItemRarity, Long)],
      rng: Rng
  ): (ItemRarity, Rng) = {
    val total        = weights.map(_._2).sum.max(1L)
    val (roll, next) = rng.between(0L, total)
    @tailrec
    def walk(rem: List[(ItemRarity, Long)], acc: Long): ItemRarity =
      rem match {
        case (a, _) :: Nil  => a
        case (a, w) :: tail => if (roll < acc + w) a else walk(tail, acc + w)
        case Nil            => weights.head._1
      }
    (walk(weights, 0L), next)
  }
}
