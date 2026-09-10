package pangea.generator

import pangea.domain.Rng
import pangea.generator.loot.LootGenerator
import pangea.generator.loot.LootGenerator.LootDrop
import pangea.model.item.{Gem => GemModel, GemKind, Item, ItemDetails, ItemType}
import pangea.model.monster.{Race, Rarity}
import zio.test._

object LootGeneratorSpec extends ZIOSpecDefault {

  private def raceOf(i: Item): Option[String] = i.details match {
    case ItemDetails.Trophy(r, _) => Some(r)
    case _                        => None
  }

  private def category(d: LootDrop): String = d match {
    case LootDrop.Gear(_)         => "gear"
    case LootDrop.Trophy(_)       => "trophy"
    case LootDrop.MapHalf(_)      => "mapHalf"
    case LootDrop.Gem(_)          => "gem"
    case LootDrop.Silver(_, true) => "silverPile"
    case LootDrop.Silver(_, _)    => "silverSmall"
    case LootDrop.Doubloons(_)    => "doubloons"
  }

  // Доля боёв (в %), в которых выпала категория `cat`, по выборке сидов.
  private def catRatePct(tier: Rarity, cat: String, samples: Long = 20000L): Double = {
    val hits = (1L to samples).count { s =>
      LootGenerator.roll(tier, Race.Orc, 30L, Rng(s))._1.map(category).contains(cat)
    }
    hits * 100.0 / samples
  }

  def spec = suite("LootGeneratorSpec")(

    test("детерминизм: один seed → одинаковый дроп") {
      val (a, _) = LootGenerator.roll(Rarity.Legendary, Race.Orc, 20L, Rng(123L))
      val (b, _) = LootGenerator.roll(Rarity.Legendary, Race.Orc, 20L, Rng(123L))
      assertTrue(a == b)
    },

    test("нет повторных категорий за один бой (ответ 4)") {
      val cases = (1L to 200L).map { s =>
        val (drops, _) = LootGenerator.roll(Rarity.Legendary, Race.Human, 30L, Rng(s))
        val cats       = drops.map(category)
        cats.distinct.size == cats.size
      }
      assertTrue(cases.forall(identity))
    },

    test("обычный моб: не больше одного дропа за бой") {
      val cases = (1L to 200L).map { s =>
        LootGenerator.roll(Rarity.Common, Race.Goblin, 5L, Rng(s))._1.size <= 1
      }
      assertTrue(cases.forall(identity))
    },

    test("легендарный моб роняет лут хотя бы иногда (первые два слота 100%)") {
      val any = (1L to 50L).exists(s => LootGenerator.roll(Rarity.Legendary, Race.Demon, 40L, Rng(s))._1.nonEmpty)
      assertTrue(any)
    },

    test("трофей хранит расу и уровень убийства") {
      val trophy = (1L to 300L).iterator
        .flatMap(s => LootGenerator.roll(Rarity.Rare, Race.Khajiit, 12L, Rng(s))._1)
        .collectFirst { case LootDrop.Trophy(i) => i }
      assertTrue(trophy.exists(i =>
        i.itemType == ItemType.Trophy &&
        raceOf(i).contains(Race.Khajiit.entryName) &&
        i.lvl == 12L &&
        i.name.contains("Каджит")))
    },

    test("серебро всегда положительное и около lvl×4 ±20%") {
      val silvers = (1L to 300L).iterator
        .flatMap(s => LootGenerator.roll(Rarity.Mythical, Race.Gnome, 10L, Rng(s))._1)
        .collect { case LootDrop.Silver(a, _) => a }
        .toList
      // база = 40, разброс 80..120% → [32, 48]
      assertTrue(silvers.nonEmpty) &&
      assertTrue(silvers.forall(g => g >= 32L && g <= 48L))
    },

    test("экипировка генерируется с разбросом уровня из ItemGenerator (killLevel-6 .. killLevel+1)") {
      val gears = (1L to 300L).iterator
        .flatMap(s => LootGenerator.roll(Rarity.Rare, Race.Elf, 50L, Rng(s))._1)
        .collect { case LootDrop.Gear(i) => i }
        .toList
      assertTrue(gears.nonEmpty) &&
      assertTrue(gears.forall(i => i.lvl >= 1L && i.lvl <= 51L)) &&
      assertTrue(gears.forall(_.itemType != ItemType.Trophy))
    },

    test("половинка карты падает у мифических и легендарных мобов") {
      def half(tier: Rarity) = (1L to 5000L).iterator
        .flatMap(s => LootGenerator.roll(tier, Race.Orc, 40L, Rng(s))._1)
        .collectFirst { case LootDrop.MapHalf(i) => i }
      assertTrue(half(Rarity.Legendary).exists(_.itemType == ItemType.TreasureMapHalf)) &&
        assertTrue(half(Rarity.Mythical).exists(_.itemType == ItemType.TreasureMapHalf))
    },

    test("у обычных/необычных/редких мобов половинка карты не падает") {
      val lowerTiers = List(Rarity.Common, Rarity.Uncommon, Rarity.Rare)
      val anyHalf = lowerTiers.exists { tier =>
        (1L to 3000L).iterator
          .flatMap(s => LootGenerator.roll(tier, Race.Orc, 40L, Rng(s))._1)
          .exists { case LootDrop.MapHalf(_) => true; case _ => false }
      }
      assertTrue(!anyHalf)
    },

    // ── Камни-усилители ───────────────────────────────────────────────────────
    test("камень падает у редких, мифических и легендарных; у обычных и необычных — нет") {
      def hasGem(tier: Rarity) = (1L to 5000L).iterator
        .flatMap(s => LootGenerator.roll(tier, Race.Orc, 30L, Rng(s))._1)
        .exists { case LootDrop.Gem(_) => true; case _ => false }
      assertTrue(hasGem(Rarity.Rare)) &&
      assertTrue(hasGem(Rarity.Mythical)) &&
      assertTrue(hasGem(Rarity.Legendary)) &&
      assertTrue(!hasGem(Rarity.Common)) &&
      assertTrue(!hasGem(Rarity.Uncommon))
    },

    test("выпавший камень — 1-го тира, вид случаен и черепа тоже падают") {
      val gems = (1L to 6000L).iterator
        .flatMap(s => LootGenerator.roll(Rarity.Legendary, Race.Orc, 30L, Rng(s))._1)
        .collect { case LootDrop.Gem(i) => i }
        .toList
      val kinds = gems.flatMap(_.gem).map(_.kind).toSet
      assertTrue(gems.nonEmpty) &&
      assertTrue(gems.forall(_.itemType == ItemType.Gem)) &&
      assertTrue(gems.flatMap(_.gem).forall(_.grade == GemModel.MinGrade)) &&
      assertTrue(kinds.contains(GemKind.Skull)) &&
      assertTrue(kinds.size == GemKind.values.size) // встречаются все семь видов
    },

    test("легендарный роняет камень заметно чаще редкого и мифического (вес 5% против 1%)") {
      val rare = catRatePct(Rarity.Rare, "gem")
      val myth = catRatePct(Rarity.Mythical, "gem")
      val leg  = catRatePct(Rarity.Legendary, "gem")
      assertTrue(math.abs(rare - myth) < 0.5) && // у обоих вес 1%
      assertTrue(leg > rare * 4)                 // у легендарного впятеро больший вес
    },

    // Веса категорий должны в сумме давать ровно 100, иначе в первом слоте
    // появляется дыра «пусто». У редких и выше первый слот срабатывает всегда,
    // поэтому пустая добыча тут же выдала бы ошибку в арифметике весов.
    test("у редких и выше добыча никогда не пуста (сумма весов категорий = 100)") {
      val tiers = List(Rarity.Rare, Rarity.Mythical, Rarity.Legendary)
      assertTrue(tiers.forall { tier =>
        (1L to 3000L).forall(s => LootGenerator.roll(tier, Race.Orc, 30L, Rng(s))._1.nonEmpty)
      })
    },

    // ── Дроп с элементаля ─────────────────────────────────────────────────────
    test("дроп с элементаля есть всегда: 0..1 + BossLvL предметов") {
      val counts = (1L to 300L).map { s =>
        LootGenerator.rollMiniBoss(pangea.model.monster.MiniBoss.FireElemental, bossLvl = 3L, heroLvl = 40L, Rng(s))._1.size
      }
      assertTrue(counts.forall(n => n == 3 || n == 4)) && // BossLvL 3 плюс 0 или 1
      assertTrue(counts.contains(3)) && assertTrue(counts.contains(4))
    },

    test("с элементаля падают его ингредиент и вещи набора — фиолетовые и синие") {
      val items = (1L to 300L).iterator
        .flatMap(s => LootGenerator.rollMiniBoss(pangea.model.monster.MiniBoss.FireElemental, 2L, 40L, Rng(s))._1)
        .flatMap(_.itemOpt).toList
      val (materials, gear) = items.partition(_.itemType == ItemType.Material)
      assertTrue(materials.nonEmpty) && assertTrue(gear.nonEmpty) &&
      assertTrue(materials.forall(_.material.contains(pangea.model.item.MaterialKind.EverburningIron))) &&
      // половина роллов — ингредиент, оставшуюся делят пополам фиолет и синь
      assertTrue(gear.forall(g => g.rarity == pangea.model.item.Rarity.Purple ||
                                  g.rarity == pangea.model.item.Rarity.Blue)) &&
      assertTrue(gear.exists(_.rarity == pangea.model.item.Rarity.Purple)) &&
      assertTrue(gear.exists(_.rarity == pangea.model.item.Rarity.Blue)) &&
      assertTrue(gear.forall(_.set.contains(pangea.model.item.ItemSet.WildFlame))) &&
      // имя сетовое: набор встал вместо титула
      assertTrue(gear.forall(_.name.endsWith(pangea.model.item.ItemSet.WildFlame.title)))
    },

    test("фиолетовых и синих вещей с элементаля примерно поровну — по 25% роллов") {
      val gear = (1L to 600L).iterator
        .flatMap(s => LootGenerator.rollMiniBoss(pangea.model.monster.MiniBoss.FireElemental, 2L, 40L, Rng(s))._1)
        .flatMap(_.itemOpt).filter(_.itemType != ItemType.Material).toList
      val purple = gear.count(_.rarity == pangea.model.item.Rarity.Purple).toDouble
      val blue   = gear.count(_.rarity == pangea.model.item.Rarity.Blue).toDouble
      assertTrue(LootGenerator.ElementalPurpleChancePct == 25L) &&
      // доли равные, отклонение на выборке в сотни роллов невелико
      assertTrue(math.abs(purple - blue) / (purple + blue) < 0.15)
    },

    test("у Гнилого Джо вещь набора тоже бывает и фиолетовой, и синей") {
      val gear = (1L to 600L).iterator
        .flatMap(s => LootGenerator.rollMiniBoss(pangea.model.monster.MiniBoss.RottenJoe, 2L, 40L, Rng(s))._1)
        .flatMap(_.itemOpt)
        .filter(i => i.set.contains(pangea.model.item.ItemSet.Ghoul)).toList
      val purple = gear.count(_.rarity == pangea.model.item.Rarity.Purple).toDouble
      val blue   = gear.count(_.rarity == pangea.model.item.Rarity.Blue).toDouble
      assertTrue(gear.nonEmpty) &&
      assertTrue(gear.forall(g => g.rarity == pangea.model.item.Rarity.Purple ||
                                  g.rarity == pangea.model.item.Rarity.Blue)) &&
      assertTrue(purple > 0.0) && assertTrue(blue > 0.0) &&
      // последняя четверть роллов делится пополам
      assertTrue(math.abs(purple - blue) / (purple + blue) < 0.2)
    },

    test("уровень сетовой вещи — уровень героя ±1, а не уровень босса") {
      val gear = (1L to 300L).iterator
        .flatMap(s => LootGenerator.rollMiniBoss(pangea.model.monster.MiniBoss.FireElemental, 2L, 40L, Rng(s))._1)
        .flatMap(_.itemOpt).filter(_.itemType != ItemType.Material).toList
      assertTrue(gear.nonEmpty) && assertTrue(gear.forall(i => i.lvl >= 39L && i.lvl <= 41L))
    },

    test("с каменного падают его магические камни и вещи «Каменного стража»") {
      val items = (1L to 300L).iterator
        .flatMap(s => LootGenerator.rollMiniBoss(pangea.model.monster.MiniBoss.StoneElemental, 2L, 40L, Rng(s))._1)
        .flatMap(_.itemOpt).toList
      val (materials, gear) = items.partition(_.itemType == ItemType.Material)
      assertTrue(materials.nonEmpty) && assertTrue(gear.nonEmpty) &&
      assertTrue(materials.forall(_.material.contains(pangea.model.item.MaterialKind.MagicStone))) &&
      assertTrue(gear.forall(g => g.rarity == pangea.model.item.Rarity.Purple ||
                                  g.rarity == pangea.model.item.Rarity.Blue)) &&
      assertTrue(gear.forall(_.set.contains(pangea.model.item.ItemSet.StoneGuard))) &&
      assertTrue(gear.forall(_.name.endsWith(pangea.model.item.ItemSet.StoneGuard.title)))
    },

    test("уровень вещи не выходит за границы игры: ни нулевого, ни 151-го") {
      def gearAt(heroLvl: Long) = (1L to 300L).iterator
        .flatMap(s => LootGenerator.rollMiniBoss(pangea.model.monster.MiniBoss.FireElemental, 2L, heroLvl, Rng(s))._1)
        .flatMap(_.itemOpt).filter(_.itemType != ItemType.Material).toList
      val lowest  = gearAt(1L)   // разброс −1 увёл бы вещь в нулевой уровень
      val highest = gearAt(150L) // разброс +1 увёл бы её в 151-й
      assertTrue(lowest.nonEmpty) && assertTrue(highest.nonEmpty) &&
      assertTrue(lowest.forall(i => i.lvl >= 1L && i.lvl <= 2L)) &&
      assertTrue(highest.forall(i => i.lvl >= 149L && i.lvl <= 150L))
    },

    test("ингредиент и вещь выпадают примерно поровну") {
      val items = (1L to 600L).iterator
        .flatMap(s => LootGenerator.rollMiniBoss(pangea.model.monster.MiniBoss.FireElemental, 2L, 40L, Rng(s))._1)
        .flatMap(_.itemOpt).toList
      val materialShare = items.count(_.itemType == ItemType.Material).toDouble / items.size
      assertTrue(materialShare > 0.4 && materialShare < 0.6)
    },

    // ── Пассивки лута ─────────────────────────────────────────────────────────
    test("без пассивок доп. дропов нет и RNG не тратится") {
      val (drops, r) = LootGenerator.rollPassiveDrops(
        taxidermist = false, jeweler = false, Rarity.Common, Race.Human, 10L, Rng(7L))
      assertTrue(drops.isEmpty) && assertTrue(r == Rng(7L))
    },

    test("Таксидермист иногда даёт лишний трофей этой расы (~10%)") {
      val trophies = (1L to 1000L).iterator.flatMap { s =>
        LootGenerator.rollPassiveDrops(taxidermist = true, jeweler = false, Rarity.Rare, Race.Orc, 12L, Rng(s))._1
      }.collect { case LootDrop.Trophy(i) => i }.toList
      assertTrue(trophies.nonEmpty) &&
        assertTrue(trophies.forall(i => i.itemType == ItemType.Trophy && raceOf(i).contains(Race.Orc.entryName))) &&
        assertTrue(trophies.forall(_.lvl == 12L))
    },

    test("Таксидермист срабатывает примерно в 10% случаев") {
      val hits = (1L to 2000L).count { s =>
        LootGenerator.rollPassiveDrops(taxidermist = true, jeweler = false, Rarity.Rare, Race.Orc, 12L, Rng(s))._1.nonEmpty
      }
      assertTrue(hits >= 140 && hits <= 260) // ~200 из 2000
    },

    test("Ювелир иногда даёт отдельную (не груду) порцию серебра ~lvl×4±20%") {
      val silvers = (1L to 1000L).iterator.flatMap { s =>
        LootGenerator.rollPassiveDrops(taxidermist = false, jeweler = true, Rarity.Common, Race.Human, 10L, Rng(s))._1
      }.collect { case LootDrop.Silver(a, pile) => (a, pile) }.toList
      assertTrue(silvers.nonEmpty) &&
        assertTrue(silvers.forall { case (a, pile) => a >= 32L && a <= 48L && !pile })
    },

    test("обе пассивки вместе могут дать и трофей, и серебро за один вызов") {
      val both = (1L to 2000L).iterator.map { s =>
        LootGenerator.rollPassiveDrops(taxidermist = true, jeweler = true, Rarity.Rare, Race.Orc, 12L, Rng(s))._1
      }.exists { ds =>
        ds.exists { case LootDrop.Trophy(_) => true; case _ => false } &&
        ds.exists { case LootDrop.Silver(_, _) => true; case _ => false }
      }
      assertTrue(both)
    }
  )
}
