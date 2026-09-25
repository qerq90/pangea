package pangea.generator

import pangea.domain.Rng
import pangea.generator.loot.TreasureHuntGenerator
import pangea.model.item.{ItemType, MapZone, MaterialKind, Rarity}
import zio.test._

object TreasureHuntGeneratorSpec extends ZIOSpecDefault {

  private val allowedRarities: Set[Rarity] =
    Set(Rarity.Blue, Rarity.Violet, Rarity.Purple, Rarity.Orange)

  // Ущелье мертвецов — зона 51..75; лут должен катиться в этих уровнях.
  private val zone    = MapZone.DeadmansGorge
  private val rewards = (1 to 500).map(s => TreasureHuntGenerator.roll(zone, Rng(s.toLong))._1)

  override def spec = suite("TreasureHuntGenerator")(

    test("снаряжения всегда от 2 до 4 экземпляров") {
      assertTrue(rewards.forall(r => r.items.size >= 2 && r.items.size <= 4))
    },

    test("встречаются все три количества (2, 3 и 4)") {
      val counts = rewards.map(_.items.size).toSet
      assertTrue(counts == Set(2, 3, 4))
    },

    test("редкость снаряжения только из допустимого набора (от синей и выше)") {
      assertTrue(rewards.forall(_.items.forall(i => allowedRarities.contains(i.rarity))))
    },

    test("уровень снаряжения — в диапазоне зоны (51..75 для Ущелья мертвецов)") {
      val lvls = rewards.flatMap(_.items).map(_.lvl)
      assertTrue(lvls.nonEmpty) &&
        assertTrue(lvls.forall(l => l >= zone.levels.min.toLong && l <= zone.levels.max.toLong))
    },

    test("серебро выпадает всегда (гарантированно положительное)") {
      assertTrue(rewards.forall(_.silver > 0L))
    },

    test("дублоны выпадают часто, но не всегда (~80%)") {
      val withDoubloons = rewards.count(_.doubloons > 0L)
      assertTrue(withDoubloons > 0 && withDoubloons < rewards.size)
    },

    test("дублоны, если выпали, в диапазоне 30..70") {
      val doubs = rewards.map(_.doubloons).filter(_ > 0L)
      assertTrue(doubs.nonEmpty) &&
        assertTrue(doubs.forall(d => d >= 30L && d <= 70L))
    },

    test("камней всегда от 1 до 5 штук") {
      assertTrue(rewards.forall(r => r.gems.size >= 1 && r.gems.size <= 5))
    },

    test("камни — предметы типа Gem грейда «расколотый» (1-й тир)") {
      val gems = rewards.flatMap(_.gems)
      assertTrue(gems.nonEmpty) &&
        assertTrue(gems.forall(_.itemType == ItemType.Gem)) &&
        assertTrue(gems.forall(_.gem.exists(_.grade == 1)))
    },

    test("ингредиенты минибоссов: ~35% кладов, ступень зоны плюс 0..1 штук, любой ингредиент любого босса") {
      import pangea.model.monster.MiniBoss
      val pool = MiniBoss.values.map(_.ingredient).toSet
      val handfuls = rewards.map(_.materials.filter(m => m.material.exists(pool.contains)))
      val with_    = handfuls.count(_.nonEmpty)
      assertTrue(MapZone.Kinet.tier == 1 && MapZone.DeadmansGorge.tier == 3 && MapZone.AbandonedTemple.tier == 6) &&
        assertTrue(with_ > rewards.size * 28 / 100 && with_ < rewards.size * 42 / 100) &&
        assertTrue(handfuls.forall(h => h.isEmpty || h.size == zone.tier || h.size == 1 + zone.tier)) &&
        assertTrue(handfuls.exists(_.size == zone.tier) && handfuls.exists(_.size == 1 + zone.tier)) &&
        // все ингредиенты встречаются, шкура волка в том числе
        assertTrue(handfuls.flatten.flatMap(_.material).toSet == pool)
    },

    test("редкие травы — только знающему цветы 2 ранга, ~15%, тем же счётом") {
      val known   = (1 to 800).map(s => TreasureHuntGenerator.roll(zone, Rng(s.toLong), knowsRareHerbs = true)._1)
      val herbs   = known.map(_.materials.filter(_.material.exists(_.herbRank == 2)))
      val with_   = herbs.count(_.nonEmpty)
      assertTrue(rewards.forall(!_.materials.exists(_.material.exists(_.isHerb)))) &&   // без знания трав нет
        assertTrue(with_ > known.size * 10 / 100 && with_ < known.size * 20 / 100) &&
        assertTrue(herbs.forall(h => h.isEmpty || h.size == zone.tier || h.size == 1 + zone.tier)) &&
        assertTrue(herbs.flatten.flatMap(_.material).toSet == MaterialKind.commonHerbs(2).toSet) &&
        // снаряжение и серебро от знания не меняются
        assertTrue(known.take(500).map(r => (r.silver, r.items.map(_.name))) == rewards.map(r => (r.silver, r.items.map(_.name))))
    },

    test("детерминизм: один seed → одинаковая добыча") {
      val (a, _) = TreasureHuntGenerator.roll(zone, Rng(777L))
      val (b, _) = TreasureHuntGenerator.roll(zone, Rng(777L))
      assertTrue(a.silver == b.silver) &&
        assertTrue(a.doubloons == b.doubloons) &&
        assertTrue(a.items.map(_.name) == b.items.map(_.name))
    }
  )
}
