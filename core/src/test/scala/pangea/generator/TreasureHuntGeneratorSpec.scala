package pangea.generator

import pangea.domain.Rng
import pangea.generator.loot.TreasureHuntGenerator
import pangea.model.item.{MapZone, Rarity}
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

    test("золото выпадает всегда (гарантированно положительное)") {
      assertTrue(rewards.forall(_.gold > 0L))
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

    test("детерминизм: один seed → одинаковая добыча") {
      val (a, _) = TreasureHuntGenerator.roll(zone, Rng(777L))
      val (b, _) = TreasureHuntGenerator.roll(zone, Rng(777L))
      assertTrue(a.gold == b.gold) &&
        assertTrue(a.doubloons == b.doubloons) &&
        assertTrue(a.items.map(_.name) == b.items.map(_.name))
    }
  )
}
