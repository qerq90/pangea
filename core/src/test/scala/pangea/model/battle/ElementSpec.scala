package pangea.model.battle

import pangea.model.hero.HeroGems
import pangea.model.item.{Gem, GemKind}
import zio.test._

object ElementSpec extends ZIOSpecDefault {

  private def gem(kind: GemKind, grade: Int = 1) = Gem(kind, grade)

  override def spec = suite("Element/DoT model")(

    test("стихии камней: сапфир→Холод, рубин→Огонь, топаз→Молния, бриллиант→Воздух") {
      assertTrue(Element.of(GemKind.Sapphire).contains(Element.Cold)) &&
        assertTrue(Element.of(GemKind.Ruby).contains(Element.Fire)) &&
        assertTrue(Element.of(GemKind.Topaz).contains(Element.Lightning)) &&
        assertTrue(Element.of(GemKind.Diamond).contains(Element.Air)) &&
        assertTrue(Element.of(GemKind.Skull).isEmpty) &&
        assertTrue(Element.of(GemKind.Amethyst).isEmpty) &&
        assertTrue(Element.of(GemKind.Emerald).isEmpty)
    },

    test("горение растёт: +2 п.п. до 10, дальше +1") {
      val curve = Iterator.iterate(Burn(Burn.Initial))(_.grown).map(_.pct).take(7).toList
      assertTrue(curve == List(2, 4, 6, 8, 10, 11, 12))
    },

    test("горение: повторный поджог +2; ослабление лечения = 50 + pct") {
      assertTrue(Burn(4).reignited.pct == 6) &&
        assertTrue(Burn(8).healWeakenPct == 58) &&
        assertTrue(Burn.onIgnite.pct == 2)
    },

    test("кровотечение стакается и НЕ имеет метода затухания") {
      val b = Bleed(4).stackedWith(4)
      assertTrue(b.pct == 8) &&
        assertTrue(b.damageOn(1000) == 80L)
    },

    test("яд: урон = % макс.HP, затухает и ослабляется лечением (в отличие от крови)") {
      assertTrue(Poison(10).damageOn(1000) == 100L) &&
        assertTrue(Poison(10).decayed.exists(_.pct == 8)) &&
        assertTrue(Poison(2).decayed.isEmpty)
    },

    test("HeroGems: огонь в оружии — множители урона и грейд-бонус") {
      val g = HeroGems(weapon = List(gem(GemKind.Ruby, 3)), armor = Nil)
      assertTrue(g.hasElement(Element.Fire)) &&
        assertTrue(math.abs(g.armorDamageMult - 0.8) < 1e-9) &&
        assertTrue(math.abs(g.hpDamageMult - 1.1) < 1e-9) &&
        assertTrue(math.abs(g.elementalDamageMult - 1.06) < 1e-9) // 1 + 0.02·3
    },

    test("HeroGems: молния даёт долю урона брони по HP; воздух — бонус уклонения/точности") {
      val light = HeroGems(weapon = List(gem(GemKind.Topaz)), armor = Nil)
      val air   = HeroGems(weapon = List(gem(GemKind.Diamond)), armor = Nil)
      assertTrue(math.abs(light.lightningArmorToHpFrac - 0.2) < 1e-9) &&
        assertTrue(air.airEvasionAccuracyBonusPct == 5L)
    },

    test("HeroGems: несколько стихий — множители перемножаются, грейды суммируются") {
      val g = HeroGems(weapon = List(gem(GemKind.Ruby, 2), gem(GemKind.Diamond, 1)), armor = Nil)
      // огонь×воздух: armor 0.8·0.9, hp 1.1·0.9; грейд-бонус 1 + 0.02·3
      assertTrue(math.abs(g.armorDamageMult - 0.72) < 1e-9) &&
        assertTrue(math.abs(g.hpDamageMult - 0.99) < 1e-9) &&
        assertTrue(math.abs(g.elementalDamageMult - 1.06) < 1e-9)
    }
  )
}
