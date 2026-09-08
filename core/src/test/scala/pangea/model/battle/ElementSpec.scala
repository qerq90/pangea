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

    test("HeroGems: молния даёт долю урона брони по HP; воздух — бонус уклонения/точности") {
      val light = HeroGems(weapon = List(gem(GemKind.Topaz)), armor = Nil)
      val air   = HeroGems(weapon = List(gem(GemKind.Diamond)), armor = Nil)
      assertTrue(math.abs(light.lightningArmorToHpFrac - 0.2) < 1e-9) &&
        assertTrue(air.airEvasionAccuracyBonusPct == 5L)
    },

    // ── Стихийные грани урона ─────────────────────────────────────────────────
    // Базовые ставки каждой стихии: множитель урона по броне и по HP.
    test("базовые грани стихий: огонь −20%/+10%, холод +10%/0, молния −20%/−10%, воздух −10%/−10%") {
      def mults(kind: GemKind) = {
        val g = HeroGems(weapon = List(gem(kind, 0)), armor = Nil) // грейд 0 → усиления нет
        (g.armorDamageMult, g.hpDamageMult)
      }
      val (fireA, fireH)   = mults(GemKind.Ruby)
      val (coldA, coldH)   = mults(GemKind.Sapphire)
      val (lightA, lightH) = mults(GemKind.Topaz)
      val (airA, airH)     = mults(GemKind.Diamond)
      assertTrue(math.abs(fireA - 0.80) < 1e-9 && math.abs(fireH - 1.10) < 1e-9) &&
      assertTrue(math.abs(coldA - 1.10) < 1e-9 && math.abs(coldH - 1.00) < 1e-9) &&
      assertTrue(math.abs(lightA - 0.80) < 1e-9 && math.abs(lightH - 0.90) < 1e-9) &&
      assertTrue(math.abs(airA - 0.90) < 1e-9 && math.abs(airH - 0.90) < 1e-9)
    },

    // Усиление стихии сдвигает ОБЕ грани на столько же процентных пунктов —
    // именно так, как задумано: огонь с +5% бьёт на −15% по броне и +15% по HP.
    test("усиление стихии двигает обе грани в п.п.: огонь +5% → −15% по броне, +15% по HP") {
      // Усиление = 2% за грейд, поэтому грейд 2.5 недостижим — берём грейд 5 (+10%)
      // и грейд 1 (+2%), чтобы проверить формулу на двух точках.
      val g5 = HeroGems(weapon = List(gem(GemKind.Ruby, 5)), armor = Nil)
      val g1 = HeroGems(weapon = List(gem(GemKind.Ruby, 1)), armor = Nil)
      assertTrue(math.abs(g5.elementalBoost - 0.10) < 1e-9) &&
      assertTrue(math.abs(g5.armorDamageMult - 0.90) < 1e-9) && // 0.80 + 0.10
      assertTrue(math.abs(g5.hpDamageMult    - 1.20) < 1e-9) && // 1.10 + 0.10
      assertTrue(math.abs(g1.armorDamageMult - 0.82) < 1e-9) && // 0.80 + 0.02
      assertTrue(math.abs(g1.hpDamageMult    - 1.12) < 1e-9)    // 1.10 + 0.02
    },

    test("усиление двигает грани у каждой стихии одинаково (+2% за грейд)") {
      val boost = 0.02 * 4 // грейд 4
      val cases = List(
        GemKind.Ruby     -> (0.80, 1.10),
        GemKind.Sapphire -> (1.10, 1.00),
        GemKind.Topaz    -> (0.80, 0.90),
        GemKind.Diamond  -> (0.90, 0.90)
      )
      assertTrue(cases.forall { case (kind, (baseArmor, baseHp)) =>
        val g = HeroGems(weapon = List(gem(kind, 4)), armor = Nil)
        math.abs(g.armorDamageMult - (baseArmor + boost)) < 1e-9 &&
        math.abs(g.hpDamageMult - (baseHp + boost)) < 1e-9
      })
    },

    test("нестихийные камни (череп/аметист/изумруд) грани урона не трогают") {
      val g = HeroGems(weapon = List(gem(GemKind.Skull, 5), gem(GemKind.Amethyst, 5), gem(GemKind.Emerald, 5)), armor = Nil)
      assertTrue(g.weaponElements.isEmpty) &&
      assertTrue(g.elementalBoost == 0.0) &&
      assertTrue(g.armorDamageMult == 1.0) &&
      assertTrue(g.hpDamageMult == 1.0)
    },

    test("HeroGems: несколько стихий — базы перемножаются, усиление прибавляется один раз") {
      val g = HeroGems(weapon = List(gem(GemKind.Ruby, 2), gem(GemKind.Diamond, 1)), armor = Nil)
      // огонь×воздух: armor 0.8·0.9 = 0.72, hp 1.1·0.9 = 0.99; усиление 0.02·3 = 0.06
      assertTrue(math.abs(g.elementalBoost - 0.06) < 1e-9) &&
        assertTrue(math.abs(g.armorDamageMult - 0.78) < 1e-9) &&
        assertTrue(math.abs(g.hpDamageMult - 1.05) < 1e-9)
    },

    test("одинаковые стихийные камни: база не стакается, а усиление копится по грейдам") {
      val one = HeroGems(weapon = List(gem(GemKind.Ruby, 3)), armor = Nil)
      val two = HeroGems(weapon = List(gem(GemKind.Ruby, 3), gem(GemKind.Ruby, 2)), armor = Nil)
      assertTrue(one.weaponElements == two.weaponElements) &&               // стихия одна
      assertTrue(math.abs(one.elementalBoost - 0.06) < 1e-9) &&
      assertTrue(math.abs(two.elementalBoost - 0.10) < 1e-9) &&             // грейды 3+2
      assertTrue(math.abs(two.armorDamageMult - 0.90) < 1e-9) &&            // 0.80 + 0.10
      assertTrue(math.abs(two.hpDamageMult - 1.20) < 1e-9)
    }
  )
}
