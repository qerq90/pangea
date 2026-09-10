package pangea.model.hero

import pangea.model.battle.Element
import pangea.model.item.{Gem, GemKind, MaterialKind}
import zio.test._

/** Пыль камней: сколько её остаётся от камня, что она даёт оружию и когда
  * магия отвечает всполохом. */
object WeaponDustSpec extends ZIOSpecDefault {

  private def dust(kind: GemKind): MaterialKind = MaterialKind.dustOf(kind)

  override def spec = suite("Пыль камней")(

    // ── Сколько пыли даёт камень ─────────────────────────────────────────────
    test("надколотый даёт единицу, каждый следующий грейд — на одну больше") {
      assertTrue(Gem(GemKind.Ruby, 1).dustYield == 1) &&
      assertTrue(Gem(GemKind.Ruby, 2).dustYield == 2) &&
      assertTrue(Gem(GemKind.Ruby, 3).dustYield == 3) &&
      assertTrue(Gem(GemKind.Ruby, 4).dustYield == 4) &&
      assertTrue(Gem(GemKind.Ruby, 5).dustYield == 5)
    },

    test("у каждого камня своя пыль, у черепа — чёрный порошок") {
      assertTrue(dust(GemKind.Diamond)  == MaterialKind.DiamondDust) &&
      assertTrue(dust(GemKind.Ruby)     == MaterialKind.RubyDust) &&
      assertTrue(dust(GemKind.Topaz)    == MaterialKind.TopazDust) &&
      assertTrue(dust(GemKind.Emerald)  == MaterialKind.EmeraldDust) &&
      assertTrue(dust(GemKind.Sapphire) == MaterialKind.SapphireDust) &&
      assertTrue(dust(GemKind.Amethyst) == MaterialKind.AmethystDust) &&
      assertTrue(dust(GemKind.Skull)    == MaterialKind.BlackPowder) &&
      // семь камней — семь видов пыли, ни одного лишнего
      assertTrue(MaterialKind.dusts.size == GemKind.values.size)
    },

    // ── Покрытие оружия ──────────────────────────────────────────────────────
    test("слой пыли работает как камень грейда 1") {
      val one = WeaponDust(List(MaterialKind.TopazDust))
      assertTrue(one.gems == List(Gem(GemKind.Topaz, 1))) &&
      assertTrue(one.elements == Set[Element](Element.Lightning))
    },

    test("стихийная пыль даёт стихию, но не поднимает эффективность стихии") {
      val gems = HeroGems(weapon = Nil, armor = Nil, dust = WeaponDust(List(MaterialKind.TopazDust)).gems)
      assertTrue(gems.weaponElements == Set[Element](Element.Lightning)) &&
      assertTrue(gems.elementalBoost == 0.0) &&
      // а тот же камень в гнезде эффективность поднимает — 2% за грейд
      assertTrue(HeroGems(List(Gem(GemKind.Topaz, 1)), Nil).elementalBoost == 0.02)
    },

    test("нестихийная пыль даёт свою оружейную грань в размере грейда 1") {
      val black   = HeroGems(Nil, Nil, WeaponDust(List(MaterialKind.BlackPowder)).gems)
      val emerald = HeroGems(Nil, Nil, WeaponDust(List(MaterialKind.EmeraldDust)).gems)
      val amethyst = HeroGems(Nil, Nil, WeaponDust(List(MaterialKind.AmethystDust)).gems)
      assertTrue(black.vampirismPct == GemKind.Skull.WeaponVampPctPerGrade) &&
      assertTrue(emerald.weaponPoisonPct == 2) && // 1.5% округляется к 2
      assertTrue(amethyst.accuracyBonusPct == GemKind.Amethyst.WeaponAccuracyPctPerGrade)
    },

    // ── Всполох магии ────────────────────────────────────────────────────────
    test("сапфировая пыль на оружие с рубином — всполох и штраф вместо эффекта") {
      val outcome = WeaponDust.sprinkle(
        MaterialKind.SapphireDust, WeaponDust.empty, List(Gem(GemKind.Ruby, 3)))
      val next = outcome.next
      assertTrue(outcome.isInstanceOf[WeaponDust.Outcome.Clash]) &&
      assertTrue(next.layers.isEmpty) &&
      assertTrue(next.penalty) &&
      assertTrue(next.damageMult == 0.75)
    },

    test("рубиновая пыль на оружие с сапфиром — тот же всполох") {
      val outcome = WeaponDust.sprinkle(
        MaterialKind.RubyDust, WeaponDust.empty, List(Gem(GemKind.Sapphire, 1)))
      assertTrue(outcome.isInstanceOf[WeaponDust.Outcome.Clash]) && assertTrue(outcome.next.penalty)
    },

    test("сначала рубиновая пыль, потом сапфировая — всполох от самой пыли") {
      val first  = WeaponDust.sprinkle(MaterialKind.RubyDust, WeaponDust.empty, Nil)
      val second = WeaponDust.sprinkle(MaterialKind.SapphireDust, first.next, Nil)
      assertTrue(first.isInstanceOf[WeaponDust.Outcome.Applied]) &&
      assertTrue(second.isInstanceOf[WeaponDust.Outcome.Clash]) &&
      // прежний слой остаётся, новый не встал
      assertTrue(second.next.layers == List(MaterialKind.RubyDust)) &&
      assertTrue(second.next.penalty)
    },

    test("и в обратном порядке — сапфировая, затем рубиновая") {
      val first  = WeaponDust.sprinkle(MaterialKind.SapphireDust, WeaponDust.empty, Nil)
      val second = WeaponDust.sprinkle(MaterialKind.RubyDust, first.next, Nil)
      assertTrue(second.isInstanceOf[WeaponDust.Outcome.Clash]) && assertTrue(second.next.penalty)
    },

    test("прочие стихии уживаются: молния поверх воздуха ложится спокойно") {
      val first  = WeaponDust.sprinkle(MaterialKind.DiamondDust, WeaponDust.empty, Nil)
      val second = WeaponDust.sprinkle(MaterialKind.TopazDust, first.next, Nil)
      assertTrue(second.isInstanceOf[WeaponDust.Outcome.Applied]) &&
      assertTrue(second.next.layers.size == 2) &&
      assertTrue(!second.next.penalty)
    },

    // ── Четвёртая горсть ─────────────────────────────────────────────────────
    test("три слоя держатся, четвёртая горсть сбивает всё и даёт штраф") {
      val three = List(MaterialKind.TopazDust, MaterialKind.DiamondDust, MaterialKind.AmethystDust)
        .foldLeft(WeaponDust.empty)((acc, d) => WeaponDust.sprinkle(d, acc, Nil).next)
      val fourth = WeaponDust.sprinkle(MaterialKind.BlackPowder, three, Nil)
      assertTrue(three.layers.size == WeaponDust.MaxLayers) &&
      assertTrue(!three.penalty) &&
      assertTrue(fourth.isInstanceOf[WeaponDust.Outcome.Overload]) &&
      assertTrue(fourth.next.layers.isEmpty) &&
      assertTrue(fourth.next.penalty)
    },

    test("штраф режет урон ровно на четверть, чистое покрытие — нет") {
      assertTrue(WeaponDust.empty.damageMult == 1.0) &&
      assertTrue(WeaponDust(List(MaterialKind.RubyDust)).damageMult == 1.0) &&
      assertTrue(WeaponDust(Nil, penalty = true).damageMult == 0.75) &&
      assertTrue(WeaponDust.PenaltyPct == 25L)
    },

    // ── Хранение ─────────────────────────────────────────────────────────────
    test("запись читается по полям: чего нет — берётся по умолчанию") {
      import io.circe.syntax.EncoderOps
      val stored = WeaponDust(List(MaterialKind.RubyDust), penalty = true)
      assertTrue(stored.asJson.as[WeaponDust].toOption.contains(stored)) &&
      assertTrue(io.circe.Json.obj().as[WeaponDust].toOption.contains(WeaponDust.empty))
    }
  )
}
