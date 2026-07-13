package pangea.service.state.states.battle

import zio.test._

/** Чистые тесты на BattleState.damageReduction, в частности на баф «Заслона»
  * (`bonusPct`): он ПРИБАВЛЯЕТСЯ в п.п. к итоговому снижению урона и всё равно
  * упирается в кап 0.7. */
object DamageReductionSpec extends ZIOSpecDefault {

  private val eps = 1e-9

  // protection=10, defenderInt=10, attackerInt=10 → numer=20, denom=40, raw=0.5.
  private def reduction(bonusPct: Long): Double =
    BattleState.damageReduction(
      protection  = 10L,
      defenderInt = 10L,
      attackerInt = 10L,
      bonusPct    = bonusPct
    )

  override def spec = suite("BattleState.damageReduction")(

    test("bonusPct = 0 → чистая формула (P+Iₚ)/(P+Iₚ+2·Iₑ)") {
      assertTrue(math.abs(reduction(0L) - 0.5) < eps)
    },

    test("bonusPct прибавляется к итогу в п.п., а не умножает его") {
      // 5% Заслона: 0.5 → 0.55, а не 0.5·1.05 = 0.525.
      assertTrue(math.abs(reduction(5L) - 0.55) < eps) &&
      assertTrue(math.abs((reduction(5L) - reduction(0L)) - 0.05) < eps)
    },

    test("итог зажат сверху 0.7: база + бонус не пробивают кап") {
      // 0.5 + 0.25 = 0.75 → зажимается в 0.7.
      assertTrue(math.abs(reduction(25L) - 0.7) < eps)
    },

    test("кап 0.7 держится и без бонуса, когда база уже выше") {
      // numer=200, denom=220, raw≈0.909 → 0.7; +5% всё равно 0.7.
      val base    = BattleState.damageReduction(100L, 100L, 10L, bonusPct = 0L)
      val boosted = BattleState.damageReduction(100L, 100L, 10L, bonusPct = 5L)
      assertTrue(math.abs(base - 0.7) < eps) &&
      assertTrue(math.abs(boosted - 0.7) < eps)
    }
  )
}
