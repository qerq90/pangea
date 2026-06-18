package pangea.model.battle

import pangea.model.stats.FightStats
import zio.test._

object HeroBattleStateSpec extends ZIOSpecDefault {

  private val base = FightStats(atk = 100, hp = 200, armor = 50, defence = 10,
                                evasion = 5, accuracy = 20, concentration = 0)

  override def spec = suite("HeroBattleState")(

    test("applyTo без баффов → stats не меняются") {
      assertTrue(HeroBattleState.empty.applyTo(base) == base)
    },

    test("applyTo с atk-баффом → atk увеличивается") {
      val buff  = Buff(atk = 50L, armor = 0L, defence = 0L, turnsLeft = None)
      val state = HeroBattleState(List(buff))
      assertTrue(state.applyTo(base).atk == 150L)
    },

    test("applyTo с defence-баффом → defence увеличивается") {
      val buff  = Buff(atk = 0L, armor = 0L, defence = 30L, turnsLeft = None)
      val state = HeroBattleState(List(buff))
      assertTrue(state.applyTo(base).defence == 40L)
    },

    test("applyTo суммирует несколько баффов") {
      val buffs = List(
        Buff(atk = 10L, armor = 0L, defence = 5L, turnsLeft = None),
        Buff(atk = 20L, armor = 0L, defence = 5L, turnsLeft = None)
      )
      val result = HeroBattleState(buffs).applyTo(base)
      assertTrue(result.atk == 130L) &&
      assertTrue(result.defence == 20L)
    },

    test("armorBonus суммирует armor всех баффов") {
      val buffs = List(
        Buff(atk = 0L, armor = 30L, defence = 0L, turnsLeft = None),
        Buff(atk = 0L, armor = 20L, defence = 0L, turnsLeft = None)
      )
      assertTrue(HeroBattleState(buffs).armorBonus == 50L)
    },

    test("tick уменьшает turnsLeft на 1") {
      val buff   = Buff(atk = 10L, armor = 0L, defence = 0L, turnsLeft = Some(3))
      val ticked = HeroBattleState(List(buff)).tick
      assertTrue(ticked.buffs.head.turnsLeft == Some(2))
    },

    test("tick удаляет бафф при turnsLeft = 0") {
      val buff   = Buff(atk = 10L, armor = 0L, defence = 0L, turnsLeft = Some(0))
      val ticked = HeroBattleState(List(buff)).tick
      assertTrue(ticked.isEmpty)
    },

    test("tick не трогает постоянные баффы (turnsLeft = None)") {
      val buff   = Buff(atk = 10L, armor = 0L, defence = 0L, turnsLeft = None)
      val ticked = HeroBattleState(List(buff)).tick
      assertTrue(ticked.buffs.size == 1) &&
      assertTrue(ticked.buffs.head.turnsLeft.isEmpty)
    },

    test("tick с несколькими баффами: некоторые истекают, некоторые остаются") {
      val buffs = List(
        Buff(atk = 10L, armor = 0L, defence = 0L, turnsLeft = Some(0)),  // истекает
        Buff(atk = 20L, armor = 0L, defence = 0L, turnsLeft = Some(1)),  // остаётся → Some(0)
        Buff(atk = 30L, armor = 0L, defence = 0L, turnsLeft = None)       // постоянный
      )
      val ticked = HeroBattleState(buffs).tick
      assertTrue(ticked.buffs.size == 2) &&
      assertTrue(ticked.buffs.exists(_.turnsLeft == Some(0))) &&
      assertTrue(ticked.buffs.exists(_.turnsLeft.isEmpty))
    }
  )
}
