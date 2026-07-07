package pangea.model.battle

import io.circe.syntax.EncoderOps
import pangea.model.monster.{Race, Rarity}
import pangea.model.skill.Skill
import pangea.model.stats.FightStats
import zio.test._

object SoloPveBattleSkillsSpec extends ZIOSpecDefault {

  private def stub(slots: List[SkillSlotState]): SoloPveBattle = SoloPveBattle(
    monsterLvl          = 1L,
    monsterRace         = Race.values.head.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 10, hp = 100, armor = 0, defence = 0, evasion = 0, accuracy = 0, concentration = 0),
    monsterCurrentHp    = 100L,
    monsterCurrentArmor = 0L,
    skillSlots          = slots
  )

  def spec = suite("SoloPveBattleSkillsSpec")(
    test("tickBuffs уменьшает cooldown на 1, не ниже 0") {
      val b   = stub(List(
        SkillSlotState(1L, Skill.SweepingStrike, cooldown = 2),
        SkillSlotState(2L, Skill.MinorHeal,      cooldown = 0)
      )).tickBuffs()
      val ws  = b.slotByItem(1L).get
      val hs  = b.slotByItem(2L).get
      assertTrue(ws.cooldown == 1) && assertTrue(hs.cooldown == 0)
    },

    test("updateSlot обновляет только нужный слот") {
      val b = stub(List(
        SkillSlotState(10L, Skill.QuickStrike, cooldown = 0, uses = 0),
        SkillSlotState(20L, Skill.Ram,         cooldown = 0, uses = 5)
      )).updateSlot(10L)(s => s.copy(cooldown = s.skill.cooldown, uses = s.uses + 1))
      val one = b.slotByItem(10L).get
      val two = b.slotByItem(20L).get
      assertTrue(one.cooldown == Skill.QuickStrike.cooldown) &&
      assertTrue(one.uses == 1) &&
      assertTrue(two.uses == 5) // не тронут
    },

    test("Два слота с одинаковым Skill имеют независимые cd/uses") {
      val twoHeals = List(
        SkillSlotState(100L, Skill.MinorHeal, cooldown = 0, uses = 0),
        SkillSlotState(200L, Skill.MinorHeal, cooldown = 0, uses = 0)
      )
      val b = stub(twoHeals).updateSlot(100L)(s => s.copy(cooldown = 2, uses = 1))
      val first  = b.slotByItem(100L).get
      val second = b.slotByItem(200L).get
      assertTrue(first.cooldown == 2 && first.uses == 1) &&
      assertTrue(second.cooldown == 0 && second.uses == 0)
    },

    test("Кодирование→декодирование skillSlots round-trip") {
      val original = stub(List(
        SkillSlotState(7L,  Skill.CunningStrike, cooldown = 3, uses = 2),
        SkillSlotState(11L, Skill.Ram,           cooldown = 0, uses = 0)
      ))
      val decoded = original.asJson.as[SoloPveBattle]
      assertTrue(decoded.exists(_.skillSlots == original.skillSlots))
    },

    test("Декодер совместим со старыми JSON-записями без skillSlots") {
      val legacy = stub(Nil).asJson
        .mapObject(_.remove("skillSlots"))
      val decoded = legacy.as[SoloPveBattle]
      assertTrue(decoded.exists(_.skillSlots.isEmpty))
    }
  )
}
