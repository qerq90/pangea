package pangea.model.battle

import io.circe.syntax.EncoderOps
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.user.UserId
import pangea.test.TestFixtures
import zio.test._

/** Группа поверх боя 1 на 1: активная пара живёт в полях SoloPveBattle, остальные
  * мобы — слотами. Смена пары перекладывает моба вместе с его эффектами. */
object GroupStateSpec extends ZIOSpecDefault {

  private val hero = TestFixtures.hero(UserId(1L))

  private def monster(race: Race, hp: Long, rarity: Rarity = Rarity.Common): Monster =
    Monster(0L, 10L, race, rarity,
      FightStats(atk = 10, hp = hp, armor = 50, defence = 0, evasion = 0, accuracy = 10, energy = 100))

  private val trio = List(monster(Race.Orc, 100L), monster(Race.Orc, 200L), monster(Race.Orc, 300L))

  override def spec = suite("GroupState")(

    test("бой из группы: первый в паре, остальные слотами по номерам, раса первого запомнена") {
      val b = SoloPveBattle.fromGroup(trio, hero, List(5L, 6L, 7L))
      assertTrue(b.isGroup) &&
      assertTrue(b.monsterCurrentHp == 100L) &&
      assertTrue(b.monsterCurrentEnergy == 5L) &&
      assertTrue(b.group.others.map(_.currentHp) == List(200L, 300L)) &&
      assertTrue(b.group.others.map(_.currentEnergy) == List(6L, 7L)) &&
      assertTrue(b.reinforcementRace == Race.Orc.entryName) &&
      assertTrue(b.group.aliveCount == 3)
    },

    test("обычный бой 1 на 1 — группа из одного") {
      val b = SoloPveBattle.from(trio.head, hero)
      assertTrue(!b.isGroup) && assertTrue(b.group == GroupState.empty) &&
      assertTrue(b.monstersInOrder.size == 1)
    },

    test("своп: мобы меняются местами вместе со своими эффектами, геройские остаются") {
      val b0 = SoloPveBattle.fromGroup(trio, hero, Nil)
      // на активном — яд, у героя — реген; у моба № 2 — горение
      val b1 = b0.copy(effects = b0.effects.copy(
        monsterPoison = Some(Poison(5)), heroRegen = Some(Regen(3))))
      val withBurn = b1.copy(group = b1.group.copy(
        others = b1.group.others.updated(0, b1.group.others.head.copy(effects = BattleEffects(monsterBurn = Some(Burn(4)))))))
      val swapped = withBurn.swapWith(0)
      assertTrue(swapped.monsterCurrentHp == 200L) &&                      // в паре теперь второй
      assertTrue(swapped.effects.monsterBurn.contains(Burn(4))) &&         // и его горение с ним
      assertTrue(swapped.effects.monsterPoison.isEmpty) &&                 // яд первого уехал
      assertTrue(swapped.effects.heroRegen.contains(Regen(3))) &&          // реген героя на месте
      assertTrue(swapped.group.others.head.currentHp == 100L) &&           // первый ушёл в слот
      assertTrue(swapped.group.others.head.effects.monsterPoison.contains(Poison(5)))
    },

    test("своп с чужим индексом ничего не меняет") {
      val b = SoloPveBattle.fromGroup(trio, hero, Nil)
      assertTrue(b.swapWith(7) == b)
    },

    test("павший активный уходит в slain, в пару встаёт следующий по номеру") {
      val b   = SoloPveBattle.fromGroup(trio, hero, Nil).copy(monsterCurrentHp = 0L)
      val nxt = b.promoteNext.get
      assertTrue(nxt.monsterCurrentHp == 200L) &&
      assertTrue(nxt.group.others.map(_.currentHp) == List(300L)) &&
      assertTrue(nxt.group.slain.size == 1) &&
      assertTrue(nxt.group.slain.head.race == Race.Orc.entryName)
    },

    test("последнему мобу заменить себя некем — это победа") {
      val b = SoloPveBattle.from(trio.head, hero).copy(monsterCurrentHp = 0L)
      assertTrue(b.promoteNext.isEmpty)
    },

    test("подкрепление встаёт последним по номеру") {
      val b   = SoloPveBattle.fromGroup(trio.take(2), hero, Nil)
      val slot = SoloPveBattle.fromGroup(List(monster(Race.Orc, 999L)), hero, Nil).activeSlot
      val more = b.withReinforcement(slot)
      assertTrue(more.group.others.map(_.currentHp) == List(200L, 999L))
    },

    test("перемешивание: любой моб может оказаться в паре, никто не теряется") {
      val b = SoloPveBattle.fromGroup(trio, hero, Nil)
      val r = b.reorderMonsters(List(2, 0, 1))
      assertTrue(r.monsterCurrentHp == 300L) &&
      assertTrue(r.group.others.map(_.currentHp) == List(100L, 200L)) &&
      // плохой порядок (не перестановка) отвергается
      assertTrue(b.reorderMonsters(List(0, 0, 1)) == b)
    },

    test("группа переживает сериализацию, а старая запись без группы читается как 1 на 1") {
      val b    = SoloPveBattle.fromGroup(trio, hero, List(1L, 2L, 3L))
      val back = b.asJson.as[SoloPveBattle].toOption.get
      val old  = SoloPveBattle.from(trio.head, hero).asJson.hcursor.downField("group").delete.top.get
      assertTrue(back == b) &&
      assertTrue(old.as[SoloPveBattle].toOption.exists(!_.isGroup))
    }
  )
}
