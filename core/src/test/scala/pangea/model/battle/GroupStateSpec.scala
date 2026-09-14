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

    test("шаг героя на место соседа: сосед встаёт напротив вместе со своими эффектами, прежний остаётся на месте, геройские эффекты не трогаются") {
      val b0 = SoloPveBattle.fromGroup(trio, hero, Nil)
      // на активном — яд, у героя — реген; у моба № 2 — горение
      val b1 = b0.copy(effects = b0.effects.copy(
        monsterPoison = Some(Poison(5)), heroRegen = Some(Regen(3))))
      val withBurn = b1.copy(group = b1.group.copy(
        others = b1.group.others.updated(0, b1.group.others.head.copy(effects = BattleEffects(monsterBurn = Some(Burn(4)))))))
      val moved = withBurn.moveHeroTo(2)
      assertTrue(moved.group.heroPos == 2) &&
      assertTrue(moved.monsterCurrentHp == 200L) &&                        // напротив теперь второй
      assertTrue(moved.effects.monsterBurn.contains(Burn(4))) &&           // и его горение с ним
      assertTrue(moved.effects.monsterPoison.isEmpty) &&                   // яд первого уехал
      assertTrue(moved.effects.heroRegen.contains(Regen(3))) &&            // реген героя на месте
      assertTrue(moved.group.others.head.currentHp == 100L) &&             // первый остался на месте 1
      assertTrue(moved.group.others.head.effects.monsterPoison.contains(Poison(5))) &&
      assertTrue(moved.monstersInOrder.map(_.currentHp) == List(100L, 200L, 300L)) && // мобы не двигались
      assertTrue(moved.moveHeroTo(1).monstersInOrder.map(_.currentHp) == List(100L, 200L, 300L)) &&
      assertTrue(moved.moveHeroTo(1).group.heroPos == 1 && moved.moveHeroTo(1).monsterCurrentHp == 100L)
    },

    test("шаг на чужое или своё место ничего не меняет") {
      val b = SoloPveBattle.fromGroup(trio, hero, Nil)
      assertTrue(b.moveHeroTo(7) == b) && assertTrue(b.moveHeroTo(1) == b) && assertTrue(b.moveHeroTo(0) == b)
    },

    test("места: соседи героя, радиус, индексы") {
      val b = SoloPveBattle.fromGroup(trio, hero, Nil).moveHeroTo(2)
      val g = b.group
      assertTrue(g.size == 3 && g.neighbourPositions == List(1, 3)) &&
      assertTrue(g.inReach(1) && g.inReach(3) && !g.inReach(2) && !g.inReach(4)) &&
      assertTrue(g.posOf(0) == 1 && g.posOf(1) == 3) &&
      assertTrue(g.idxOf(1) == 0 && g.idxOf(3) == 1) &&
      assertTrue(b.monsterAt(3).exists(_.currentHp == 300L))
    },

    test("павший активный уходит в slain, его место пустеет, герой шагает к ближайшему (при равенстве — правее)") {
      val b    = SoloPveBattle.fromGroup(trio, hero, Nil).copy(monsterCurrentHp = 0L)
      val nxt  = b.promoteNext.get
      val edge = SoloPveBattle.fromGroup(trio, hero, Nil).moveHeroTo(3).copy(monsterCurrentHp = 0L).promoteNext.get
      val mid  = SoloPveBattle.fromGroup(trio, hero, Nil).moveHeroTo(2).copy(monsterCurrentHp = 0L).promoteNext.get
      assertTrue(nxt.monsterCurrentHp == 200L && nxt.group.heroPos == 2) &&
      assertTrue(nxt.placesInOrder.map(_._2.map(_.currentHp)) == List(None, Some(200L), Some(300L))) &&
      assertTrue(nxt.group.slain.size == 1) &&
      assertTrue(nxt.group.slain.head.race == Race.Orc.entryName) &&
      assertTrue(edge.group.heroPos == 2 && edge.monsterCurrentHp == 200L) &&
      assertTrue(mid.group.heroPos == 3 && mid.monsterCurrentHp == 300L)   // равные — правее
    },

    test("последнему мобу заменить себя некем — это победа") {
      val b = SoloPveBattle.from(trio.head, hero).copy(monsterCurrentHp = 0L)
      assertTrue(b.promoteNext.isEmpty)
    },

    test("павший вне пары оставляет пустое место: строй не смыкается, места и Таран остаются") {
      val four = SoloPveBattle.fromGroup(trio :+ monster(Race.Orc, 400L), hero, Nil)
      val aimedAtThird  = four.copy(group = four.group.copy(pendingMove = Some(3)))
      val aimedAtSecond = four.copy(group = four.group.copy(pendingMove = Some(2)))
      val secondFell    = aimedAtThird.sideFallen(0)   // пал моб на месте 2
      val targetFell    = aimedAtSecond.sideFallen(0)
      val moved         = four.moveHeroTo(3)              // герой на 3, моб с места 1 остался на месте 1
      val leftFell      = moved.sideFallen(moved.group.idxOf(1)) // пал моб на месте 1
      assertTrue(secondFell.placesInOrder.map(_._2.map(_.currentHp)) == List(Some(100L), None, Some(300L), Some(400L))) &&
      assertTrue(secondFell.group.slain.map(_.name).size == 1) &&
      assertTrue(secondFell.group.pendingMove.contains(3)) &&     // цель на своём месте
      assertTrue(targetFell.group.pendingMove.isEmpty) &&          // цель пала — Таран сгорел
      assertTrue(leftFell.group.heroPos == 3 && leftFell.monsterCurrentHp == 300L && !leftFell.group.occupied(1)) &&
      assertTrue(!secondFell.group.inReach(2) && secondFell.group.neighbourPositions.isEmpty) &&
      assertTrue(four.sideFallen(9) == four)
    },

    test("смена пары после гибели активного гасит отложенный Таран") {
      val b = SoloPveBattle.fromGroup(trio, hero, Nil).copy(monsterCurrentHp = 0L)
      val aimed = b.copy(group = b.group.copy(pendingMove = Some(2)))
      assertTrue(aimed.promoteNext.get.group.pendingMove.isEmpty)
    },

    test("подкрепление встаёт на первое свободное место за строем") {
      val b   = SoloPveBattle.fromGroup(trio.take(2), hero, Nil)
      val slot = SoloPveBattle.fromGroup(List(monster(Race.Orc, 999L)), hero, Nil).activeSlot
      val more = b.withReinforcement(slot)
      // с пустым местом 2 подкрепление всё равно встаёт за строем, на место 4
      val gap  = b.withReinforcement(slot).sideFallen(0).withReinforcement(slot)
      assertTrue(more.group.others.map(_.currentHp) == List(200L, 999L) && more.group.places == List(2, 3)) &&
      assertTrue(gap.group.places == List(3, 4) && !gap.group.occupied(2))
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
