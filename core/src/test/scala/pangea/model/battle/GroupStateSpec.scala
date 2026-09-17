package pangea.model.battle

import io.circe.syntax.EncoderOps
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.squad.{Ally, AllyKind, Squad}
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

    test("павший активный уходит в slain, к герою шагает ближайший (при равенстве — правее), его место пустеет; герой стоит") {
      val b    = SoloPveBattle.fromGroup(trio, hero, Nil).copy(monsterCurrentHp = 0L)
      val nxt  = b.promoteNext.get
      val edge = SoloPveBattle.fromGroup(trio, hero, Nil).moveHeroTo(3).copy(monsterCurrentHp = 0L).promoteNext.get
      val mid  = SoloPveBattle.fromGroup(trio, hero, Nil).moveHeroTo(2).copy(monsterCurrentHp = 0L).promoteNext.get
      assertTrue(nxt.monsterCurrentHp == 200L && nxt.group.heroPos == 1) &&
      assertTrue(nxt.placesInOrder.map(_._2.map(_.currentHp)) == List(Some(200L), None, Some(300L))) &&
      assertTrue(nxt.group.slain.size == 1) &&
      assertTrue(nxt.group.slain.head.race == Race.Orc.entryName) &&
      assertTrue(edge.group.heroPos == 3 && edge.monsterCurrentHp == 200L) &&
      assertTrue(mid.group.heroPos == 2 && mid.monsterCurrentHp == 300L)   // равные — правее
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

    test("моб обычной встречи встаёт на место 1, где бы ни стоял герой: герой на 2 — пара пуста, цель у него одна — сосед") {
      val h2 = hero.copy(squad = Squad(heroPos = 2, allies = List(Ally(AllyKind.Human, 1, 10L, 10L, 0L))))
      val b  = SoloPveBattle.from(trio.head, h2)
      val b3 = SoloPveBattle.from(trio.head, hero.copy(squad = Squad(heroPos = 3)))
      assertTrue(b.group.activePos == 1 && b.group.heroPos == 2 && !b.group.paired && b.unpaired) &&
      assertTrue(b.monsterAt(1).exists(_.currentHp == 100L) && b.monsterAt(2).isEmpty && b.pairedMonster.isEmpty) &&
      assertTrue(b.group.attackTargets == List(1) && b.group.inReach(1) && b.group.hasFormation) &&
      assertTrue(b3.group.attackTargets.isEmpty)                                   // с места 3 до места 1 не достать
    },

    test("группа: мобы по местам 1, 2, 3; в паре — тот, что на месте героя") {
      val h2 = hero.copy(squad = Squad(heroPos = 2))
      val b  = SoloPveBattle.fromGroup(trio, h2, Nil)
      assertTrue(b.group.paired && b.group.activePos == 2 && b.monsterCurrentHp == 200L) &&
      assertTrue(b.group.others.map(_.currentHp) == List(100L, 300L) && b.group.places == List(1, 3)) &&
      assertTrue(b.group.attackTargets == List(1, 2, 3))
    },

    test("engage разворачивает соседа в поля, не двигая ни героя, ни мобов; повтор возвращает всё назад") {
      val b = SoloPveBattle.fromGroup(trio, hero, Nil)
      val e = b.engage(3)
      assertTrue(e.group.heroPos == 1 && e.group.activePos == 3 && e.monsterCurrentHp == 300L) &&
      assertTrue(e.group.others.map(_.currentHp) == List(200L, 100L) && e.group.places == List(2, 1)) &&
      assertTrue(e.monstersInOrder.map(_.currentHp) == List(100L, 200L, 300L)) &&
      assertTrue(e.engage(1) == b) && assertTrue(b.engage(1) == b && b.engage(9) == b)
    },

    test("павший активный: к герою шагает ближайший свободный, а занятый союзником — нет; никого свободного — пара пуста") {
      val ally2 = Ally(AllyKind.Human, 2, 10L, 10L, 0L)
      val h     = hero.copy(squad = Squad(heroPos = 1, allies = List(ally2)))
      val dead  = SoloPveBattle.fromGroup(trio, h, Nil).copy(monsterCurrentHp = 0L)
      val nxt   = dead.promoteNext.get
      val stuck = SoloPveBattle.fromGroup(trio.take(2), h, Nil).copy(monsterCurrentHp = 0L).promoteNext.get
      assertTrue(nxt.group.paired && nxt.monsterCurrentHp == 300L) &&                 // № 3 свободен — шагнул, № 2 занят
      assertTrue(nxt.group.others.map(_.currentHp) == List(200L) && nxt.group.places == List(2)) &&
      assertTrue(!stuck.group.paired && stuck.group.activePos == 2 && stuck.monsterCurrentHp == 200L) &&
      assertTrue(stuck.group.others.isEmpty && stuck.group.slain.size == 1 && stuck.group.attackTargets == List(2))
    },

    test("pullFree: освободившийся моб шагает к герою — сам активный или ближайший из строя") {
      val h2    = hero.copy(squad = Squad(heroPos = 2, allies = List(Ally(AllyKind.Human, 1, 10L, 10L, 0L))))
      val solo  = SoloPveBattle.from(trio.head, h2)                     // моб на 1 занят союзником
      val freed = solo.copy(group = solo.group.copy(allies = Nil))      // союзник ушёл — моб свободен
      // герой на 4, союзник на 3: в начале боя свободный моб с места 1 сразу шагает к герою
      val h4    = hero.copy(squad = Squad(heroPos = 4, allies = List(Ally(AllyKind.Human, 3, 10L, 10L, 0L))))
      val far   = SoloPveBattle.fromGroup(trio, h4, Nil)
      // а если в полях занятый союзником № 3, а свободные — № 1 и № 2, к герою идёт ближний, второй
      val stuck = far.engage(3).copy(group = far.engage(3).group.copy(activePos = 3))
      val fixed = stuck.copy(group = stuck.group.copy(places = List(2, 1)))
      val pulled = fixed.pullFree.get
      assertTrue(solo.pullFree.isEmpty) &&
      assertTrue(freed.pullFree.exists(b => b.group.paired && b.group.activePos == 2)) &&
      assertTrue(far.group.paired && far.monsterCurrentHp == 100L && far.group.places == List(2, 3)) &&
      assertTrue(!fixed.group.paired && fixed.monsterCurrentHp == 300L && fixed.group.others.map(_.currentHp) == List(200L, 100L)) &&
      assertTrue(pulled.group.paired && pulled.monsterCurrentHp == 200L && pulled.group.places.sorted == List(1, 3)) &&
      assertTrue(pulled.pullFree.isEmpty)
    },

    test("группа переживает сериализацию, а старая запись без группы читается как 1 на 1") {
      val b    = SoloPveBattle.fromGroup(trio, hero, List(1L, 2L, 3L))
      val back = b.asJson.as[SoloPveBattle].toOption.get
      val old  = SoloPveBattle.from(trio.head, hero).asJson.hcursor.downField("group").delete.top.get
      val noPos = b.asJson.hcursor.downField("group").downField("activePos").delete.top.get
      assertTrue(back == b) &&
      assertTrue(old.as[SoloPveBattle].toOption.exists(!_.isGroup)) &&
      // без activePos (старая запись) активный стоит на месте героя
      assertTrue(noPos.as[SoloPveBattle].toOption.exists(_.group.paired))
    }
  )
}
