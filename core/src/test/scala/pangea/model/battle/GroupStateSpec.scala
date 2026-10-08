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

  /** Живой союзник на месте `pos` — чтобы напротив него было кого ставить. */
  private def fighter(kind: AllyKind, pos: Int): BattleAlly =
    BattleAlly(kind, pos, hp = 10L, armor = 0L, energy = 0L,
      FightStats(atk = 1, hp = 10, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0), lvl = 1L)

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

    // ── Единая схема строя ────────────────────────────────────────────────────
    test("схема одна на все бои: одиннадцать мест отряду, пятнадцать врагам") {
      assertTrue(Formation.HeroPlaces == 11 && Formation.MonsterPlaces == 15) &&
      // и никто не держит своего числа на стороне
      assertTrue(GroupState.MaxMonsters == Formation.MonsterPlaces) &&
      assertTrue(pangea.model.squad.AllyRates.Positions == Formation.HeroPlaces) &&
      // хвост чужого строя — за местами отряда: туда ему не дотянуться
      assertTrue(Formation.MonsterPlaces > Formation.HeroPlaces)
    },

    test("в обычный бой встают до пятнадцати, остальные ждут за строем") {
      val horde = List.fill(20)(monster(Race.Orc, 100L))
      val b     = SoloPveBattle.fromGroup(horde, hero, Nil)
      assertTrue(b.group.places == (2 to Formation.MonsterPlaces).toList) &&
      assertTrue(b.group.aliveCount == Formation.MonsterPlaces) &&
      assertTrue(b.group.queue.size == 20 - Formation.MonsterPlaces) &&
      assertTrue(!b.hasRoom && b.group.freePlaces.isEmpty) &&
      // вся схема занята, и ни одного места сверх неё
      assertTrue(b.placesInOrder.size == Formation.MonsterPlaces && b.placesInOrder.forall(_._2.isDefined))
    },

    test("сооружениям отдан хвост строя, и в очередь они не попадают") {
      val guards = List.fill(18)(monster(Race.Orc, 100L))
      val towers = List.fill(2)(monster(Race.Construct, 500L))
      val b      = SoloPveBattle.fromGroup(guards ++ towers, hero, Nil, towers = towers.size)
      val tail   = List(Formation.MonsterPlaces - 1, Formation.MonsterPlaces)
      assertTrue(b.group.places.takeRight(2) == tail) &&
      assertTrue(tail.flatMap(b.monsterAt).forall(_.race == Race.Construct.entryName)) &&
      // охрана встала перед башнями, остальные ждут
      assertTrue(b.group.places.dropRight(2) == (2 to Formation.MonsterPlaces - 2).toList) &&
      assertTrue(b.group.queue.size == 18 - (Formation.MonsterPlaces - 2)) &&
      assertTrue(b.group.queue.forall(_.race == Race.Orc.entryName))
    },

    test("подкрепление встаёт за своими, но дальше схемы не уходит") {
      val slot  = SoloPveBattle.fromGroup(List(monster(Race.Orc, 999L)), hero, Nil).activeSlot
      // Строй забит под завязку: пришедшему места нет, он ждёт за ним
      val full  = (1 to Formation.MonsterPlaces - 1).foldLeft(SoloPveBattle.fromGroup(trio.take(1), hero, Nil))(
                    (b, _) => b.withReinforcement(slot))
      val over  = full.withReinforcement(slot)
      // Башни стоят в хвосте, но строй — это те, кто ходит: пришедший встаёт за
      // охраной, а не за башнями, и прорехи впереди не занимает — их смыкает строй
      val towers  = List.fill(2)(monster(Race.Construct, 500L))
      val caravan = SoloPveBattle.fromGroup(trio.take(3) ++ towers, hero, Nil, towers = 2)
      val gap     = caravan.sideFallen(0).withReinforcement(slot)   // пало место 2
      // Охрана дошла до самых башен — свободного места нет совсем
      val packed  = SoloPveBattle.fromGroup(List.fill(13)(monster(Race.Orc, 100L)) ++ towers, hero, Nil, towers = 2)
      assertTrue(full.group.places.max == Formation.MonsterPlaces && !full.hasRoom) &&
      assertTrue(over.group.places.max == Formation.MonsterPlaces && over.group.queue.size == 1) &&
      assertTrue(caravan.reinforcementPlace.contains(4)) &&
      assertTrue(gap.group.places.last == 4 && !gap.group.occupied(2)) &&
      assertTrue(packed.group.freePlaces.isEmpty && packed.reinforcementPlace.isEmpty) &&
      assertTrue(packed.admit(slot).group.queue.size == 1)
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

    test("павший активный уходит в slain, место перед героем пустеет, а в полях — ближайший на своём месте") {
      val b    = SoloPveBattle.fromGroup(trio, hero, Nil).copy(monsterCurrentHp = 0L)
      val nxt  = b.promoteNext.get
      val edge = SoloPveBattle.fromGroup(trio, hero, Nil).moveHeroTo(3).copy(monsterCurrentHp = 0L).promoteNext.get
      val mid  = SoloPveBattle.fromGroup(trio, hero, Nil).moveHeroTo(2).copy(monsterCurrentHp = 0L).promoteNext.get
      // Замену к герою больше никто не подставляет: ближайший разворачивается в
      // полях боя, но стоит там, где стоял, и напротив героя пусто.
      assertTrue(nxt.monsterCurrentHp == 200L && nxt.group.heroPos == 1) &&
      assertTrue(nxt.group.activePos == 2 && !nxt.group.paired) &&
      assertTrue(nxt.placesInOrder.map(_._2.map(_.currentHp)) == List(None, Some(200L), Some(300L))) &&
      assertTrue(nxt.group.slain.size == 1) &&
      assertTrue(nxt.group.slain.head.race == Race.Orc.entryName) &&
      // герой с места не сходит, а в полях — ближайший к нему (при равенстве правее)
      assertTrue(edge.group.heroPos == 3 && edge.monsterCurrentHp == 200L && edge.group.activePos == 2) &&
      assertTrue(mid.group.heroPos == 2 && mid.monsterCurrentHp == 300L && mid.group.activePos == 3)
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
      val r = b.reshuffle(List(2, 1, 0))
      assertTrue(r.monsterCurrentHp == 300L) &&
      assertTrue(r.group.others.map(_.currentHp) == List(200L, 100L)) &&
      // плохой жребий (не перестановка) отвергается
      assertTrue(b.reshuffle(List(0, 0, 1)) == b)
    },

    test("перемешивание: жребий тянут подвижные, башни стоят на своих местах") {
      val towers = List.fill(2)(monster(Race.Construct, 500L))
      val b      = SoloPveBattle.fromGroup(trio ++ towers, hero, Nil, towers = towers.size)
      val r      = b.reshuffle(List(2, 1, 0))                  // охрана наоборот: 300, 200, 100
      // башни в жребий не идут, и мест своих не отдают
      val onlyTowers = SoloPveBattle.fromGroup(towers, hero, Nil, towers = 1)
      assertTrue(b.movableInOrder.map(_.currentHp) == List(100L, 200L, 300L)) &&
      assertTrue(r.placesInOrder.collect { case (p, Some(s)) => p -> s.currentHp } ==
                 List(1 -> 300L, 2 -> 200L, 3 -> 100L,
                      Formation.MonsterPlaces - 1 -> 500L, Formation.MonsterPlaces -> 500L)) &&
      assertTrue(r.monsterCurrentHp == 300L && r.group.paired) &&
      // двигать некого — бой не меняется
      assertTrue(onlyTowers.reshuffle(Nil) == onlyTowers)
    },

    test("перемешивание смыкает строй: сперва места, где напротив кто-то есть, потом первые свободные") {
      val quartet = trio :+ monster(Race.Orc, 400L)
      val b0      = SoloPveBattle.fromGroup(quartet, hero, Nil)
      // герой на 3, союзники на 1 и 5, мобы стоят врассыпную: 1, 3, 8, 12
      val b = b0.copy(group = b0.group.copy(
        heroPos = 3, activePos = 3, places = List(1, 8, 12),
        allies  = List(fighter(AllyKind.Human, 1), fighter(AllyKind.Gnome, 5))))
      val r = b.reshuffle(List(0, 1, 2, 3))
      // 3 — напротив героя, 1 и 5 — напротив союзников, четвёртому досталось
      // первое свободное место, а дыры в хвосте строя схлопнулись
      assertTrue(b.placesInOrder.collect { case (p, Some(_)) => p } == List(1, 3, 8, 12)) &&
      assertTrue(r.placesInOrder.collect { case (p, Some(_)) => p } == List(1, 2, 3, 5)) &&
      assertTrue(r.group.paired && r.group.size == 5) &&
      // лежачий герой целью не считается: его место занимают в общем порядке
      assertTrue(b.copy(group = b.group.copy(heroDown = true)).reshuffle(List(0, 1, 2, 3))
                  .placesInOrder.collect { case (p, Some(_)) => p } == List(1, 2, 3, 5))
    },

    test("моб обычной встречи встаёт на место 1, где бы ни стоял герой: герой на 2 — пара пуста, цель у него одна — сосед") {
      val h2 = hero.copy(squad = Squad(heroPos = 2, allies = List(Ally(AllyKind.Human, 1, 10L, 10L, 0L))))
      val b  = SoloPveBattle.from(trio.head, h2)
      val b3 = SoloPveBattle.from(trio.head, hero.copy(squad = Squad(heroPos = 3,
                 allies = List(Ally(AllyKind.Human, 1, 10L, 10L, 0L), Ally(AllyKind.Gnome, 2, 10L, 10L, 0L)))))
      assertTrue(b.group.activePos == 1 && b.group.heroPos == 2 && !b.group.paired && b.unpaired) &&
      assertTrue(b.monsterAt(1).exists(_.currentHp == 100L) && b.monsterAt(2).isEmpty && b.pairedMonster.isEmpty) &&
      assertTrue(b.group.attackTargets == List(1) && b.group.inReach(1) && b.group.hasFormation) &&
      assertTrue(b3.group.attackTargets.isEmpty)                                   // с места 3 до места 1 не достать
    },

    test("в начале боя пустоты в отряде схлопываются: герой один — на месте 1, в паре с мобом") {
      val stale = hero.copy(squad = Squad(heroPos = 2))
      val b     = SoloPveBattle.from(trio.head, stale)
      val gap   = hero.copy(squad = Squad(heroPos = 3, allies = List(Ally(AllyKind.Human, 1, 10L, 10L, 0L))))
      val g     = SoloPveBattle.from(trio.head, gap)
      assertTrue(b.group.heroPos == 1 && b.group.paired) &&
      assertTrue(g.group.heroPos == 2 && g.group.allyAt(1).isDefined && g.group.rows == 2)
    },

    test("группа: мобы по местам 1, 2, 3; в паре — тот, что на месте героя") {
      val h2 = hero.copy(squad = Squad(heroPos = 2, allies = List(Ally(AllyKind.Human, 1, 10L, 10L, 0L))))
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

    test("после павшего пара пустеет: место перед героем занимают сами, шаг за шагом") {
      val ally2 = Ally(AllyKind.Human, 2, 10L, 10L, 0L)
      val h     = hero.copy(squad = Squad(heroPos = 1, allies = List(ally2)))
      val dead  = SoloPveBattle.fromGroup(trio, h, Nil).copy(monsterCurrentHp = 0L)
      val nxt   = dead.promoteNext.get
      val stuck = SoloPveBattle.fromGroup(trio.take(2), h, Nil).copy(monsterCurrentHp = 0L).promoteNext.get
      // Ближайший встаёт в полях на своём месте, напротив героя пусто — но он в
      // досягаемости и бьёт сбоку, как и герой его.
      assertTrue(!nxt.group.paired && nxt.group.activePos == 2 && nxt.monsterCurrentHp == 200L) &&
      assertTrue(nxt.group.others.map(_.currentHp) == List(300L) && nxt.group.places == List(3)) &&
      assertTrue(nxt.group.attackTargets == List(2)) &&
      assertTrue(!stuck.group.paired && stuck.group.activePos == 2 && stuck.monsterCurrentHp == 200L) &&
      assertTrue(stuck.group.others.isEmpty && stuck.group.slain.size == 1 && stuck.group.attackTargets == List(2)) &&
      // а кто дошёл до места перед героем — тот и встал в пару
      assertTrue(nxt.engageArrived.isEmpty) &&
      assertTrue(nxt.copy(group = nxt.group.copy(places = List(1)))
                   .engageArrived.exists(_.group.paired))
    },

    test("pullFree: освободившийся моб шагает к герою — сам активный или ближайший из строя") {
      val h2    = hero.copy(squad = Squad(heroPos = 2, allies = List(Ally(AllyKind.Human, 1, 10L, 10L, 0L))))
      val solo  = SoloPveBattle.from(trio.head, h2)                     // моб на 1 занят союзником
      val freed = solo.copy(group = solo.group.copy(allies = Nil))      // союзник ушёл — моб свободен
      // герой на 3 за двумя союзниками: в начале боя он в паре с мобом на своём месте
      val h3    = hero.copy(squad = Squad(heroPos = 3,
                    allies = List(Ally(AllyKind.Human, 1, 10L, 10L, 0L), Ally(AllyKind.Gnome, 2, 10L, 10L, 0L))))
      val base  = SoloPveBattle.fromGroup(trio, h3, Nil)
      // а если герой отошёл на 4 (Таран назад не бывает — это модельный случай), в полях занятый союзником
      // № 3, а свободные — № 1 и № 2: к герою идёт ближний, второй
      val fixed = base.copy(group = base.group.copy(heroPos = 4, allies = List(BattleAlly.of(Ally(AllyKind.Human, 3, 10L, 10L, 0L), 1L))))
      val pulled = fixed.pullFree.get
      assertTrue(solo.pullFree.isEmpty) &&
      assertTrue(freed.pullFree.exists(b => b.group.paired && b.group.activePos == 2)) &&
      assertTrue(base.group.paired && base.monsterCurrentHp == 300L && base.group.places.sorted == List(1, 2)) &&
      assertTrue(!fixed.group.paired && fixed.monsterCurrentHp == 300L && fixed.group.others.map(_.currentHp).sorted == List(100L, 200L)) &&
      assertTrue(pulled.group.paired && pulled.monsterCurrentHp == 200L && pulled.group.places.sorted == List(1, 3)) &&
      assertTrue(pulled.pullFree.isEmpty)
    },

    // ── Башни не перегораживают улицу ────────────────────────────────────────

    test("за башнями не топчутся: моб перешагивает её и встаёт сразу за ней") {
      // Герой на первом месте, моб на шестнадцатом, башни на четырнадцатом и
      // пятнадцатом. Шаг в сторону героя — сразу на тринадцатое.
      val mob   = monster(Race.Orc, 100L)
      val tower = monster(Race.Construct, 500L, Rarity.Rare)
      val b0    = SoloPveBattle.fromGroup(List(mob, tower, tower, mob), hero, Nil)
      val b     = b0.copy(group = b0.group.copy(heroPos = 1, activePos = 1, places = List(14, 15, 16)))
      val (after, moved) = b.closeIn
      assertTrue(b.stepTowardsHero(16).contains(13)) &&
      assertTrue(after.group.places == List(14, 15, 13)) &&
      assertTrue(moved.map(_._2) == List(13)) &&
      // сами башни с места не сошли
      assertTrue(after.group.others.map(_.race).take(2).forall(_ == Race.Construct.entryName)) &&
      // живого моба так не перешагнуть: за ним очередь
      assertTrue(b.copy(group = b.group.copy(places = List(14, 15, 13)))
        .stepTowardsHero(14).isEmpty)
    },

    test("разрыв в строю: задние спускаются вниз, а на места башен не встают") {
      // Герой на первом, мобы на 2–5, пусто на 6–13, башни на 14–15, за ними
      // пятеро на 16–20. Десять мобов — это потолок строя.
      val mob   = monster(Race.Orc, 100L)
      val tower = monster(Race.Construct, 500L, Rarity.Rare)
      // Строй: активный на втором, трое за ним, две башни и четверо позади них.
      val all   = List(mob) ++ List.fill(3)(mob) ++ List(tower, tower) ++ List.fill(4)(mob)
      val b0    = SoloPveBattle.fromGroup(all, hero, Nil)
      val start = b0.copy(group = b0.group.copy(
        heroPos = 1, activePos = 2,
        places = List(3, 4, 5, 14, 15, 16, 17, 18, 19)))
      val towerPlaces = Set(14, 15)
      // Крутим раунды и смотрим, как строй смыкается.
      val steps = Iterator.iterate(start)(_.closeIn._1).take(20).toList
      val last  = steps.last
      def mobPlaces(b: SoloPveBattle): List[Int] =
        (b.group.activePos :: b.group.places.zip(b.group.others)
          .collect { case (p, s) if s.race != Race.Construct.entryName => p }).sorted
      assertTrue(steps.forall(b => b.group.places.zip(b.group.others)
        .forall { case (p, s) => s.race == Race.Construct.entryName || !towerPlaces.contains(p) })) &&
      // башни никуда не делись и стоят там же
      assertTrue(steps.forall(b => b.group.places.zip(b.group.others)
        .collect { case (p, s) if s.race == Race.Construct.entryName => p }.sorted == List(14, 15))) &&
      // задние спустились и встали вплотную за передними, без дыр
      assertTrue(mobPlaces(last) == List(2, 3, 4, 5, 6, 7, 8, 9)) &&
      // и дальше строй стоит: ближние уже достают героя
      assertTrue(last.closeIn._1.group.places.sorted == last.group.places.sorted)
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
