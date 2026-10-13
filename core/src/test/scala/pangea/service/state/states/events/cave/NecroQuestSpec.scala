package pangea.service.state.states.events.cave

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.cave.{CaveGenerator, CaveRates, CaveRoom, CaveScene, RoomKind, SewerRates}
import pangea.model.hero.Hero
import pangea.model.item.{Item, MaterialKind}
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity}
import pangea.model.quest.{BoardData, BoardKind, BoardRates, BoardSlot, Difficulty}
import pangea.model.stats.FightStats
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.road.{QuestRoadState, RoadProgress}
import pangea.test._
import zio.test._
import zio.{Duration, Task, ZIO}

/** «Уничтожение некроманта»: задание с доски, дорога до места, пещера, где вся
  * родня одной расы поднята, и сам Некромант на нижнем ярусе. Механика та же,
  * что у канализации, — здесь проверяется то, чем пещера некроманта от неё
  * отличается. И кости на алтаре: плита раскалывается, встаёт Некромант. */
object NecroQuestSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def content = ZIO.attempt(SceneContent.load())

  private def hero(lvl: Long = 40L): Hero =
    TestFixtures.hero(userId, dungeonLevel = 30).copy(lvl = lvl)

  private def cave(h: Hero = hero(), items: List[Item] = Nil) =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      inv    = TestInventoryRepository.withItems(items)
      sched <- TestScheduler.make
      r     <- TestRenderer.make
      c     <- content
    } yield (MonsterCaveState(dao, inv, TestItemRepository.make, sched, c, None), dao, inv, r)

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  private def sceneOf(dao: TestHeroDao): Task[Option[CaveScene]] =
    dao.readSceneData(userId).map(_.flatMap(_.as[CaveScene].toOption))

  private def battleOf(dao: TestHeroDao): Task[Option[SoloPveBattle]] =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  private def lootOf(dao: TestHeroDao): Task[Option[LootData]] =
    dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))

  private def boardOf(dao: TestHeroDao): Task[BoardData] =
    dao.readQuestData(userId).map(_.flatMap(_.as[BoardData].toOption).getOrElse(BoardData.empty))

  /** Пещера некроманта на три комнаты: вход, к северу — мертвецы, к востоку —
    * то, что задано. Раса — орки. */
  private def smallNecro(
    monsters: Int      = 3,
    kind:     RoomKind = RoomKind.Stairs,
    questLvl: Long     = 7L,
    floor:    Int      = 1
  ): CaveScene =
    CaveScene(
      race  = Race.Orc.entryName,
      rooms = List(
        CaveRoom(0, 0, 0, RoomKind.Empty, done = true),
        CaveRoom(0, 1, monsters, RoomKind.Empty, done = true),
        CaveRoom(1, 0, 0, kind)),
      at = 0, inside = true, necro = true, questLvl = questLvl, floor = floor,
      boss = Option.when(floor == 2)("Necromancer"))

  /** Доска, на которой взяты и некромант, и канализация, и пещера. */
  private def allTaken: BoardData =
    BoardData(0L, "novice", List(
      BoardSlot(BoardKind.NecromancerHunt, taken = true, lvl = 7L),
      BoardSlot(BoardKind.SewerRats, taken = true, lvl = 7L),
      BoardSlot(BoardKind.CaveClear, taken = true)))

  /** Связность: от входа обходом по соседним клеткам достижимы все комнаты. */
  private def reachable(s: CaveScene): Int = {
    val cells = s.rooms.map(r => (r.x, r.y)).toSet
    @annotation.tailrec
    def walk(seen: Set[(Int, Int)], front: List[(Int, Int)]): Set[(Int, Int)] = front match {
      case Nil => seen
      case (x, y) :: rest =>
        val next = List((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y))
          .filter(c => cells.contains(c) && !seen.contains(c))
        walk(seen ++ next, rest ++ next)
    }
    walk(Set((0, 0)), List((0, 0))).size
  }

  private def bones(id: Long): Item = MaterialGenerator.item(MaterialKind.CursedBones).copy(id = id)

  override def spec = suite("Уничтожение некроманта")(

    // ── Объявление ───────────────────────────────────────────────────────────

    test("объявление: выездное, из пула доски, со своей сложностью и своими словами") {
      for {
        c <- content
      } yield assertTrue(BoardKind.NecromancerHunt.away && BoardKind.NecromancerHunt.rolledLvl) &&
              assertTrue(BoardSlot(BoardKind.NecromancerHunt, lvl = 17L).difficulty == 17) &&
              assertTrue(BoardRates.Pool.contains(BoardKind.NecromancerHunt)) &&
              assertTrue(c.text("questBoard.ask.necro").contains("некроманта")) &&
              assertTrue(c.text("questBoard.doneNecro").nonEmpty)
    },

    // ── Генерация ────────────────────────────────────────────────────────────

    test("верхний ярус: как в канализации — алтарь всегда, ход вниз один, — но живых здесь нет") {
      val floors = (1L to 200L).toList.map(s => CaveGenerator.necroCave(9L, Race.Orc.entryName, Rng(s))._1)
      assertTrue(floors.forall(f => f.necro && !f.sewer && f.away && f.questLvl == 9L && f.floor == 1)) &&
      assertTrue(floors.forall(f => f.race == Race.Orc.entryName && f.undeadMobs && !f.lastFloor)) &&
      assertTrue(floors.forall(f => f.rooms.size >= CaveRates.MinRooms && f.rooms.size <= CaveRates.MaxRooms)) &&
      assertTrue(floors.forall(f => reachable(f) == f.rooms.size)) &&
      assertTrue(floors.forall(_.rooms.count(_.kind == RoomKind.Altar) == 1)) &&
      assertTrue(floors.forall(_.rooms.count(_.kind == RoomKind.Stairs) == 1)) &&
      assertTrue(floors.forall(_.rooms.count(_.kind == RoomKind.Rest) == 1)) &&
      assertTrue(floors.forall(_.rooms.head.monsters == 0)) &&
      // хозяина наверху нет — он ждёт внизу
      assertTrue(floors.forall(f => f.boss.isEmpty && !f.rooms.exists(_.kind == RoomKind.Lair)))
    },

    test("нижний ярус: логово самого Некроманта, привала и хода дальше нет") {
      val deep = (1L to 200L).toList.map(s => CaveGenerator.necroDeep(9L, Race.Orc.entryName, Rng(s))._1)
      assertTrue(deep.forall(d => d.necro && d.inside && d.floor == SewerRates.Floors && d.lastFloor)) &&
      assertTrue(deep.forall(d => d.race == Race.Orc.entryName && d.boss.contains("Necromancer"))) &&
      assertTrue(deep.forall(d => d.rooms.size >= SewerRates.DeepMinRooms && d.rooms.size <= SewerRates.DeepMaxRooms)) &&
      assertTrue(deep.forall(d => reachable(d) == d.rooms.size)) &&
      assertTrue(deep.forall(_.rooms.count(_.kind == RoomKind.Lair) == 1)) &&
      assertTrue(deep.forall(_.rooms.find(_.kind == RoomKind.Lair).exists(_.monsters == 1))) &&
      assertTrue(deep.forall(!_.cleared)) &&
      assertTrue(deep.forall(d => !d.rooms.exists(r => r.kind == RoomKind.Rest || r.kind == RoomKind.Stairs)))
    },

    // ── Дорога и порог ───────────────────────────────────────────────────────

    test("дорога приводит в пещеру некроманта: раса смертная, уровень — задания") {
      for {
        dao   <- TestHeroDao.withHero(userId, hero())
        sched <- TestScheduler.make
        r     <- TestRenderer.make
        c     <- content
        state  = QuestRoadState(dao, sched, c)
        _     <- dao.writeSceneData(userId, RoadProgress(0L, BoardKind.NecromancerHunt, 12L).asJson)
        _     <- TestClock.adjust(Duration.fromMillis(SewerRates.RoadMs + 1000L))
        out   <- state.action(testUser, tap("RoadDone"), r)
        scene <- sceneOf(dao)
      } yield assertTrue(out == StateType.MonsterCave) &&
              assertTrue(scene.exists(s => s.necro && !s.sewer && s.questLvl == 12L && s.floor == 1 && !s.inside)) &&
              assertTrue(scene.exists(s => Race.mortals.map(_.entryName).contains(s.race)))
    },

    test("порог: по костям видно, чья родня поднята, и сложность знаками") {
      for {
        t <- cave()
        (state, dao, _, r) = t
        _    <- dao.writeSceneData(userId, smallNecro().copy(inside = false).asJson)
        _    <- state.enter(testUser, r)
        gate <- r.sentScreens.map(_.last)
      } yield assertTrue(gate.text.contains(Difficulty.render(7)) && gate.text.contains("оврага")) &&
              assertTrue(gate.text.contains(Race.Orc.genitivePlural)) &&
              assertTrue(gate.choices.exists(c => c.id == "CaveEnter" && c.label == "Войти"))
    },

    // ── Внутри ───────────────────────────────────────────────────────────────

    test("в бой идут мертвецы одной расы уровнем в задание, и добыча — по нему же") {
      for {
        t <- cave()
        (state, dao, _, r) = t
        _      <- dao.writeSceneData(userId, smallNecro(monsters = 3, questLvl = 7L).asJson)
        next   <- state.action(testUser, tap("CaveForward"), r)
        battle <- battleOf(dao)
        said   <- texts(r)
      } yield assertTrue(next == StateType.Battle) &&
              assertTrue(battle.exists(b => b.monsterRace == Race.Orc.entryName && b.monsterUndead)) &&
              assertTrue(battle.exists(b => b.group.others.forall(s => s.undead && s.race == Race.Orc.entryName))) &&
              assertTrue(battle.exists(b => b.monsterLvl == 7L && b.lootLevel.contains(7L))) &&
              // тиры — как у крыс в канализации: второй и третий
              assertTrue(battle.exists(b => (b.rarity :: b.group.others.map(s => Rarity.withName(s.rarity)))
                .forall(rr => rr == Rarity.Uncommon || rr == Rarity.Rare))) &&
              assertTrue(said.contains("Из темноты поднимаются мертвецы"))
    },

    test("ход вниз ведёт к логову Некроманта и уносит туда взятый опыт") {
      for {
        t <- cave()
        (state, dao, _, r) = t
        _     <- dao.writeSceneData(userId,
                   smallNecro(monsters = 0, kind = RoomKind.Stairs).copy(at = 2, expEarned = 40L).asJson)
        _     <- state.enter(testUser, r)
        room  <- r.sentScreens.map(_.last)
        down  <- state.action(testUser, tap("CaveDown"), r)
        deep  <- sceneOf(dao)
        said  <- texts(r)
      } yield assertTrue(room.text.contains("вырублены ступени")) &&
              assertTrue(room.choices.exists(_.id == "CaveDown")) &&
              assertTrue(down == StateType.MonsterCave && said.contains("сложены вдоль стен")) &&
              assertTrue(deep.exists(d => d.necro && d.floor == 2 && d.questLvl == 7L && d.race == Race.Orc.entryName)) &&
              assertTrue(deep.exists(d => d.boss.contains("Necromancer") && d.expEarned == 40L && d.restUsed))
    },

    test("в логове — Некромант со свитой той же расы, свита уровнем в задание") {
      for {
        t <- cave(hero(lvl = 41L))
        (state, dao, _, r) = t
        _      <- dao.writeSceneData(userId, smallNecro(monsters = 0, kind = RoomKind.Empty, floor = 2)
                    .copy(rooms = List(
                      CaveRoom(0, 0, 0, RoomKind.Empty, done = true),
                      CaveRoom(0, 1, 1, RoomKind.Lair))).asJson)
        out    <- state.action(testUser, tap("CaveForward"), r)
        battle <- battleOf(dao)
        said   <- texts(r)
        back   <- lootOf(dao).map(_.flatMap(_.eventData).flatMap(_.as[CaveScene].toOption))
      } yield assertTrue(out == StateType.Battle && said.contains("Мертвецы вокруг поднимают")) &&
              assertTrue(battle.exists(_.boss.contains(MiniBoss.Necromancer))) &&
              assertTrue(battle.exists(_.monsterLvl == MiniBoss.Necromancer.bossLvl(41L))) &&
              assertTrue(battle.exists(b => b.group.others.size >= MiniBoss.Necromancer.GuardMin &&
                b.group.others.size <= MiniBoss.Necromancer.GuardMax)) &&
              assertTrue(battle.exists(b => b.group.others.forall(s => s.undead && s.race == Race.Orc.entryName && s.lvl == 7L))) &&
              assertTrue(battle.exists(b => b.minionLvl == 7L && b.lootLevel.contains(7L))) &&
              // вернёмся в ту же пещеру, с выбитым логовом
              assertTrue(back.exists(s => s.necro && s.floor == 2 && s.cleared))
    },

    test("верхний ярус задание не закрывает; выбитый нижний закрывает «некроманта», а не соседей") {
      for {
        t <- cave()
        (state, dao, _, r) = t
        _      <- dao.writeQuestData(userId, allTaken.asJson)
        _      <- dao.writeSceneData(userId, smallNecro(monsters = 0).copy(expEarned = 50L).asJson)
        _      <- state.enter(testUser, r)
        upper  <- texts(r)
        board1 <- boardOf(dao)
        _      <- dao.writeSceneData(userId,
                    smallNecro(monsters = 0, kind = RoomKind.Empty, floor = 2).copy(expEarned = 50L).asJson)
        _      <- state.enter(testUser, r)
        said   <- texts(r)
        board2 <- boardOf(dao)
        after  <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(upper.contains("Этот ярус упокоен") && board1.slots.forall(!_.done)) &&
              assertTrue(said.contains("+80 опыта") && after.exp == 80L) &&
              assertTrue(said.contains("Некроманта больше нет") && said.contains("больше никто не встаёт")) &&
              assertTrue(board2.slots.find(_.kind == BoardKind.NecromancerHunt).exists(_.done)) &&
              assertTrue(board2.slots.filterNot(_.kind == BoardKind.NecromancerHunt).forall(!_.done))
    },

    test("из пещеры некроманта уходят в город, и объявление на этом проваливается") {
      for {
        t <- cave()
        (state, dao, _, r) = t
        _     <- dao.writeQuestData(userId, allTaken.asJson)
        _     <- dao.writeSceneData(userId, smallNecro().asJson)
        ask   <- state.action(testUser, tap("CaveOut"), r)
        said  <- texts(r)
        out   <- state.action(testUser, tap("CaveOutYes"), r)
        left  <- texts(r)
        after <- dao.readSceneData(userId)
        board <- boardOf(dao)
      } yield assertTrue(ask == StateType.MonsterCave && said.contains("некромант поднимет снова")) &&
              assertTrue(out == StateType.GlobalMap && after.contains(io.circe.Json.Null)) &&
              assertTrue(left.contains("остаётся своим мертвецам")) &&
              assertTrue(board.slots.map(_.kind) == List(BoardKind.SewerRats, BoardKind.CaveClear))
    },

    test("бегство из боя в пещере некроманта проваливает её объявление, а не канализацию") {
      val monster = Monster(0L, 7L, Race.Orc, Rarity.Common,
        FightStats(atk = 1, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0)).copy(undead = true)
      val tough = hero().copy(fightStats = FightStats(atk = 10, hp = 100000, armor = 0, defence = 0,
        evasion = 9999, accuracy = 9999, energy = 0))
      for {
        dao <- TestHeroDao.withHero(userId, tough)
        r   <- TestRenderer.make
        c   <- content
        _   <- dao.writeActiveBattle(userId, SoloPveBattle.from(monster, tough).asJson)
        _   <- dao.writeQuestData(userId, allTaken.asJson)
        _   <- dao.writeSceneData(userId, LootData(Nil, Nil, returnState = Some(StateType.MonsterCave),
                 eventData = Some(smallNecro().asJson)).asJson)
        state = pangea.service.state.states.battle.BattleState(
                  dao, TestInventoryRepository.accepting, TestItemRepository.make, c)
        out   <- state.action(testUser, tap("ConfirmFlee"), r)
        board <- boardOf(dao)
        said  <- texts(r)
        scene <- dao.readSceneData(userId)
      } yield assertTrue(out == StateType.Dungeon) &&
              assertTrue(board.slots.map(_.kind) == List(BoardKind.SewerRats, BoardKind.CaveClear)) &&
              assertTrue(said.contains("задание провалено")) &&
              assertTrue(scene.contains(io.circe.Json.Null))
    },

    test("гибель в пещере некроманта тоже проваливает её объявление") {
      val brute = Monster(0L, 7L, Race.Orc, Rarity.Common,
        FightStats(atk = 99999, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 99999, energy = 0))
        .copy(undead = true)
      val frail = hero().copy(
        baseStats  = hero().baseStats.copy(agi = 0),
        fightStats = FightStats(atk = 10, hp = 1, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))
      for {
        dao <- TestHeroDao.withHero(userId, frail)
        r   <- TestRenderer.make
        c   <- content
        _   <- dao.writeActiveBattle(userId, SoloPveBattle.from(brute, frail).asJson)
        _   <- dao.writeQuestData(userId, allTaken.asJson)
        _   <- dao.writeSceneData(userId, LootData(Nil, Nil, returnState = Some(StateType.MonsterCave),
                 eventData = Some(smallNecro().asJson)).asJson)
        state = pangea.service.state.states.battle.BattleState(
                  dao, TestInventoryRepository.accepting, TestItemRepository.make, c)
        // Уклонение не бывает ниже 5%: бьём, пока бой не кончится.
        out <- {
                 def attack(left: Int): Task[StateType] =
                   state.action(testUser, tap("Attack"), r).flatMap { st =>
                     if (st == StateType.Battle && left > 1) attack(left - 1) else ZIO.succeed(st)
                   }
                 attack(10)
               }
        board <- boardOf(dao)
        said  <- texts(r)
      } yield assertTrue(out == StateType.Death) &&
              assertTrue(board.slots.map(_.kind) == List(BoardKind.SewerRats, BoardKind.CaveClear)) &&
              assertTrue(said.contains("задание провалено"))
    },

    test("у пещеры некроманта свои слова там, где они ей нужны") {
      val lists = Set("walls", "rooms.empty")
      for {
        c <- content
      } yield assertTrue(MonsterCaveState.NecroSays.filterNot(lists.contains)
                .forall(n => c.text(s"cave.$n").nonEmpty && c.text(s"necro.$n").nonEmpty)) &&
              assertTrue(c.list("necro.walls").size == c.list("cave.walls").size) &&
              assertTrue(c.list("necro.rooms.empty").size == c.list("cave.rooms.empty").size) &&
              // слова только выездных мест — и у канализации, и у пещеры некроманта
              assertTrue(MonsterCaveState.AwayOnly.forall(n => c.text(s"sewer.$n").nonEmpty && c.text(s"necro.$n").nonEmpty)) &&
              assertTrue(MonsterCaveState.questOf(smallNecro()) == BoardKind.NecromancerHunt) &&
              assertTrue(MonsterCaveState.questOf(smallNecro().copy(necro = false, sewer = true)) == BoardKind.SewerRats) &&
              assertTrue(MonsterCaveState.questOf(smallNecro().copy(necro = false)) == BoardKind.CaveClear) &&
              assertTrue(MonsterCaveState.doneKey(BoardKind.NecromancerHunt) == "questBoard.doneNecro")
    },

    // ── Кости на алтаре ──────────────────────────────────────────────────────

    test("проклятые кости раскалывают алтарь, и из разлома встаёт Некромант со свитой расы пещеры") {
      // Пещера гоблинов из лабиринта: герой стоит у алтаря.
      val scene = CaveScene(Race.Goblin.entryName, List(
        CaveRoom(0, 0, 0, RoomKind.Empty, done = true),
        CaveRoom(1, 0, 0, RoomKind.Altar)), at = 1, inside = true)
      for {
        t <- cave(hero(lvl = 41L), items = List(bones(51L)))
        (state, dao, inv, r) = t
        _      <- dao.writeSceneData(userId, scene.asJson)
        _      <- state.action(testUser, tap("CaveAltar"), r)
        list   <- r.sentScreens.map(_.last)
        out    <- state.action(testUser, tap("CaveUse_51"), r)
        said   <- texts(r)
        battle <- battleOf(dao)
        back   <- lootOf(dao)
        next    = back.flatMap(_.eventData).flatMap(_.as[CaveScene].toOption)
      } yield assertTrue(list.choices.map(_.id).contains("CaveUse_51")) &&
              assertTrue(out == StateType.Battle && said.contains("раскалывается пополам")) &&
              // кости ушли на камень
              assertTrue(inv.snapshot.isEmpty) &&
              assertTrue(battle.exists(_.boss.contains(MiniBoss.Necromancer))) &&
              assertTrue(battle.exists(_.monsterLvl == MiniBoss.Necromancer.bossLvl(41L))) &&
              // свита — мертвецы той расы, чья пещера, уровнем в этаж лабиринта
              assertTrue(battle.exists(b => b.group.others.nonEmpty &&
                b.group.others.forall(s => s.undead && s.race == Race.Goblin.entryName && s.lvl == 30L))) &&
              assertTrue(battle.exists(_.lootLevel.isEmpty)) &&
              // из боя — обратно в пещеру, к расколотой плите
              assertTrue(back.exists(_.returnState.contains(StateType.MonsterCave))) &&
              assertTrue(next.exists(s => s.altarBroken && s.altarSpent))
    },

    test("расколотая плита больше ничего не берёт и так и выглядит") {
      val scene = CaveScene(Race.Goblin.entryName, List(
        CaveRoom(0, 0, 0, RoomKind.Empty, done = true),
        CaveRoom(1, 0, 0, RoomKind.Altar)), at = 1, inside = true, altarSpent = true, altarBroken = true)
      for {
        t <- cave(items = List(bones(52L)))
        (state, dao, inv, r) = t
        _    <- dao.writeSceneData(userId, scene.asJson)
        _    <- state.enter(testUser, r)
        room <- r.sentScreens.map(_.last)
      } yield assertTrue(room.text.contains("Расколотая пополам плита")) &&
              assertTrue(!room.choices.map(_.id).contains("CaveAltar")) &&
              assertTrue(inv.snapshot.map(_.id) == List(52L))
    },

    test("в канализации свита — крысы уровнем в задание, и добыча по нему же") {
      val sewer = CaveScene(Race.Animal.entryName, List(
        CaveRoom(0, 0, 0, RoomKind.Empty, done = true),
        CaveRoom(1, 0, 0, RoomKind.Altar)), at = 1, inside = true, sewer = true, questLvl = 7L)
      for {
        t <- cave(items = List(bones(53L)))
        (state, dao, _, r) = t
        _      <- dao.writeSceneData(userId, sewer.asJson)
        _      <- state.action(testUser, tap("CaveAltar"), r)
        _      <- state.action(testUser, tap("CaveUse_53"), r)
        battle <- battleOf(dao)
      } yield assertTrue(battle.exists(_.boss.contains(MiniBoss.Necromancer))) &&
              assertTrue(battle.exists(b => b.group.others.forall(s => s.undead && s.race == Race.Animal.entryName && s.lvl == 7L))) &&
              assertTrue(battle.exists(b => b.minionLvl == 7L && b.lootLevel.contains(7L)))
    }
  )
}
