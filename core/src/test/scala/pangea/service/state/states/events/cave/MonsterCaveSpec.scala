package pangea.service.state.states.events.cave

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.{ChoiceColor, SceneContent}
import pangea.generator.item.{FlaskGenerator, MaterialGenerator}
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.cave.{CaveGenerator, CaveRates, CaveRoom, CaveScene, RoomKind}
import pangea.model.hero.Hero
import pangea.model.item.{BrewKind, FlaskKind, Item, ItemDetails, MaterialKind, Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.LootState.LootData
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** Пещера с монстрами: порог с вещами, комнаты на сетке, кучки мобов и награда
  * за зачистку. */
object MonsterCaveSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(dungeonLevel: Int = 10): Hero =
    TestFixtures.hero(userId, dungeonLevel = dungeonLevel)

  private def content = ZIO.attempt(SceneContent.load())

  private def cave(h: Hero = hero(), items: List[Item] = Nil) =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      inv    = TestInventoryRepository.withItems(items)
      sched <- TestScheduler.make
      r     <- TestRenderer.make
      c     <- content
    } yield (MonsterCaveState(dao, inv, TestItemRepository.make, sched, c), dao, inv, sched, r)

  /** Пещерка на три комнаты: вход (0,0), к северу — мобы, к востоку — находка. */
  private def smallCave(
    monsters: Int      = 3,
    kind:     RoomKind = RoomKind.Chest,
    inside:   Boolean  = true,
    weakened: Boolean  = false,
    poisoned: Boolean  = false
  ): CaveScene =
    CaveScene(
      race   = Race.Goblin.entryName,
      rooms  = List(
        CaveRoom(0, 0, 0, RoomKind.Empty, done = true),
        CaveRoom(0, 1, monsters, RoomKind.Empty, done = true),
        CaveRoom(1, 0, 0, kind)),
      at     = 0,
      inside = inside,
      weakened = weakened,
      poisoned = poisoned)

  private def put(dao: TestHeroDao, scene: CaveScene): Task[Unit] =
    dao.writeSceneData(userId, scene.asJson)

  private def sceneOf(dao: TestHeroDao): Task[Option[CaveScene]] =
    dao.readSceneData(userId).map(_.flatMap(_.as[CaveScene].toOption))

  private def lootOf(dao: TestHeroDao): Task[Option[LootData]] =
    dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))

  private def battleOf(dao: TestHeroDao): Task[Option[SoloPveBattle]] =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  private def brew(kind: BrewKind, id: Long): Item = BrewKind.item(kind).copy(id = id)

  override def spec = suite("Пещера с монстрами")(

    test("в пуле событий 1%, забранный у боя; билеты дальше по списку не сдвинулись") {
      val ev = StateType.events
      assertTrue(ev.count(_ == StateType.MonsterCave) == 1) &&
      assertTrue(ev.count(_ == StateType.Battle) == 37 && ev.size == 100) &&
      assertTrue(ev(37) == StateType.MonsterCave && ev(38) == StateType.FlowerMeadow) &&
      assertTrue(ev(67) == StateType.Spring && ev(99) == StateType.ElementalLair)
    },

    test("пещера: 10–20 связанных комнат, 15–30 мобов кучками по 3–5, ровно один привал") {
      val caves = (1L to 200L).toList.map(seed => CaveGenerator.generate(Race.Orc.entryName, Rng(seed))._1)
      // Связность: от входа обходом по соседним клеткам достижимы все комнаты.
      def reachable(s: CaveScene): Int = {
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
      assertTrue(caves.forall(c => c.rooms.size >= CaveRates.MinRooms && c.rooms.size <= CaveRates.MaxRooms)) &&
      assertTrue(caves.forall(c => c.rooms.map(r => (r.x, r.y)).distinct.size == c.rooms.size)) &&
      assertTrue(caves.forall(c => reachable(c) == c.rooms.size)) &&
      assertTrue(caves.forall { c =>
        val total = c.rooms.map(_.monsters).sum
        total >= CaveRates.MinMonsters && total <= CaveRates.MaxMonsters
      }) &&
      // Кучками по 3–5, и на пороге не бьют: во входной комнате пусто.
      assertTrue(caves.forall(c => c.rooms.map(_.monsters).filter(_ > 0).forall(n => n >= 3 && n <= 5))) &&
      assertTrue(caves.forall(_.rooms.head.monsters == 0)) &&
      assertTrue(caves.forall(_.rooms.count(_.kind == RoomKind.Rest) == 1)) &&
      // Пещера пустой не бывает: мобы есть всегда.
      assertTrue(caves.forall(c => !c.cleared))
    },

    test("редкости пещеры: легендарных нет, четвёртый тир — 40%") {
      val pool = CaveRates.RarityPool
      assertTrue(pool.size == 100 && !pool.contains(Rarity.Legendary)) &&
      assertTrue(pool.count(_ == Rarity.Mythical) == 40 && pool.count(_ == Rarity.Rare) == 29) &&
      assertTrue(pool.count(_ == Rarity.Uncommon) == 30 && pool.count(_ == Rarity.Common) == 1)
    },

    test("порог: видно расу, но не число мобов; «Войти» ведёт внутрь") {
      for {
        t <- cave()
        (state, dao, _, _, r) = t
        _      <- TestRandom.feedInts(3) *> TestRandom.feedLongs(42L)
        _      <- state.enter(testUser, r)
        gate   <- r.sentScreens.map(_.last)
        inside <- state.action(testUser, tap("CaveEnter"), r)
        room   <- r.sentScreens.map(_.last)
        scene  <- sceneOf(dao)
      } yield assertTrue(gate.choices.map(_.id) == List("CaveEnter", "CaveSupply", "OpenCharacter", "CaveOut")) &&
              assertTrue(!gate.text.contains("15") && !gate.text.contains("30")) &&
              assertTrue(inside == StateType.MonsterCave && scene.exists(_.inside)) &&
              assertTrue(room.choices.map(_.id).startsWith(List("CaveForward", "CaveLeft", "CaveRight", "CaveBack")))
    },

    test("направления: зелёные там, где ход, красные в камень; в стену не ходят") {
      for {
        t <- cave()
        (state, dao, _, _, r) = t
        _     <- put(dao, smallCave())
        _     <- state.enter(testUser, r)
        room  <- r.sentScreens.map(_.last)
        byId   = room.choices.map(c => c.id -> c.color).toMap
        stay  <- state.action(testUser, tap("CaveLeft"), r)
        wall  <- texts(r)
        scene <- sceneOf(dao)
      } yield assertTrue(byId("CaveForward") == ChoiceColor.Positive && byId("CaveRight") == ChoiceColor.Positive) &&
              assertTrue(byId("CaveLeft") == ChoiceColor.Negative && byId("CaveBack") == ChoiceColor.Negative) &&
              assertTrue(stay == StateType.MonsterCave && wall.contains("глухая стена")) &&
              assertTrue(scene.exists(_.at == 0))
    },

    test("комната с мобами: групповой бой на всю кучку, добыча вернёт в пещеру") {
      for {
        t <- cave()
        (state, dao, _, _, r) = t
        _      <- put(dao, smallCave(monsters = 4))
        next   <- state.action(testUser, tap("CaveForward"), r)
        battle <- battleOf(dao)
        loot   <- lootOf(dao)
        back    = loot.flatMap(_.eventData).flatMap(_.as[CaveScene].toOption)
      } yield assertTrue(next == StateType.Battle) &&
              assertTrue(battle.exists(b => b.group.others.size == 3 && b.monsterRace == Race.Goblin.entryName)) &&
              assertTrue(battle.exists(b => (b.rarity :: b.group.others.map(s => Rarity.withName(s.rarity)))
                .forall(_ != Rarity.Legendary))) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.MonsterCave))) &&
              // комната зачищена заранее: вернуться из боя можно только победив
              assertTrue(back.exists(s => s.rooms(1).monsters == 0 && s.at == 1 && s.expEarned > 0L))
    },

    test("сонный дурман срезает атаку и энергию мобов, сам уходит из сумки") {
      val dope = brew(BrewKind.SleepingDope, 7L)
      for {
        t <- cave(items = List(dope))
        (state, dao, inv, _, r) = t
        _      <- put(dao, smallCave(inside = false))
        _      <- state.action(testUser, tap("CaveSupply"), r)
        list   <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("CaveUse_7"), r)
        said   <- texts(r)
        scene  <- sceneOf(dao)
        _      <- put(dao, scene.get.copy(inside = true))
        _      <- state.action(testUser, tap("CaveForward"), r)
        battle <- battleOf(dao)
        base    = MonsterGenerator.generateOfRaceAndRarity(10, Race.Goblin, battle.get.rarity).fightStats
      } yield assertTrue(list.choices.map(_.id).contains("CaveUse_7")) &&
              assertTrue(said.contains("дурью") && scene.exists(_.weakened)) &&
              assertTrue(inv.snapshot.isEmpty) &&
              assertTrue(battle.exists(_.monsterStats.atk == base.atk * 80L / 100L)) &&
              assertTrue(battle.exists(_.monsterStats.energy == base.energy * 80L / 100L))
    },

    test("дымная фляга работает так же, но тратит один заряд — и надетая тоже") {
      val flask = FlaskGenerator.item(FlaskKind.Smoke, ItemRarity.Blue).copy(id = 5L)
      val armed = hero().copy(equipment = TestFixtures.emptyEquipment.copy(flask = flask))
      for {
        t <- cave(armed)
        (state, dao, _, _, r) = t
        _     <- put(dao, smallCave(inside = false))
        _     <- state.action(testUser, tap("CaveSupply"), r)
        _     <- state.action(testUser, tap("CaveUse_5"), r)
        scene <- sceneOf(dao)
        after <- dao.getHeroByUserId(userId).map(_.get.equipment.flask.details)
      } yield assertTrue(scene.exists(_.weakened)) &&
              assertTrue(after match {
                case ItemDetails.Flask(_, charges, max) => charges == max - 1
                case _                                  => false
              })
    },

    test("глефовый гриб: вся пещера встречает героя отравленной") {
      val mushroom = MaterialGenerator.item(MaterialKind.GlaiveMushroom).copy(id = 9L)
      for {
        t <- cave(items = List(mushroom))
        (state, dao, inv, _, r) = t
        _      <- put(dao, smallCave(inside = false))
        _      <- state.action(testUser, tap("CaveSupply"), r)
        _      <- state.action(testUser, tap("CaveUse_9"), r)
        scene  <- sceneOf(dao)
        _      <- put(dao, scene.get.copy(inside = true))
        _      <- state.action(testUser, tap("CaveForward"), r)
        battle <- battleOf(dao)
      } yield assertTrue(scene.exists(_.poisoned) && inv.snapshot.isEmpty) &&
              assertTrue(battle.exists(_.effects.monsterPoison.isDefined)) &&
              assertTrue(battle.exists(_.group.others.forall(_.effects.monsterPoison.isDefined)))
    },

    test("бесполезная вещь тратится впустую, сюжетную пещера не берёт") {
      val junk  = brew(BrewKind.Schnapps, 3L)
      val quest = pangea.model.item.QuestItemKind.item(pangea.model.item.QuestItemKind.MarisaLetter).copy(id = 4L)
      for {
        t <- cave(items = List(junk, quest))
        (state, dao, inv, _, r) = t
        _     <- put(dao, smallCave(inside = false))
        _     <- state.action(testUser, tap("CaveSupply"), r)
        _     <- state.action(testUser, tap("CaveUse_3"), r)
        wasted <- texts(r)
        _     <- state.action(testUser, tap("CaveUse_4"), r)
        kept  <- texts(r)
        scene <- sceneOf(dao)
      } yield assertTrue(wasted.contains("нет никакого дела") && kept.contains("не годится")) &&
              assertTrue(inv.snapshot.map(_.id) == List(4L)) &&
              assertTrue(scene.exists(s => !s.weakened && !s.poisoned))
    },

    test("сундук и схрон отдают добычу через общий экран и возвращают в пещеру") {
      for {
        t <- cave()
        (state, dao, _, _, r) = t
        _      <- put(dao, smallCave(kind = RoomKind.Chest).copy(at = 2))
        chest  <- state.action(testUser, tap("CaveSearch"), r)
        loot   <- lootOf(dao)
        back    = loot.flatMap(_.eventData).flatMap(_.as[CaveScene].toOption)
        _      <- put(dao, smallCave(kind = RoomKind.Stash).copy(at = 2))
        stash  <- state.action(testUser, tap("CaveSearch"), r)
        loot2  <- lootOf(dao)
      } yield assertTrue(chest == StateType.Loot && stash == StateType.Loot) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.MonsterCave))) &&
              assertTrue(loot2.exists(_.returnState.contains(StateType.MonsterCave))) &&
              // обысканная комната второй раз ничего не даст
              assertTrue(back.exists(_.rooms(2).done))
    },

    test("трава в трещине достаётся сразу, комната после этого пуста") {
      for {
        t <- cave()
        (state, dao, inv, _, r) = t
        _     <- put(dao, smallCave(kind = RoomKind.Herb).copy(at = 2))
        next  <- state.action(testUser, tap("CaveSearch"), r)
        said  <- texts(r)
        scene <- sceneOf(dao)
      } yield assertTrue(next == StateType.MonsterCave && said.contains("Вы сорвали")) &&
              assertTrue(inv.snapshot.exists(_.material.isDefined)) &&
              assertTrue(scene.exists(_.rooms(2).done))
    },

    test("привал: полминуты, один раз за пещеру, поднимает и героя, и отряд") {
      val tired = hero().copy(fightStats = hero().fightStats.copy(hp = 1L, energy = 0L))
      for {
        t <- cave(tired)
        (state, dao, _, sched, r) = t
        _      <- put(dao, smallCave(kind = RoomKind.Rest).copy(at = 2))
        _      <- state.enter(testUser, r)
        screen <- r.sentScreens.map(_.last)
        rest   <- state.action(testUser, tap("CaveRest"), r)
        tasks  <- sched.scheduled
        asleep <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("CaveRested"), r)
        after  <- dao.getHeroByUserId(userId).map(_.get)
        scene  <- sceneOf(dao)
        again  <- state.action(testUser, tap("CaveRest"), r)
        room   <- r.sentScreens.map(_.last)
      } yield assertTrue(screen.choices.map(_.id).contains("CaveRest")) &&
              assertTrue(rest == StateType.MonsterCave && asleep.hideKeyboard) &&
              assertTrue(tasks.exists(x => x.kind == TaskKind.CaveRest && x.expectedState == StateType.MonsterCave)) &&
              assertTrue(after.fightStats.hp > 1L && after.fightStats.energy > 0L) &&
              assertTrue(scene.exists(s => s.restUsed && s.restUntil == 0L)) &&
              // второй раз тот же угол не выручит: кнопки уже нет
              assertTrue(again == StateType.MonsterCave && !room.choices.map(_.id).contains("CaveRest"))
    },

    test("пока герой дремлет, по пещере он не ходит") {
      for {
        t <- cave()
        (state, dao, _, _, r) = t
        _      <- put(dao, smallCave(kind = RoomKind.Rest).copy(at = 2))
        _      <- state.action(testUser, tap("CaveRest"), r)
        move   <- state.action(testUser, tap("CaveLeft"), r)
        screen <- r.sentScreens.map(_.last)
        scene  <- sceneOf(dao)
      } yield assertTrue(move == StateType.MonsterCave && screen.text.contains("дремлете")) &&
              assertTrue(scene.exists(_.at == 2))
    },

    test("за последнего убитого — вдвое больше опыта, и только один раз") {
      for {
        t <- cave()
        (state, dao, _, _, r) = t
        _      <- put(dao, smallCave(monsters = 0).copy(expEarned = 20L))
        _      <- state.enter(testUser, r)
        said   <- texts(r)
        after  <- dao.getHeroByUserId(userId).map(_.get)
        scene  <- sceneOf(dao)
        _      <- state.enter(testUser, r)
        twice  <- dao.getHeroByUserId(userId).map(_.get)
        screen <- r.sentScreens.map(_.last)
      } yield assertTrue(said.contains("+40 опыта") && after.exp == 40L) &&
              assertTrue(said.contains("Кажется, теперь всё чисто.")) &&
              assertTrue(scene.exists(_.rewarded)) &&
              assertTrue(twice.exp == 40L) &&
              // зачистка не выталкивает из пещеры: герой в той же комнате и ходит дальше
              assertTrue(screen.choices.map(_.id).contains("CaveForward")) &&
              assertTrue(scene.exists(_.inside))
    },

    test("сперва драка, потом находка: вернувшись с добычи, герой стоит там же") {
      // Комната с кучкой мобов И сундуком: заход — бой, находка ждёт конца.
      val room  = CaveRoom(0, 1, 3, RoomKind.Chest)
      val start = CaveScene(Race.Goblin.entryName,
        List(CaveRoom(0, 0, 0, RoomKind.Empty, done = true), room), at = 0, inside = true)
      for {
        t <- cave()
        (state, dao, _, _, r) = t
        _      <- put(dao, start)
        fight  <- state.action(testUser, tap("CaveForward"), r)
        loot   <- lootOf(dao)
        // так добычу возвращает LootState: eventData уходит в scene_data
        _      <- dao.writeSceneData(userId, loot.flatMap(_.eventData).get)
        _      <- state.enter(testUser, r)
        screen <- r.sentScreens.map(_.last)
        scene  <- sceneOf(dao)
      } yield assertTrue(fight == StateType.Battle) &&
              assertTrue(scene.exists(s => s.at == 1 && s.rooms(1).monsters == 0)) &&
              assertTrue(screen.text.contains("сундук") && screen.choices.map(_.id).contains("CaveSearch"))
    },

    test("уход из пещеры — с подтверждением, и пещера закрывается") {
      for {
        t <- cave()
        (state, dao, _, sched, r) = t
        _       <- put(dao, smallCave())
        ask     <- state.action(testUser, tap("CaveOut"), r)
        confirm <- r.sentScreens.map(_.last)
        stay    <- state.action(testUser, tap("CaveOutNo"), r)
        left    <- state.action(testUser, tap("CaveOut"), r).zipRight(state.action(testUser, tap("CaveOutYes"), r))
        after   <- dao.readSceneData(userId)
        cancels <- sched.cancelled
      } yield assertTrue(ask == StateType.MonsterCave && confirm.choices.map(_.id) == List("CaveOutYes", "CaveOutNo")) &&
              assertTrue(stay == StateType.MonsterCave) &&
              assertTrue(left == StateType.Dungeon && after.contains(io.circe.Json.Null)) &&
              assertTrue(cancels.contains(userId -> TaskKind.CaveRest))
    }
  )
}
