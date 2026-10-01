package pangea.service.state.states.events.cave

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.{ChoiceColor, SceneContent}
import pangea.generator.item.{FlaskGenerator, MaterialGenerator, TreasureMapGenerator}
import pangea.generator.monster.MonsterGenerator
import pangea.model.monster.MiniBoss
import pangea.model.battle.SoloPveBattle
import pangea.model.artifact.ArtifactKind
import pangea.model.cave.{CaveGenerator, CaveRates, CaveRoom, CaveScene, RoomKind}
import pangea.generator.item.GemGenerator
import pangea.model.item.{GemKind, ItemType, TrophyKind}
import pangea.model.squad.{AllyKind, AllyRates, Squad, UndeadForm}
import pangea.model.hero.Hero
import pangea.generator.loot.TreasureHuntGenerator
import pangea.model.item.{BrewKind, FlaskKind, Item, ItemDetails, MapZone, MaterialKind, Rarity => ItemRarity}
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

  /** Момент, когда поднялись кости: тесты идут при нулевых часах, так что
    * ставим срок заведомо впереди. */
  private val nowStamp = 0L

  private def cave(h: Hero = hero(), items: List[Item] = Nil,
                   artifacts: Option[TestArtifactRepository] = None) =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      inv    = TestInventoryRepository.withItems(items)
      sched <- TestScheduler.make
      r     <- TestRenderer.make
      c     <- content
    } yield (MonsterCaveState(dao, inv, TestItemRepository.make, sched, c, artifacts), dao, inv, sched, r)

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

  /** Все описания стен: пещера выбирает из них наугад. */
  private val walls: List[String] = SceneContent.load().list("cave.walls")

  /** Описания пустых комнат: у каждой своё, закреплённое за местом. */
  private val emptyRooms: List[String] = SceneContent.load().list("cave.rooms.empty")

  private def brew(kind: BrewKind, id: Long): Item = BrewKind.item(kind).copy(id = id)

  /** Трофей расы `race` с уровнем `lvl` — такой падает с обычного моба. */
  private def trophy(kind: TrophyKind, race: Race, lvl: Long): Item =
    Item(id = 1L, name = s"${kind.displayName} ($race)", lvl = lvl, rarity = ItemRarity.Gray,
      itemType = ItemType.Trophy, attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Trophy(race.entryName, kind))

  /** Пещера, где герой стоит прямо у алтаря. */
  private def altarCave(): CaveScene =
    smallCave(kind = RoomKind.Altar).copy(at = 2)

  private def swapTo(pos: Int): UserAction =
    UserAction("", Some(s"""{"action":"CaveSwap","pos":"$pos"}"""))

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
        wall  <- r.sentScreens.map(_.dropRight(1).last.text)
        // в стену можно упираться сколько угодно — описание всякий раз своё
        _     <- ZIO.foreachDiscard(1 to 20)(_ => state.action(testUser, tap("CaveBack"), r))
        seen  <- r.sentScreens.map(_.map(_.text).filter(walls.contains).distinct)
        scene <- sceneOf(dao)
      } yield assertTrue(byId("CaveForward") == ChoiceColor.Positive && byId("CaveRight") == ChoiceColor.Positive) &&
              assertTrue(byId("CaveLeft") == ChoiceColor.Negative && byId("CaveBack") == ChoiceColor.Negative) &&
              assertTrue(stay == StateType.MonsterCave && walls.contains(wall)) &&
              assertTrue(walls.size > 3 && seen.size > 1) &&
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
              // звать в пещере некого: ни подкрепления со стороны, ни клича сородичам
              assertTrue(battle.exists(_.noKin)) &&
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

    // ── Карта клада ───────────────────────────────────────────────────────────

    test("карта клада на пороге: тратится, и в пещере появляется комната с кладом") {
      val map = TreasureMapGenerator.full(MapZone.Kinet).copy(id = 8L)
      for {
        t <- cave(items = List(map))
        (state, dao, inv, _, r) = t
        _     <- put(dao, smallCave(inside = false))
        _     <- state.action(testUser, tap("CaveSupply"), r)
        _     <- state.action(testUser, tap("CaveUse_8"), r)
        said  <- texts(r)
        scene <- sceneOf(dao)
        rooms  = scene.get.rooms
        dug    = rooms.last
        cells  = rooms.map(rm => (rm.x, rm.y))
      } yield assertTrue(said.contains("вела прямиком к этой пещере")) &&
              assertTrue(inv.snapshot.isEmpty && scene.exists(_.treasure.contains(MapZone.Kinet))) &&
              assertTrue(rooms.size == 4 && dug.kind == RoomKind.Treasure && dug.monsters == 0 && !dug.done) &&
              // комната пристроена к пещере, а не поверх неё: место своё, а ход есть
              assertTrue(cells.distinct.size == cells.size) &&
              assertTrue(rooms.init.exists(rm => math.abs(rm.x - dug.x) + math.abs(rm.y - dug.y) == 1))
    },

    test("вторая карта не тратится: клад в пещере уже отмечен") {
      val first  = TreasureMapGenerator.full(MapZone.Kinet).copy(id = 8L)
      val second = TreasureMapGenerator.full(MapZone.TreeShip).copy(id = 9L)
      for {
        t <- cave(items = List(first, second))
        (state, dao, inv, _, r) = t
        _     <- put(dao, smallCave(inside = false))
        _     <- state.action(testUser, tap("CaveSupply"), r)
        _     <- state.action(testUser, tap("CaveUse_8"), r)
        _     <- state.action(testUser, tap("CaveUse_9"), r)
        said  <- texts(r)
        scene <- sceneOf(dao)
      } yield assertTrue(said.contains("второй карте здесь делать нечего")) &&
              assertTrue(inv.snapshot.map(_.id) == List(9L)) &&
              // зона осталась от первой карты, и комната с кладом по-прежнему одна
              assertTrue(scene.exists(_.treasure.contains(MapZone.Kinet))) &&
              assertTrue(scene.exists(_.rooms.count(_.kind == RoomKind.Treasure) == 1))
    },

    test("половинку карты пещера не берёт: на ней не видно, где копать") {
      val half = TreasureMapGenerator.create(10L, half = true).copy(id = 7L)
      for {
        t <- cave(items = List(half))
        (state, dao, inv, _, r) = t
        _     <- put(dao, smallCave(inside = false))
        _     <- state.action(testUser, tap("CaveSupply"), r)
        _     <- state.action(testUser, tap("CaveUse_7"), r)
        said  <- texts(r)
        scene <- sceneOf(dao)
      } yield assertTrue(said.contains("не видно, где копать")) &&
              assertTrue(inv.snapshot.map(_.id) == List(7L)) &&
              assertTrue(scene.exists(s => s.treasure.isEmpty && s.rooms.size == 3))
    },

    test("комната с кладом отдаёт ровно то же, что поход за кладом из города") {
      val treasure = CaveRoom(1, 1, 0, RoomKind.Treasure)
      val withDig  = smallCave().copy(rooms = smallCave().rooms :+ treasure, at = 3,
                       treasure = Some(MapZone.Kinet))
      val seed     = 4242L
      val (reward, _) = TreasureHuntGenerator.roll(MapZone.Kinet, Rng(seed), knowsRareHerbs = false)
      for {
        t <- cave()
        (state, dao, _, _, r) = t
        _     <- put(dao, withDig)
        _     <- TestRandom.feedLongs(seed)
        out   <- state.action(testUser, tap("CaveSearch"), r)
        loot  <- lootOf(dao)
        said  <- texts(r)
        back   = loot.get.eventData.flatMap(_.as[CaveScene].toOption).get
      } yield assertTrue(out == StateType.Loot && said.contains("клад «Кинэт»")) &&
              assertTrue(loot.exists(_.items.map(_.name) ==
                (reward.items ++ reward.gems ++ reward.materials).map(_.name))) &&
              assertTrue(loot.exists(_.silvers == List(reward.silver).filter(_ > 0L))) &&
              assertTrue(loot.exists(_.doubloons == reward.doubloons)) &&
              // из клада герой возвращается в ту же пещеру, а яма остаётся разрытой
              assertTrue(loot.exists(_.returnState.contains(StateType.MonsterCave))) &&
              assertTrue(back.rooms(3).done && back.at == 3)
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

    test("у пустых комнат описания разные, но за комнатой закреплено своё") {
      // Ряд пустых комнат в одну линию: (0,0) вход, дальше на север.
      val rooms = (0 to 6).toList.map(y => CaveRoom(0, y, 0, RoomKind.Empty))
      val line  = CaveScene(Race.Goblin.entryName, rooms, at = 0, inside = true)
      for {
        t <- cave()
        (state, dao, _, _, r) = t
        _      <- put(dao, line)
        _      <- state.enter(testUser, r)
        first  <- r.sentScreens.map(_.last.text)
        // проходим до конца и обратно, запоминая, что видели в каждой комнате
        _      <- ZIO.foreachDiscard(1 to 6)(_ => state.action(testUser, tap("CaveForward"), r))
        there  <- r.sentScreens.map(_.map(_.text).filter(emptyRooms.contains))
        _      <- ZIO.foreachDiscard(1 to 6)(_ => state.action(testUser, tap("CaveBack"), r))
        back   <- r.sentScreens.map(_.map(_.text).filter(emptyRooms.contains))
        home   <- r.sentScreens.map(_.last.text)
      } yield assertTrue(emptyRooms.size >= 8 && emptyRooms.contains(first)) &&
              // соседние комнаты выглядят по-разному
              assertTrue(there.distinct.size > 3) &&
              // на обратном пути каждая узнаётся: описание то же, что и в первый раз
              assertTrue(back == there ++ there.reverse.tail) &&
              assertTrue(home == first)
    },

    test("обысканная комната показывает, что от находки осталось") {
      for {
        t <- cave()
        (state, dao, _, _, r) = t
        _      <- put(dao, smallCave(kind = RoomKind.Chest).copy(at = 2))
        _      <- state.enter(testUser, r)
        before <- r.sentScreens.map(_.last.text)
        _      <- put(dao, smallCave(kind = RoomKind.Chest).copy(at = 2)
                    .withRoom(2, _.copy(done = true)))
        _      <- state.enter(testUser, r)
        after  <- r.sentScreens.map(_.last.text)
        _      <- put(dao, smallCave(kind = RoomKind.Stash).copy(at = 2).withRoom(2, _.copy(done = true)))
        _      <- state.enter(testUser, r)
        stash  <- r.sentScreens.map(_.last.text)
      } yield assertTrue(before.contains("окованный сундук")) &&
              assertTrue(after.contains("Вскрытый сундук") && !emptyRooms.contains(after)) &&
              assertTrue(stash.contains("Разрытые камни"))
    },

    test("сорванная трава уходит в Живую сумку — и герой об этом слышит") {
      val bag = TestArtifactRepository.of(
        bag = TestArtifactRepository.artifact(ArtifactKind.LivingBag, tier = 1))
      for {
        t <- cave(artifacts = Some(bag))
        (state, dao, inv, _, r) = t
        _     <- put(dao, smallCave(kind = RoomKind.Herb).copy(at = 2))
        _     <- state.action(testUser, tap("CaveSearch"), r)
        said  <- texts(r)
      } yield assertTrue(said.contains("Живая сумка") && said.contains("Свободно мест")) &&
              assertTrue(bag.snapshot.of(ArtifactKind.LivingBag).items.data.size == 1 && inv.snapshot.isEmpty)
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

    test("за последнего убитого — сто шестьдесят процентов взятого, и только один раз") {
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
      } yield assertTrue(said.contains("+32 опыта") && after.exp == 32L) &&
              assertTrue(CaveRates.ClearExpPct == 160L) &&
              assertTrue(said.contains("Кажется, теперь всё чисто.")) &&
              assertTrue(scene.exists(_.rewarded)) &&
              assertTrue(twice.exp == 32L) &&
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

    // ── Алтарь тёмных сил ───────────────────────────────────────────────────

    test("алтарь ровно в половине пещер и всегда один; привал его больше не съедает") {
      val caves = (1L to 300L).toList.map(seed => CaveGenerator.generate(Race.Orc.entryName, Rng(seed))._1)
      val withAltar = caves.count(_.rooms.exists(_.kind == RoomKind.Altar))
      assertTrue(caves.forall(_.rooms.count(_.kind == RoomKind.Altar) <= 1)) &&
      // ровно половина с поправкой на случайность выборки: место привала
      // алтарь больше не съедает — оно просто пропускается при выборе
      assertTrue(withAltar > 120 && withAltar < 180) &&
      assertTrue(caves.forall(s => s.rooms.count(r => r.kind == RoomKind.Altar || r.kind == RoomKind.Rest) ==
                   (if (s.rooms.exists(_.kind == RoomKind.Altar)) 2 else 1))) &&
      // привал алтарём не вытесняется: угол для отдыха в пещере всё равно один
      assertTrue(caves.forall(_.rooms.count(_.kind == RoomKind.Rest) == 1))
    },

    test("трофеи поднимают нежить по своей ценности, а клык — того самого волка") {
      val sack  = trophy(TrophyKind.Sack, Race.Goblin, lvl = 16L)
      val relic = trophy(TrophyKind.Relic, Race.Orc, lvl = 20L)
      val fang  = pangea.generator.loot.LootGenerator.wolfFang(bossLvl = 5L, floorLvl = 30L)
      val goblin = DarkAltar.formOf(sack)
      val orc    = DarkAltar.formOf(relic)
      val wolf   = DarkAltar.formOf(fang)
      // мешок — раб, реликвия — вожак: имя то же, что у моба этого тира
      val rawGoblin = MonsterGenerator.generateOfRaceAndRarity(16, Race.Goblin, Rarity.Common)
      assertTrue(goblin.exists(f => f.name == rawGoblin.name && f.lvl == 16L)) &&
      assertTrue(goblin.exists(_.stats.atk == rawGoblin.fightStats.atk * 80L / 100L)) &&
      assertTrue(orc.exists(_.name == MonsterGenerator.generateOfRaceAndRarity(20, Race.Orc, Rarity.Mythical).name)) &&
      assertTrue(DarkAltar.rarityOf(TrophyKind.Head).contains(Rarity.Uncommon) &&
                 DarkAltar.rarityOf(TrophyKind.Talisman).contains(Rarity.Rare)) &&
      // волк встаёт тем, каким его убили: уровень босса зашит в самом клыке
      assertTrue(wolf.exists(f => f.name == DarkAltar.DarkWolfName && f.lvl == 5L)) &&
      assertTrue(wolf.exists(_.stats.hp == MiniBoss.WhiteWolf.stats(5L).hp * 80L / 100L))
    },

    test("камень-усилитель уходит в алтарь и возвращается черепом того же достоинства") {
      val ruby = GemGenerator.item(GemKind.Ruby, 3).copy(id = 11L)
      for {
        t <- cave(items = List(ruby))
        (state, dao, inv, _, r) = t
        _     <- put(dao, altarCave())
        _     <- state.action(testUser, tap("CaveAltar"), r)
        list  <- r.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("CaveUse_11"), r)
        said  <- texts(r)
        after  = inv.snapshot.find(_.id == 11L)
        scene <- sceneOf(dao)
      } yield assertTrue(list.choices.map(_.id).contains("CaveUse_11")) &&
              assertTrue(after.exists(_.gem.exists(g => g.kind == GemKind.Skull && g.grade == 3))) &&
              assertTrue(said.contains("выплёвывает")) &&
              // череп стоит камню тех же сил, что и поднятый: алтарь гаснет
              assertTrue(scene.exists(_.altarSpent) && said.contains("Сила алтаря израсходована"))
    },

    test("трофей поднимает союзника, и алтарь после этого гаснет") {
      val sack = trophy(TrophyKind.Sack, Race.Goblin, lvl = 16L).copy(id = 21L)
      for {
        t <- cave(items = List(sack))
        (state, dao, inv, _, r) = t
        _      <- put(dao, altarCave())
        _      <- state.action(testUser, tap("CaveAltar"), r)
        _      <- state.action(testUser, tap("CaveUse_21"), r)
        said   <- texts(r)
        hero   <- dao.getHeroByUserId(userId).map(_.get)
        scene  <- sceneOf(dao)
        room   <- r.sentScreens.map(_.last)
        // второй трофей алтарь уже не примет
        _      <- state.action(testUser, tap("CaveAltar"), r)
        closed <- r.sentScreens.map(_.last)
      } yield assertTrue(inv.snapshot.isEmpty && said.contains("поднимается")) &&
              assertTrue(hero.squad.allies.size == 1) &&
              assertTrue(hero.squad.allies.head.kind == AllyKind.Undead) &&
              assertTrue(hero.squad.allies.head.undead.exists(_.lvl == 16L)) &&
              // поднятый не уходит по времени и полон сил
              assertTrue(!hero.squad.allies.head.expired(Long.MaxValue)) &&
              assertTrue(hero.squad.allies.head.hp > 0L) &&
              assertTrue(said.contains("Сила алтаря израсходована")) &&
              assertTrue(scene.exists(_.altarSpent)) &&
              // кнопки алтаря на остывшей плите больше нет
              assertTrue(!room.choices.map(_.id).contains("CaveAltar")) &&
              assertTrue(!closed.choices.map(_.id).contains("CaveAltar"))
    },

    test("поднятый идёт в бой своими статами и под своим именем") {
      val form  = UndeadForm("Гоблин немощный раб", 16L, AllyKind.Human.stats(4L))
      val risen = hero().copy(squad = Squad.empty.raise(form, 10L, nowStamp))
      for {
        t <- cave(risen)
        (state, dao, _, _, r) = t
        _      <- put(dao, smallCave(monsters = 3))
        _      <- state.action(testUser, tap("CaveForward"), r)
        battle <- battleOf(dao)
        ally    = battle.flatMap(_.group.allies.headOption)
      } yield assertTrue(ally.exists(a => a.name == "Гоблин немощный раб" && a.lvl == 16L)) &&
              assertTrue(ally.exists(a => a.stats == AllyKind.Human.stats(4L) && a.hp == a.stats.hp)) &&
              assertTrue(ally.exists(_.kind.race == Race.Undead))
    },

    test("полный отряд: алтарь спрашивает, кем пожертвовать; отказ бережёт и трофей, и своих") {
      val sack = trophy(TrophyKind.Sack, Race.Goblin, lvl = 16L).copy(id = 31L)
      val full = (1 to AllyRates.Positions - 1).foldLeft(Squad.empty) { (sq, i) =>
        sq.raise(UndeadForm(s"Поднятый $i", 1L, AllyKind.Human.stats(1L)), 10L, nowStamp)
      }
      for {
        t <- cave(hero().copy(squad = full), items = List(sack))
        (state, dao, inv, _, r) = t
        _      <- put(dao, altarCave())
        _      <- state.action(testUser, tap("CaveAltar"), r)
        _      <- state.action(testUser, tap("CaveUse_31"), r)
        ask    <- r.sentScreens.map(_.last)
        // сперва отказываемся — трофей и отряд целы
        _      <- state.action(testUser, tap("CaveSwapNo"), r)
        kept   <- dao.getHeroByUserId(userId).map(_.get)
        // снимок сумки берём сразу: стаб отдаёт текущее состояние, а не копию
        keptBag = inv.snapshot.map(_.id)
        // потом соглашаемся: место уступает тот, кого выбрали
        _      <- state.action(testUser, tap("CaveAltar"), r)
        _      <- state.action(testUser, tap("CaveUse_31"), r)
        _      <- state.action(testUser, swapTo(3), r)
        after  <- dao.getHeroByUserId(userId).map(_.get)
        scene  <- sceneOf(dao)
      } yield assertTrue(ask.text.contains("некуда встать") && ask.choices.map(_.id).contains("CaveSwap")) &&
              assertTrue(ask.choices.flatMap(_.row).groupBy(identity).forall(_._2.size <= 5)) &&
              assertTrue(kept.squad.allies.size == 10 && keptBag == List(31L)) &&
              assertTrue(after.squad.allies.size == 10 && inv.snapshot.isEmpty) &&
              assertTrue(after.squad.allyAt(3).exists(_.undead.exists(_.lvl == 16L))) &&
              assertTrue(!after.squad.allies.exists(_.name == "Поднятый 2")) &&
              assertTrue(scene.exists(_.altarSpent))
    },

    test("чужое алтарю безразлично: вещь остаётся у героя") {
      val herb = MaterialGenerator.item(MaterialKind.GlaiveMushroom).copy(id = 41L)
      for {
        t <- cave(items = List(herb))
        (state, dao, inv, _, r) = t
        _     <- put(dao, altarCave())
        _     <- state.action(testUser, tap("CaveAltar"), r)
        _     <- state.action(testUser, tap("CaveUse_41"), r)
        said  <- texts(r)
        hero  <- dao.getHeroByUserId(userId).map(_.get)
        scene <- sceneOf(dao)
      } yield assertTrue(said.contains("равнодушен") && inv.snapshot.map(_.id) == List(41L)) &&
              assertTrue(hero.squad.allies.isEmpty && scene.exists(!_.altarSpent))
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
