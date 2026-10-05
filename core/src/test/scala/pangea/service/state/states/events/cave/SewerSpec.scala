package pangea.service.state.states.events.cave

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.loot.LootGenerator
import pangea.generator.loot.LootGenerator.LootDrop
import pangea.generator.monster.MonsterGenerator
import pangea.model.artifact.ArtifactKind
import pangea.model.battle.SoloPveBattle
import pangea.model.cave.{CaveGenerator, CaveRates, CaveRoom, CaveScene, RoomKind, SewerRates}
import pangea.model.hero.Hero
import pangea.model.item.{Item, MaterialKind}
import pangea.model.monster.{MiniBoss, Monster, MonsterRaceFactor, Race, Rarity}
import pangea.model.quest.{BoardData, BoardKind, BoardSlot, Difficulty}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.road.{QuestRoadState, RoadProgress}
import pangea.test._
import zio.test._
import zio.{Duration, Task, ZIO}

/** Канализация: задание с доски, дорога до места, крысы вместо смертных и
  * второй ярус под первым. Механика пещерная — здесь проверяется ровно то, чем
  * канализация от пещеры отличается. */
object SewerSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def content = ZIO.attempt(SceneContent.load())

  private def hero(lvl: Long = 40L): Hero =
    TestFixtures.hero(userId, dungeonLevel = 40).copy(lvl = lvl)

  private def cave(h: Hero = hero(), items: List[Item] = Nil) =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      inv    = TestInventoryRepository.withItems(items)
      sched <- TestScheduler.make
      r     <- TestRenderer.make
      c     <- content
    } yield (MonsterCaveState(dao, inv, TestItemRepository.make, sched, c, None), dao, sched, r)

  private def road(h: Hero = hero()) =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      sched <- TestScheduler.make
      r     <- TestRenderer.make
      c     <- content
    } yield (QuestRoadState(dao, sched, c), dao, sched, r)

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  private def sceneOf(dao: TestHeroDao): Task[Option[CaveScene]] =
    dao.readSceneData(userId).map(_.flatMap(_.as[CaveScene].toOption))

  /** Канализация на три комнаты: вход, за ним крысы, сбоку — то, что задано. */
  private def smallSewer(
    monsters: Int      = 3,
    kind:     RoomKind = RoomKind.Stairs,
    questLvl: Long     = 7L,
    floor:    Int      = 1
  ): CaveScene =
    CaveScene(
      race  = Race.Animal.entryName,
      rooms = List(
        CaveRoom(0, 0, 0, RoomKind.Empty, done = true),
        CaveRoom(0, 1, monsters, RoomKind.Empty, done = true),
        CaveRoom(1, 0, 0, kind)),
      at = 0, inside = true, sewer = true, questLvl = questLvl, floor = floor)

  /** Доска, на которой взяты и канализация, и пещера. */
  private def bothTaken: BoardData =
    BoardData(0L, "novice", List(
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

  override def spec = suite("Канализация")(

    // ── Генерация ────────────────────────────────────────────────────────────

    test("верхний ярус: как пещера, но алтарь в каждой и ход вниз ровно один") {
      val floors = (1L to 200L).toList.map(s => CaveGenerator.sewer(9L, Rng(s))._1)
      assertTrue(floors.forall(f => f.sewer && f.questLvl == 9L && f.floor == 1)) &&
      assertTrue(floors.forall(_.race == Race.Animal.entryName)) &&
      assertTrue(floors.forall(f => f.rooms.size >= CaveRates.MinRooms && f.rooms.size <= CaveRates.MaxRooms)) &&
      assertTrue(floors.forall(f => reachable(f) == f.rooms.size)) &&
      assertTrue(floors.forall { f =>
        val total = f.rooms.map(_.monsters).sum
        total >= CaveRates.MinMonsters && total <= CaveRates.MaxMonsters
      }) &&
      // алтарь здесь не «в половине случаев», а всегда — и один
      assertTrue(SewerRates.AltarChancePct == 100) &&
      assertTrue(floors.forall(_.rooms.count(_.kind == RoomKind.Altar) == 1)) &&
      // ход вниз есть всегда и тоже один, и он не вытесняет ни привал, ни алтарь
      assertTrue(floors.forall(_.rooms.count(_.kind == RoomKind.Stairs) == 1)) &&
      assertTrue(floors.forall(_.rooms.count(_.kind == RoomKind.Rest) == 1)) &&
      // на пороге не бьют
      assertTrue(floors.forall(_.rooms.head.monsters == 0))
    },

    test("нижний ярус: короче, без привала и без хода дальше") {
      val deep = (1L to 200L).toList.map(s => CaveGenerator.sewerDeep(9L, Rng(s))._1)
      assertTrue(deep.forall(d => d.sewer && d.floor == SewerRates.Floors && d.inside)) &&
      assertTrue(deep.forall(d => d.rooms.size >= SewerRates.DeepMinRooms && d.rooms.size <= SewerRates.DeepMaxRooms)) &&
      assertTrue(deep.forall(d => reachable(d) == d.rooms.size)) &&
      assertTrue(deep.forall { d =>
        // Крысы считаются без логова: там ждёт король, а не кучка.
        val total = d.rooms.filterNot(_.kind == RoomKind.Lair).map(_.monsters).sum
        total >= SewerRates.DeepMinMonsters - SewerRates.DeepMaxGroup &&
          total <= SewerRates.DeepMaxMonsters
      }) &&
      // Логово ровно одно, не у входа, и король в нём один
      assertTrue(deep.forall(_.rooms.count(_.kind == RoomKind.Lair) == 1)) &&
      assertTrue(deep.forall(_.rooms.head.kind != RoomKind.Lair)) &&
      assertTrue(deep.forall(_.rooms.find(_.kind == RoomKind.Lair).exists(_.monsters == SewerRates.KingCount))) &&
      // пока король жив, ярус не выбит
      assertTrue(deep.forall(!_.cleared)) &&
      assertTrue(deep.forall(!_.rooms.exists(_.kind == RoomKind.Rest))) &&
      assertTrue(deep.forall(!_.rooms.exists(_.kind == RoomKind.Stairs))) &&
      // алтарь внизу — как повезёт, примерно в половине
      assertTrue(deep.forall(_.rooms.count(_.kind == RoomKind.Altar) <= 1)) && {
        val withAltar = deep.count(_.rooms.exists(_.kind == RoomKind.Altar))
        assertTrue(withAltar > 70 && withAltar < 130)
      }
    },

    test("пещеру канализация не задела: алтарь в ней по-прежнему в половине случаев") {
      val caves     = (1L to 300L).toList.map(s => CaveGenerator.generate(Race.Orc.entryName, Rng(s))._1)
      val withAltar = caves.count(_.rooms.exists(_.kind == RoomKind.Altar))
      assertTrue(withAltar > 120 && withAltar < 180) &&
      assertTrue(caves.forall(_.rooms.count(_.kind == RoomKind.Rest) == 1)) &&
      assertTrue(caves.forall(c => !c.sewer && c.floor == 1 && c.lastFloor)) &&
      // хода вниз в пещере не бывает
      assertTrue(caves.forall(!_.rooms.exists(_.kind == RoomKind.Stairs)))
    },

    // ── Крысы ────────────────────────────────────────────────────────────────

    test("крысы: обычная второй редкости, чумная третьей, и у каждой своё имя") {
      val pool = SewerRates.RarityPool
      assertTrue(pool.size == 100 && pool.toSet == Set[Rarity](Rarity.Uncommon, Rarity.Rare)) &&
      assertTrue(pool.count(_ == Rarity.Uncommon) == 70 && pool.count(_ == Rarity.Rare) == 30) &&
      // вторая и третья редкости обычного моба — те же самые
      assertTrue(Rarity.values.indexOf(Rarity.Uncommon) == 1 && Rarity.values.indexOf(Rarity.Rare) == 2) &&
      assertTrue(Monster(0L, 5L, Race.Animal, Rarity.Uncommon, null).name == "Крыса") &&
      assertTrue(Monster(0L, 5L, Race.Animal, Rarity.Rare, null).name == "Чумная крыса")
    },

    test("крысе нечем лечиться: ни фляги себе, ни соседу, ни починки доспеха") {
      val forRats = pangea.model.skill.MonsterSkill.values.filter(_.availableTo(Race.Animal))
      val flask   = pangea.model.skill.MonsterSkill.HealingFlask
      val repair  = pangea.model.skill.MonsterSkill.EmergencyRepair
      assertTrue(!forRats.contains(flask) && !forRats.contains(repair)) &&
      // бить ей по-прежнему есть чем
      assertTrue(forRats.contains(pangea.model.skill.MonsterSkill.QuickStrike)) &&
      assertTrue(forRats.contains(pangea.model.skill.MonsterSkill.CrushingStrike)) &&
      // у тех, кто носит пояс и доспех, всё на месте
      assertTrue(Race.mortals.forall(r => flask.availableTo(r) && repair.availableTo(r))) &&
      // сооружения и прочие боссовые расы тоже себя не латают
      assertTrue(Race.bossRaces.forall(r => !flask.availableTo(r) && !repair.availableTo(r)))
    },

    test("статы крысы: мяса мало, брони нет, зато вёрткая") {
      val f   = MonsterRaceFactor.of(Race.Animal)
      val rat = MonsterGenerator.generateOfRaceAndRarity(10, Race.Animal, Rarity.Uncommon).fightStats
      val sick = MonsterGenerator.generateOfRaceAndRarity(10, Race.Animal, Rarity.Rare).fightStats
      assertTrue(f.hpFactor == 0.6 && f.armorFactor == 0.3 && f.defenceFactor == 0.5) &&
      assertTrue(f.attackFactor == 0.9 && f.accuracyFactor == 1.2 && f.evasionFactor == 1.6) &&
      // база = уровень × редкость × 1.1 = 10 × 1.2 × 1.1 = 13.2
      assertTrue(rat.atk == 118L && rat.hp == 316L && rat.armor == 91L) &&
      assertTrue(rat.defence == 46L && rat.evasion == 343L && rat.accuracy == 261L) &&
      // чумная сильнее ровно на отношение редкостей
      assertTrue(sick.hp == 422L && sick.atk == 158L)
    },

    test("дроп с крысы: ни трофеев, ни серебра, ни камней — только то, что срезают") {
      val drops = (1L to 4000L).toList.flatMap(s =>
        LootGenerator.roll(Rarity.Rare, Race.Animal, 10L, Rng(s))._1)
      val mats = drops.collect { case LootDrop.Gear(i) => i.material }.flatten
      assertTrue(drops.nonEmpty) &&
      assertTrue(!drops.exists {
        case _: LootDrop.Trophy | _: LootDrop.Silver | _: LootDrop.Doubloons => true
        case _: LootDrop.Gem | _: LootDrop.Flask | _: LootDrop.MapHalf       => true
        case _                                                               => false
      }) &&
      // вещей тоже нет: всё «снаряжение» в этой добыче — материалы
      assertTrue(mats.size == drops.count { case _: LootDrop.Gear => true; case _ => false }) &&
      assertTrue(mats.toSet == MaterialKind.sewerSpoils.toSet) &&
      // руны изредка, и только малые
      assertTrue(drops.exists { case _: LootDrop.Rune => true; case _ => false }) &&
      assertTrue(drops.collect { case LootDrop.Rune(i) => i.name }.forall(_.contains("Малая"))) && {
        val count = (k: MaterialKind) => mats.count(_ == k)
        // шкурка и хвост — обычное дело, черви реже, кровь короля реже всех
        assertTrue(count(MaterialKind.RatPelt) > count(MaterialKind.PlagueWorms)) &&
        assertTrue(count(MaterialKind.RatTail) > count(MaterialKind.PlagueWorms)) &&
        assertTrue(count(MaterialKind.PlagueWorms) > count(MaterialKind.RatKingBlood)) &&
        assertTrue(count(MaterialKind.RatKingBlood) > 0)
      }
    },

    test("«Таксидермист» со зверя трофея не снимает") {
      val extras = (1L to 300L).toList.flatMap(s =>
        LootGenerator.rollPassiveDrops(100L, 0L, Rarity.Rare, Race.Animal, 10L, Rng(s))._1)
      val human = (1L to 10L).toList.flatMap(s =>
        LootGenerator.rollPassiveDrops(100L, 0L, Rarity.Rare, Race.Human, 10L, Rng(s))._1)
      assertTrue(extras.isEmpty) &&
      assertTrue(human.forall { case _: LootDrop.Trophy => true; case _ => false } && human.size == 10)
    },

    test("крысиное добро: за него не платят, но в шкаф его положить можно") {
      val spoils = MaterialKind.sewerSpoils
      val pelt   = pangea.generator.item.MaterialGenerator.item(MaterialKind.RatPelt)
      assertTrue(spoils.toSet == Set[MaterialKind](MaterialKind.RatPelt, MaterialKind.RatTail,
        MaterialKind.PlagueWorms, MaterialKind.RatKingBlood)) &&
      // Шкурки, хвосты и черви не стоят ничего; кровь короля Ришелье берёт
      // золотом — это редкий ингредиент будущего набора.
      assertTrue(spoils.filterNot(_ == MaterialKind.RatKingBlood)
        .forall(m => m.worthless && !m.isHerb && m.doubloonPrice == 0L)) &&
      assertTrue(!MaterialKind.RatKingBlood.worthless &&
        MaterialKind.RatKingBlood.doubloonPrice == 5L) &&
      assertTrue(spoils.forall(_.description.nonEmpty)) &&
      // шкаф герой набивает руками — что в нём держать, решает он сам
      assertTrue(ArtifactKind.Wardrobe.accepts(pelt)) &&
      // сама добыча в хранилища не прыгает: ни ларец, ни живая сумка её не ловят
      assertTrue(ArtifactKind.forItem(pelt).isEmpty)
    },

    // ── Дорога ───────────────────────────────────────────────────────────────

    test("дорога: десять минут, потом канализация по уровню задания") {
      for {
        t <- road()
        (state, dao, sched, r) = t
        _      <- dao.writeSceneData(userId, RoadProgress(0L, BoardKind.SewerRats, 12L).asJson)
        _      <- state.enter(testUser, r)
        start  <- r.sentScreens.map(_.last)
        early  <- state.action(testUser, UserAction("привет", None), r)
        _      <- TestClock.adjust(Duration.fromMillis(SewerRates.RoadMs + 1000L))
        out    <- state.action(testUser, tap("RoadDone"), r)
        scene  <- sceneOf(dao)
        said   <- texts(r)
        cancels <- sched.cancelled
      } yield assertTrue(start.choices.map(_.id) == List("RoadBack")) &&
              assertTrue(early == StateType.QuestRoad && said.contains("Осталось примерно")) &&
              assertTrue(out == StateType.MonsterCave) &&
              assertTrue(scene.exists(s => s.sewer && s.questLvl == 12L && s.floor == 1 && !s.inside)) &&
              assertTrue(cancels.contains(userId -> TaskKind.QuestRoad))
    },

    test("с дороги можно повернуть назад, не дожидаясь конца") {
      for {
        t <- road()
        (state, dao, sched, r) = t
        _       <- dao.writeSceneData(userId, RoadProgress(0L, BoardKind.SewerRats, 12L).asJson)
        _       <- state.enter(testUser, r)
        screen  <- r.sentScreens.map(_.last)
        out     <- state.action(testUser, tap("RoadBack"), r)
        after   <- dao.readSceneData(userId)
        said    <- texts(r)
        cancels <- sched.cancelled
      } yield assertTrue(screen.choices.map(_.id) == List("RoadBack")) &&
              assertTrue(out == StateType.GlobalMap && after.contains(io.circe.Json.Null)) &&
              assertTrue(said.contains("поворачиваете обратно")) &&
              // задачу поллера снимаем: на месте героя уже не ждут
              assertTrue(cancels.contains(userId -> TaskKind.QuestRoad))
    },

    test("дорога без дороги не бросает героя в поле, а выводит в город") {
      for {
        t <- road()
        (state, dao, _, r) = t
        out   <- state.action(testUser, tap("RoadDone"), r)
        after <- dao.readSceneData(userId)
      } yield assertTrue(out == StateType.GlobalMap && after.contains(io.circe.Json.Null))
    },

    // ── Внутри ───────────────────────────────────────────────────────────────

    test("порог канализации: сложность знаками, расу не поминают") {
      for {
        t <- cave()
        (state, dao, _, r) = t
        _    <- dao.writeSceneData(userId, smallSewer().copy(inside = false).asJson)
        _    <- state.enter(testUser, r)
        gate <- r.sentScreens.map(_.last)
      } yield assertTrue(gate.text.contains(Difficulty.render(7)) && gate.text.contains("Решётка")) &&
              assertTrue(!gate.text.contains("Животное")) &&
              assertTrue(gate.choices.exists(c => c.id == "CaveEnter" && c.label == "Спуститься"))
    },

    test("в бой идут крысы уровнем в задание, а не в этаж лабиринта") {
      for {
        t <- cave(hero())
        (state, dao, _, r) = t
        _      <- dao.writeSceneData(userId, smallSewer(monsters = 3, questLvl = 7L).asJson)
        next   <- state.action(testUser, tap("CaveForward"), r)
        battle <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        said   <- texts(r)
      } yield assertTrue(next == StateType.Battle) &&
              assertTrue(battle.exists(_.monsterRace == Race.Animal.entryName)) &&
              assertTrue(battle.exists(_.monsterLvl == 7L)) &&
              assertTrue(battle.exists(b => (b.rarity :: b.group.others.map(s => Rarity.withName(s.rarity)))
                .forall(rr => rr == Rarity.Uncommon || rr == Rarity.Rare))) &&
              assertTrue(said.contains("Из труб с писком"))
    },

    test("ход вниз ведёт на нижний ярус и уносит туда взятый опыт") {
      for {
        t <- cave()
        (state, dao, _, r) = t
        _     <- dao.writeSceneData(userId,
                   smallSewer(monsters = 0, kind = RoomKind.Stairs).copy(at = 2, expEarned = 40L).asJson)
        _     <- state.enter(testUser, r)
        room  <- r.sentScreens.map(_.last)
        down  <- state.action(testUser, tap("CaveDown"), r)
        deep  <- sceneOf(dao)
        said  <- texts(r)
      } yield assertTrue(room.choices.exists(c => c.id == "CaveDown" && c.label == "Спуститься ниже")) &&
              assertTrue(down == StateType.MonsterCave && said.contains("нижний ярус")) &&
              assertTrue(deep.exists(d => d.floor == 2 && d.sewer && d.questLvl == 7L)) &&
              assertTrue(deep.exists(d => d.expEarned == 40L && d.restUsed && !d.rewarded)) &&
              assertTrue(deep.exists(_.rooms.size >= SewerRates.DeepMinRooms))
    },

    test("за выбитый верхний ярус не платят: большая крыса ниже") {
      for {
        t <- cave()
        (state, dao, _, r) = t
        _     <- dao.writeQuestData(userId, bothTaken.asJson)
        _     <- dao.writeSceneData(userId, smallSewer(monsters = 0).copy(expEarned = 50L).asJson)
        _     <- state.enter(testUser, r)
        said  <- texts(r)
        after <- dao.getHeroByUserId(userId).map(_.get)
        scene <- sceneOf(dao)
        board <- dao.readQuestData(userId).map(_.flatMap(_.as[BoardData].toOption).get)
      } yield assertTrue(said.contains("ещё слышна возня")) &&
              assertTrue(after.exp == 0L && scene.exists(!_.rewarded)) &&
              assertTrue(board.slots.forall(!_.done))
    },

    test("выбитый нижний ярус закрывает объявление о крысах, но не о пещерах") {
      for {
        t <- cave()
        (state, dao, _, r) = t
        _     <- dao.writeQuestData(userId, bothTaken.asJson)
        _     <- dao.writeSceneData(userId,
                   smallSewer(monsters = 0, kind = RoomKind.Empty, floor = 2).copy(expEarned = 50L).asJson)
        _     <- state.enter(testUser, r)
        said  <- texts(r)
        after <- dao.getHeroByUserId(userId).map(_.get)
        board <- dao.readQuestData(userId).map(_.flatMap(_.as[BoardData].toOption).get)
      } yield assertTrue(said.contains("+80 опыта") && after.exp == 80L) &&
              assertTrue(said.contains("Большая крыса больше никого не испугает")) &&
              assertTrue(said.contains("В канализации тихо")) &&
              // закрыто объявление о крысах; смежное, про пещеру, не тронуто
              assertTrue(board.slots.find(_.kind == BoardKind.SewerRats).exists(_.done)) &&
              assertTrue(board.slots.find(_.kind == BoardKind.CaveClear).exists(!_.done))
    },

    test("в логове ждёт король: бой с минибоссом, а не с кучкой") {
      for {
        t <- cave(hero(lvl = 10L))
        (state, dao, _, r) = t
        _      <- dao.writeSceneData(userId,
                    smallSewer(monsters = 0, kind = RoomKind.Lair, floor = 2).copy(at = 0).asJson)
        // в комнате с логовом король стоит сам, комнату занимает он
        _      <- dao.writeSceneData(userId, smallSewer(monsters = 0, kind = RoomKind.Lair, floor = 2)
                    .copy(rooms = List(
                      CaveRoom(0, 0, 0, RoomKind.Empty, done = true),
                      CaveRoom(0, 1, SewerRates.KingCount, RoomKind.Lair))).asJson)
        out    <- state.action(testUser, tap("CaveForward"), r)
        battle <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        said   <- texts(r)
        loot   <- dao.readSceneData(userId).map(_.flatMap(_.as[pangea.service.state.states.LootState.LootData].toOption))
        back    = loot.flatMap(_.eventData).flatMap(_.as[CaveScene].toOption)
      } yield assertTrue(out == StateType.Battle && said.contains("Крысиный король")) &&
              assertTrue(battle.exists(_.boss.contains(MiniBoss.RatKing))) &&
              // уровень босса — от уровня героя, а не от задания
              assertTrue(battle.exists(_.monsterLvl == MiniBoss.RatKing.bossLvl(10L))) &&
              // звать ему некого, зато своих он позовёт сам — уровнем в задание
              assertTrue(battle.exists(b => b.noKin && b.minionLvl == 7L)) &&
              // всякая рана от него гноится
              assertTrue(battle.exists(_.effects.monsterPoisonsOnHit)) &&
              // вернёмся в ту же канализацию, с выбитым логовом
              assertTrue(back.exists(s => s.sewer && s.floor == 2 && s.cleared))
    },

    test("из канализации уходят в город, а не в лабиринт") {
      for {
        t <- cave()
        (state, dao, _, r) = t
        _     <- dao.writeSceneData(userId, smallSewer().asJson)
        ask   <- state.action(testUser, tap("CaveOut"), r)
        said  <- texts(r)
        out   <- state.action(testUser, tap("CaveOutYes"), r)
        after <- dao.readSceneData(userId)
      } yield assertTrue(ask == StateType.MonsterCave && said.contains("Уйти из канализации?")) &&
              assertTrue(out == StateType.GlobalMap && after.contains(io.circe.Json.Null))
    },

    test("у канализации свои слова там, где они ей нужны") {
      // Эти два ключа — списки, их читают не text, а list.
      val lists = Set("walls", "rooms.empty")
      for {
        c <- content
      } yield assertTrue(MonsterCaveState.SewerSays.filterNot(lists.contains)
                .forall(n => c.text(s"cave.$n").nonEmpty && c.text(s"sewer.$n").nonEmpty)) &&
              assertTrue(lists.subsetOf(MonsterCaveState.SewerSays)) &&
              assertTrue(c.list("sewer.walls").size == c.list("cave.walls").size) &&
              assertTrue(c.list("sewer.rooms.empty").size == c.list("cave.rooms.empty").size) &&
              assertTrue(List("room.stairs", "act.down", "descended", "upperClear")
                .forall(n => c.text(s"sewer.$n").nonEmpty))
    }
  )
}
