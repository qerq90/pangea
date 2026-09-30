package pangea.service.state.states.dungeon

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.item.{Item, ItemDetails}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.model.schedule.TaskKind
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer, TestScheduler}
import zio.test._
import zio.test.{TestClock, TestRandom}
import zio.{Duration, ZIO}

object DungeonStateSpec extends ZIOSpecDefault {

  private def flaskCharges(i: Item): Option[Int] = i.details match {
    case f: ItemDetails.Flask => Some(f.charges)
    case _                    => None
  }

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def isValidFindOutcome(s: StateType): Boolean =
    StateType.events.contains(s) || s == StateType.Battle || s == StateType.Dungeon

  private def makeState(dungeonLevel: Int = 1, maxDungeonLevel: Int = 150) =
    for {
      heroDao   <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, dungeonLevel = dungeonLevel, maxDungeonLevel = maxDungeonLevel))
      renderer  <- TestRenderer.make
      scheduler <- TestScheduler.make
      content   <- ZIO.attempt(SceneContent.load())
      invRepo    = TestInventoryRepository.accepting
    } yield (DungeonState(heroDao, invRepo, scheduler, content), heroDao, renderer, scheduler)

  import pangea.engine.ChoiceColor
  private def colorOf(s: pangea.engine.Screen, id: String): Option[ChoiceColor] =
    s.choices.find(_.id == id).map(_.color)

  override def spec = suite("DungeonState")(

    // ── Отдых ─────────────────────────────────────────────────────────────────
    test("«Отдых» с быстрым отдыхом тратит его сразу, не заводя привал") {
      for {
        q <- makeState()
        (state, heroDao, renderer, scheduler) = q
        // Раненый герой с двумя быстрыми отдыхами от благословения.
        hero0 <- heroDao.getHeroByUserId(userId).map(_.get)
        _     <- heroDao.updateFightStats(userId, hero0.fightStats.copy(hp = 1L, energy = 0L))
        _     <- heroDao.writeAzatData(userId,
                   pangea.model.hero.AzatState(instantRests = 2).asJson)
        res     <- state.action(testUser, UserAction("", Some("""{"action":"Rest"}""")), renderer)
        hero    <- heroDao.getHeroByUserId(userId).map(_.get)
        azat    <- heroDao.readAzatData(userId).map(_.flatMap(_.as[pangea.model.hero.AzatState].toOption).get)
        scene   <- heroDao.readSceneData(userId)
        tasks   <- scheduler.scheduled
        screens <- renderer.sentScreens
      } yield assertTrue(res == StateType.Dungeon) &&      // остались в лабиринте
              assertTrue(hero.fightStats.hp == hero.effectiveMaxHp(0L)) &&
              assertTrue(azat.instantRests == 1) &&        // потрачен ровно один заряд
              assertTrue(scene.contains(io.circe.Json.Null)) &&
              assertTrue(!tasks.exists(_.kind == pangea.model.schedule.TaskKind.Revive)) &&
              assertTrue(screens.map(_.text).mkString.contains("Осталось"))
    },

    test("без быстрых отдыхов «Отдых» по-прежнему уводит к костру") {
      for {
        q <- makeState()
        (state, _, renderer, _) = q
        res <- state.action(testUser, UserAction("", Some("""{"action":"Rest"}""")), renderer)
      } yield assertTrue(res == StateType.Rest)
    },


    test("enter → показывает экран с уровнем лабиринта") {
      for {
        quad                      <- makeState()
        (state, _, renderer, _)    = quad
        _                         <- state.enter(testUser, renderer)
        screens                   <- renderer.sentScreens
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.contains("лабиринт")) &&
              assertTrue(screens.head.text.contains("1")) &&
              assertTrue(screens.head.choices.map(_.id).contains("FindEvent")) &&
              assertTrue(screens.head.choices.map(_.id).contains("Rest"))
    },

    test("FindEvent → отправляет сообщение про коридоры, переходит в событие") {
      for {
        quad                      <- makeState()
        (state, _, renderer, _)    = quad
        result                    <- state.action(testUser, tap("FindEvent"), renderer)
        screens                   <- renderer.sentScreens
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(isValidFindOutcome(result))
    },

    test("каждое событие пула выпадает: по своему билету — своё состояние") {
      // У голого героя пул — ровно StateType.events, а индекс в нём подаём
      // сами. Берём по первому билету каждого события: их одиннадцать.
      val firstIdx = StateType.events.zipWithIndex.groupBy(_._1).map {
        case (event, tickets) => event -> tickets.map(_._2).min
      }
      def outcome(idx: Int, extra: Int*) =
        for {
          quad <- makeState()
          (state, _, renderer, _) = quad
          _    <- TestRandom.feedInts(idx +: extra: _*)
          res  <- state.action(testUser, tap("FindEvent"), renderer)
        } yield res
      val ordinary = firstIdx.toList.filterNot { case (event, _) =>
        event == StateType.Battle || event == StateType.Spring
      }
      for {
        // Девять событий уводят героя прямо в себя.
        plain  <- ZIO.foreach(ordinary) { case (event, i) => outcome(i).map(event -> _) }
        // Бой и ручей разыгрываются на месте: бой заводит сам бой, а ручей
        // лечит и бросает на засаду (99 — засады нет, герой остался в лабиринте).
        battle <- outcome(firstIdx(StateType.Battle))
        spring <- outcome(firstIdx(StateType.Spring), 99)
      } yield assertTrue(plain.size == 9 && plain.forall { case (event, got) => got == event }) &&
              assertTrue(battle == StateType.Battle) &&
              assertTrue(spring == StateType.Dungeon)
    },

    test("в пуле ровно одиннадцать событий на сто билетов — новое молча не добавить") {
      assertTrue(StateType.events.distinct.toSet == Set[StateType](
        StateType.Battle, StateType.MonsterCave, StateType.FlowerMeadow, StateType.FoundItem,
        StateType.Spring, StateType.Girl, StateType.SilverVein, StateType.Caravan,
        StateType.TreasureMobs, StateType.TreasureDig, StateType.ElementalLair)) &&
      assertTrue(StateType.events.size == 100 && StateType.events.distinct.size == 11)
    },

    test("приговор: навстречу выходит именное существо названного рода на четверти сил") {
      val race   = pangea.model.monster.Race.Orc
      val doomed = TestFixtures.hero(userId).copy(statBoosts = pangea.model.stats.StatBoosts.none.add(
        pangea.model.stats.StatBoost(pangea.model.item.BrewRates.SentenceBoost + race.entryName,
          pangea.model.stats.ParamsBuff.zero, pangea.model.item.BrewRates.SentenceMs), 0L))
      for {
        heroDao   <- TestHeroDao.withHero(userId, doomed)
        renderer  <- TestRenderer.make
        content   <- ZIO.attempt(SceneContent.load())
        scheduler <- TestScheduler.make
        state      = DungeonState(heroDao, TestInventoryRepository.accepting, scheduler, content)
        _         <- TestRandom.feedInts(67)
        result    <- state.action(testUser, tap("FindEvent"), renderer)
        battle    <- heroDao.readActiveBattle(userId)
                       .map(_.flatMap(_.as[pangea.model.battle.SoloPveBattle].toOption).get)
        updated   <- heroDao.getHeroByUserId(userId).map(_.get)
        now       <- zio.Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
        screens   <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
        full       = pangea.generator.monster.MonsterGenerator
                       .generateOfRaceAndRarity(doomed.dungeonLevel, race, pangea.model.monster.Rarity.Legendary)
      } yield assertTrue(result == StateType.Battle) &&
              // именное существо этого рода — легендарное, и оно уже изранено
              assertTrue(battle.monsterRace == race.entryName && battle.rarity == pangea.model.monster.Rarity.Legendary) &&
              assertTrue(battle.monsterCurrentHp == full.fightStats.hp * pangea.model.item.BrewRates.SentenceHpPct / 100L) &&
              assertTrue(battle.monsterCurrentArmor == full.fightStats.armor * pangea.model.item.BrewRates.SentenceHpPct / 100L) &&
              assertTrue(battle.escapesAfter == pangea.model.item.BrewRates.SentenceRounds) &&
              assertTrue(screens.contains("кровавому следу") && screens.contains(full.name)) &&
              // приговор сгорел на встрече
              assertTrue(!updated.statBoosts.hasActive(
                pangea.model.item.BrewRates.SentenceBoost + race.entryName, now))
    },

    test("волчий зов: вместо обычного события выходит Белый волк, и запах сгорает") {
      val called = TestFixtures.hero(userId).copy(statBoosts = pangea.model.stats.StatBoosts.none.add(
        pangea.model.stats.StatBoost(pangea.model.item.BrewRates.WolfCallBoost,
          pangea.model.stats.ParamsBuff.zero, pangea.model.item.BrewRates.WolfCallMs), 0L))
      for {
        heroDao   <- TestHeroDao.withHero(userId, called)
        renderer  <- TestRenderer.make
        content   <- ZIO.attempt(SceneContent.load())
        scheduler <- TestScheduler.make
        state      = DungeonState(heroDao, TestInventoryRepository.accepting, scheduler, content)
        // что бы ни выпало в пуле событий, на запах выходит волк
        _        <- TestRandom.feedInts(67)
        result   <- state.action(testUser, tap("FindEvent"), renderer)
        battle   <- heroDao.readActiveBattle(userId)
                      .map(_.flatMap(_.as[pangea.model.battle.SoloPveBattle].toOption))
        updated  <- heroDao.getHeroByUserId(userId).map(_.get)
        now      <- zio.Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
        screens  <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(battle.exists(_.bossKind.contains(pangea.model.monster.MiniBoss.WhiteWolf.entryName))) &&
              assertTrue(screens.contains("Белый волк")) &&
              // запах сгорел: второй раз волк сам не придёт
              assertTrue(!updated.statBoosts.hasActive(pangea.model.item.BrewRates.WolfCallBoost, now))
    },

    test("FindEvent Spring (индекс 67) → восстанавливает HP, без засады остаётся в Dungeon") {
      val lowHpHero = TestFixtures.hero(userId).copy(
        fightStats = TestFixtures.hero(userId).fightStats.copy(hp = 10L)
      )
      for {
        heroDao  <- TestHeroDao.withHero(userId, lowHpHero)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        scheduler <- TestScheduler.make
        invRepo   = TestInventoryRepository.accepting
        state     = DungeonState(heroDao, invRepo, scheduler, content)
        _        <- TestRandom.feedInts(67, 99) // 67 = Spring, 99 → roll 100 = нет засады
        result   <- state.action(testUser, tap("FindEvent"), renderer)
        updated  <- heroDao.getHeroByUserId(userId)
        screens  <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(updated.exists(_.fightStats.hp > 10L)) &&
              assertTrue(screens.exists(_.text.contains("ручеёк")))
    },

    test("FindEvent Spring → засада (roll ≤ 50) → начинается бой") {
      for {
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        scheduler <- TestScheduler.make
        invRepo   = TestInventoryRepository.accepting
        state     = DungeonState(heroDao, invRepo, scheduler, content)
        _        <- TestRandom.feedInts(67, 0) // 67 = Spring, 0 → roll 1 = засада
        _        <- TestRandom.feedLongs(42L)  // seed для монстра
        result   <- state.action(testUser, tap("FindEvent"), renderer)
        screens  <- renderer.sentScreens
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(screens.exists(_.text.contains("ручья")))
    },

    test("FindEvent Spring с экипированной флягой → фляга в слоте пополнена") {
      import pangea.model.item.{FlaskEffect, Item, ItemType, Rarity}
      val flask = Item(1L, "Фляга", 1L, Rarity.Gray, ItemType.Flask,
                   attack=0, accuracy=0, energy=0, armor=0, defence=0, evasion=0,
                   details = ItemDetails.Flask(FlaskEffect.HealPercent(25), charges = 0, maxCharges = 8))
      val heroWithEmptyFlask = TestFixtures.hero(userId).copy(
        fightStats = TestFixtures.hero(userId).fightStats.copy(hp = 10L),
        equipment  = TestFixtures.emptyEquipment.copy(flask = flask)
      )
      for {
        heroDao  <- TestHeroDao.withHero(userId, heroWithEmptyFlask)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        scheduler <- TestScheduler.make
        invRepo   = TestInventoryRepository.accepting
        state     = DungeonState(heroDao, invRepo, scheduler, content)
        _        <- TestRandom.feedInts(67, 99) // без засады
        result   <- state.action(testUser, tap("FindEvent"), renderer)
        updated  <- heroDao.getHeroByUserId(userId)
        screens  <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(updated.exists(h => flaskCharges(h.equipment.flask).contains(8))) &&
              assertTrue(screens.exists(_.text.contains("пополнена")))
    },

    test("FindEvent Spring → фляги в инвентаре пополнены") {
      import pangea.model.item.{FlaskEffect, Item, ItemType, Rarity}
      val flaskInInv = Item(2L, "Запасная фляга", 1L, Rarity.Gray, ItemType.Flask,
                        attack=0, accuracy=0, energy=0, armor=0, defence=0, evasion=0,
                        details = ItemDetails.Flask(FlaskEffect.HealPercent(25), charges = 0, maxCharges = 4))
      for {
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        scheduler <- TestScheduler.make
        invRepo   = TestInventoryRepository.withItems(List(flaskInInv))
        state     = DungeonState(heroDao, invRepo, scheduler, content)
        _        <- TestRandom.feedInts(67, 99) // без засады
        _        <- state.action(testUser, tap("FindEvent"), renderer)
      } yield assertTrue(invRepo.snapshot.find(_.id == 2L).exists(i => flaskCharges(i).contains(4)))
    },

    test("GoDarker → увеличивает уровень лабиринта на 1") {
      for {
        quad                          <- makeState(dungeonLevel = 5)
        (state, heroDao, renderer, _)  = quad
        result                        <- state.action(testUser, tap("GoDarker"), renderer)
        screens                       <- renderer.sentScreens
        updatedHero                   <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.contains("6")) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 6))
    },

    test("GoLighter → уменьшает уровень лабиринта на 1") {
      for {
        quad                          <- makeState(dungeonLevel = 5)
        (state, heroDao, renderer, _)  = quad
        result                        <- state.action(testUser, tap("GoLighter"), renderer)
        screens                       <- renderer.sentScreens
        updatedHero                   <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.contains("4")) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 4))
    },

    test("GoLighter на уровне 1 → остаётся на уровне 1") {
      for {
        quad                              <- makeState(dungeonLevel = 1)
        (state, heroDao, renderer, _)      = quad
        _                                 <- state.action(testUser, tap("GoLighter"), renderer)
        updatedHero                       <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updatedHero.exists(_.dungeonLevel == 1))
    },

    test("GoDarker на уровне 150 → дно лабиринта, выслеживание не запускается") {
      for {
        quad                              <- makeState(dungeonLevel = 150)
        (state, heroDao, renderer, _)      = quad
        result                            <- state.action(testUser, tap("GoDarker"), renderer)
        screens                           <- renderer.sentScreens
        updatedHero                       <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.exists(_.text.contains("самого дна"))) &&
              // экран обычный (не ожидание выслеживания)
              assertTrue(screens.last.choices.map(_.id).contains("FindEvent")) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 150))
    },

    test("enter на максимально доступном этаже → кнопка «к тьме» красная") {
      for {
        quad                      <- makeState(dungeonLevel = 5, maxDungeonLevel = 5)
        (state, _, renderer, _)    = quad
        _                         <- state.enter(testUser, renderer)
        screens                   <- renderer.sentScreens
      } yield assertTrue(colorOf(screens.head, "GoDarker").contains(ChoiceColor.Negative)) &&
              assertTrue(colorOf(screens.head, "GoLighter").contains(ChoiceColor.Positive))
    },

    test("enter на первом этаже → кнопка «к свету» красная") {
      for {
        quad                      <- makeState(dungeonLevel = 1, maxDungeonLevel = 3)
        (state, _, renderer, _)    = quad
        _                         <- state.enter(testUser, renderer)
        screens                   <- renderer.sentScreens
      } yield assertTrue(colorOf(screens.head, "GoLighter").contains(ChoiceColor.Negative)) &&
              assertTrue(colorOf(screens.head, "GoDarker").contains(ChoiceColor.Positive))
    },

    test("GoDarker без поверженной тьмы → запускает выслеживание (миазмы), планирует таймер, не двигается") {
      for {
        quad                                  <- makeState(dungeonLevel = 5, maxDungeonLevel = 5)
        (state, heroDao, renderer, scheduler)  = quad
        result                                <- state.action(testUser, tap("GoDarker"), renderer)
        screens                               <- renderer.sentScreens
        scheduled                             <- scheduler.scheduled
        updatedHero                           <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.exists(_.text.contains("миазмы тьмы"))) &&
              // на экране ожидания — только «перестать искать», без обычных действий и без «идти по следу»
              assertTrue(screens.last.choices.map(_.id) == List("StopTracking")) &&
              // запланирована push-задача пробуждения выслеживания
              assertTrue(scheduled.exists(_.kind == TaskKind.DarknessTracking)) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 5))
    },

    test("таймер выслеживания сработал → синтетический GoDarker выводит на Отмеченного целевого уровня (бой)") {
      import pangea.model.battle.SoloPveBattle
      for {
        quad                          <- makeState(dungeonLevel = 5, maxDungeonLevel = 5)
        (state, heroDao, renderer, _)  = quad
        _                             <- state.action(testUser, tap("GoDarker"), renderer) // старт таймера
        _                             <- TestClock.adjust(Duration.fromMillis(5L * 60L * 1000L)) // дедлайн наступил
        result                        <- state.action(testUser, tap("GoDarker"), renderer) // поллер шлёт GoDarker
        screens                       <- renderer.sentScreens
        battleJson                    <- heroDao.readActiveBattle(userId)
        battle                         = battleJson.flatMap(_.as[SoloPveBattle].toOption)
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(screens.exists(_.text.contains("Миазмы сгущаются"))) &&
              assertTrue(battle.exists(_.monsterMarked)) &&
              assertTrue(battle.exists(_.monsterLvl == 6L)) // цель = текущий (5) + 1
    },

    test("перестать искать → сбрасывает выслеживание, снимает таймер, возвращает обычный экран этажа") {
      for {
        quad                                  <- makeState(dungeonLevel = 5, maxDungeonLevel = 5)
        (state, heroDao, renderer, scheduler)  = quad
        _                                     <- state.action(testUser, tap("GoDarker"), renderer)       // старт
        result                                <- state.action(testUser, tap("StopTracking"), renderer)   // без переспроса
        screens                               <- renderer.sentScreens
        cancelled                             <- scheduler.cancelled
        updatedHero                           <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.exists(_.text.contains("рассеиваются"))) &&
              assertTrue(screens.last.choices.map(_.id).contains("FindEvent")) &&
              assertTrue(cancelled.contains(userId -> TaskKind.DarknessTracking)) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 5))
    },

    test("GoDarker с поверженной тьмой (этаж < max) → спускается глубже") {
      for {
        quad                          <- makeState(dungeonLevel = 5, maxDungeonLevel = 6)
        (state, heroDao, renderer, _)  = quad
        result                        <- state.action(testUser, tap("GoDarker"), renderer)
        updatedHero                   <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 6))
    },

    test("GoLighter на первом этаже → не двигается, сообщение «выше некуда»") {
      for {
        quad                          <- makeState(dungeonLevel = 1, maxDungeonLevel = 3)
        (state, heroDao, renderer, _)  = quad
        result                        <- state.action(testUser, tap("GoLighter"), renderer)
        screens                       <- renderer.sentScreens
        updatedHero                   <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.exists(_.text.contains("Выше уже некуда"))) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 1))
    },

    test("Rest → переходит в Rest без сообщений") {
      for {
        quad                      <- makeState()
        (state, _, renderer, _)    = quad
        result                    <- state.action(testUser, tap("Rest"), renderer)
        screens                   <- renderer.sentScreens
      } yield assertTrue(result == StateType.Rest) &&
              assertTrue(screens.isEmpty)
    },

    test("OpenCharacter → переходит в HeroStats и запоминает Dungeon как return_state") {
      for {
        quad                            <- makeState()
        (state, heroDao, renderer, _)    = quad
        result                          <- state.action(testUser, tap("OpenCharacter"), renderer)
        returnState                     <- heroDao.readReturnState(userId)
        screens                         <- renderer.sentScreens
      } yield assertTrue(result == StateType.HeroStats) &&
              assertTrue(returnState.contains(StateType.Dungeon)) &&
              assertTrue(screens.isEmpty)
    },

    test("неизвестный ввод → остаётся в Dungeon без сообщений") {
      for {
        quad                      <- makeState()
        (state, _, renderer, _)    = quad
        result                    <- state.action(testUser, UserAction("что угодно", None), renderer)
        screens                   <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.isEmpty)
    }
  )
}
