package pangea.service.state.states.events.item

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.events.item.FoundItemState.FoundItemData
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestJournal, TestRenderer}
import zio.test._
import zio.ZIO

object FoundItemStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def makeState(canAddItem: Boolean = true) =
    for {
      renderer <- TestRenderer.make
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
      journal  <- TestJournal.make
      content  <- ZIO.attempt(SceneContent.load())
      invRepo   = if (canAddItem) TestInventoryRepository.accepting else TestInventoryRepository.full
      itemRepo  = TestItemRepository.make
      state     = FoundItemState(heroDao, invRepo, itemRepo, journal, content)
    } yield (state, renderer, heroDao)

  // Находка известна заранее: кладём её в scene_data сами, как это сделал бы enter.
  private def helmet(lvl: Long = 1L) =
    Item(77L, "Найденный шлем", lvl, Rarity.White, ItemType.Helmet,
      attack = 0, accuracy = 0, energy = 0, armor = 4, defence = 0, evasion = 0)

  private def withFound(item: Item, hero: Hero = TestFixtures.hero(userId), canAddItem: Boolean = true,
                        bag: List[Item] = Nil) =
    for {
      renderer <- TestRenderer.make
      heroDao  <- TestHeroDao.withHero(userId, hero)
      _        <- heroDao.writeSceneData(userId, FoundItemData(item).asJson)
      journal  <- TestJournal.make
      content  <- ZIO.attempt(SceneContent.load())
      invRepo   = if (canAddItem) TestInventoryRepository.withItems(bag) else TestInventoryRepository.full
      state     = FoundItemState(heroDao, invRepo, TestItemRepository.make, journal, content)
    } yield (state, renderer, heroDao, invRepo)

  private def tapSlot(key: String, slot: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","slot":"$slot"}"""))

  // Герой в старом шлеме — находке остаётся встать вместо него.
  private def busy: Hero = {
    val h = TestFixtures.hero(userId)
    h.copy(equipment = h.equipment.copy(helmet = helmet().copy(id = 5L, name = "Старый шлем")))
  }

  override def spec = suite("FoundItemState")(

    test("находка встаёт в пустой слот → первой кнопкой «Забрать и надеть»; не по уровню — кнопки нет") {
      for {
        fit     <- withFound(helmet())
        (fs, fr, _, _) = fit
        _       <- fs.action(testUser, UserAction("что угодно", None), fr)
        fitIds  <- fr.sentScreens.map(_.last.choices.map(_.id))
        high    <- withFound(helmet(lvl = 3L))
        (hs, hr, _, _) = high
        _       <- hs.action(testUser, UserAction("что угодно", None), hr)
        highIds <- hr.sentScreens.map(_.last.choices.map(_.id))
      } yield assertTrue(fitIds == List("TakeWearItem", "TakeItem", "DontTakeItem")) &&
              assertTrue(highIds == List("TakeItem", "DontTakeItem"))
    },

    test("«Забрать и надеть» → находка на герое, сумка пуста, «сразу надели», уход в Dungeon") {
      for {
        t <- withFound(helmet())
        (state, renderer, heroDao, invRepo) = t
        result <- state.action(testUser, tap("TakeWearItem"), renderer)
        hero   <- heroDao.getHeroByUserId(userId)
        text   <- renderer.sentScreens.map(_.map(_.text).mkString(" | "))
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(hero.exists(_.equipment.helmet.id == 77L)) &&
              assertTrue(hero.exists(_.fightStats.armor == 4L)) &&
              assertTrue(invRepo.snapshot.isEmpty) &&
              assertTrue(text.contains("Вы получили Найденный шлем и сразу надели."))
    },

    test("«Забрать и надеть» при полной сумке → находка всё равно надета") {
      for {
        t <- withFound(helmet(), canAddItem = false)
        (state, renderer, heroDao, _) = t
        _    <- state.action(testUser, tap("TakeWearItem"), renderer)
        hero <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(hero.exists(_.equipment.helmet.id == 77L))
    },

    test("слот занят → «Забрать и надеть» есть; нажатие — находка вместо старого шлема, старый в сумке") {
      for {
        t <- withFound(helmet(), hero = busy)
        (state, renderer, heroDao, invRepo) = t
        _      <- state.action(testUser, UserAction("что угодно", None), renderer)
        ids    <- renderer.sentScreens.map(_.last.choices.map(_.id))
        result <- state.action(testUser, tap("TakeWearItem"), renderer)
        hero   <- heroDao.getHeroByUserId(userId)
        text   <- renderer.sentScreens.map(_.map(_.text).mkString(" | "))
      } yield assertTrue(ids.headOption.contains("TakeWearItem")) &&
              assertTrue(result == StateType.Dungeon) &&
              assertTrue(hero.exists(_.equipment.helmet.id == 77L)) &&
              assertTrue(invRepo.snapshot.map(_.id) == List(5L)) &&
              assertTrue(text.contains("сразу надели") && text.contains("🎒 Снято в сумку: Старый шлем"))
    },

    test("слот занят, сумка полна → отказ, находка остаётся на экране, шлем прежний") {
      val full = (1L to 20L).toList.map(i => helmet().copy(id = 100L + i, itemType = ItemType.Boots))
      for {
        t <- withFound(helmet(), hero = busy, bag = full)
        (state, renderer, heroDao, invRepo) = t
        result <- state.action(testUser, tap("TakeWearItem"), renderer)
        hero   <- heroDao.getHeroByUserId(userId)
        text   <- renderer.sentScreens.map(_.map(_.text).mkString(" | "))
      } yield assertTrue(result == StateType.FoundItem) &&
              assertTrue(text.contains("Сумка полна — снятой вещи некуда деться")) &&
              assertTrue(hero.exists(_.equipment.helmet.id == 5L)) &&
              assertTrue(invRepo.snapshot.size == 20)
    },

    test("два кольца надеты → спрашиваем, какое снять; выбор первого — меняется первое") {
      val h     = TestFixtures.hero(userId)
      val found = helmet().copy(itemType = ItemType.Ring, name = "Найденное кольцо")
      val rings = h.copy(equipment = h.equipment.copy(
                    firstRing  = helmet().copy(id = 6L, itemType = ItemType.Ring, name = "Кольцо раз"),
                    secondRing = helmet().copy(id = 7L, itemType = ItemType.Ring, name = "Кольцо два")))
      for {
        t <- withFound(found, hero = rings)
        (state, renderer, heroDao, invRepo) = t
        asked  <- state.action(testUser, tap("TakeWearItem"), renderer)
        choice <- renderer.sentScreens.map(_.last)
        result <- state.action(testUser, tapSlot("TakeWearItem", 1), renderer)
        hero   <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(asked == StateType.FoundItem) &&
              assertTrue(choice.choices.map(_.id) == List("TakeWearItem", "TakeWearItem", "FoundBack")) &&
              assertTrue(result == StateType.Dungeon) &&
              assertTrue(hero.exists(h => h.equipment.firstRing.id == 77L && h.equipment.secondRing.id == 7L)) &&
              assertTrue(invRepo.snapshot.map(_.id) == List(6L))
    },

    test("enter → сохраняет предмет в scene_data, показывает экран находки") {
      for {
        triple                    <- makeState()
        (state, renderer, heroDao) = triple
        _                         <- state.enter(testUser, renderer)
        screens                   <- renderer.sentScreens
        sceneData                 <- heroDao.readSceneData(userId)
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.contains("находите предмет")) &&
              assertTrue(sceneData.isDefined)
    },

    test("TakeItem → берёт предмет из scene_data, отправляет подтверждение, переходит в Dungeon") {
      for {
        triple              <- makeState()
        (state, renderer, _) = triple
        _                   <- state.enter(testUser, renderer)
        result              <- state.action(testUser, tap("TakeItem"), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.exists(_.text.startsWith("Вы получили")))
    },

    test("TakeItem при полном инвентаре → сообщает об ошибке, переходит в Dungeon") {
      for {
        triple              <- makeState(canAddItem = false)
        (state, renderer, _) = triple
        _                   <- state.enter(testUser, renderer)
        result              <- state.action(testUser, tap("TakeItem"), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.exists(_.text.contains("Сумка странника переполнена")))
    },

    test("DontTakeItem → отказывается от предмета, переходит в Dungeon") {
      for {
        triple              <- makeState()
        (state, renderer, _) = triple
        _                   <- state.enter(testUser, renderer)
        result              <- state.action(testUser, tap("DontTakeItem"), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.exists(_.text.contains("уходите")))
    },

    test("неизвестный ввод → перепоказывает экран находки, остаётся в FoundItem") {
      for {
        triple              <- makeState()
        (state, renderer, _) = triple
        result              <- state.action(testUser, UserAction("что угодно", None), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.FoundItem) &&
              assertTrue(screens.nonEmpty)
    },

    test("enter → экран содержит имя предмета и кнопки Взять/Оставить") {
      for {
        triple                    <- makeState()
        (state, renderer, heroDao) = triple
        _                         <- state.enter(testUser, renderer)
        screens                   <- renderer.sentScreens
        sceneData                 <- heroDao.readSceneData(userId)
      } yield assertTrue(screens.head.choices.map(_.id).contains("TakeItem")) &&
              assertTrue(screens.head.choices.map(_.id).contains("DontTakeItem")) &&
              assertTrue(sceneData.isDefined)
    },

    test("enter → предмет в scene_data имеет реальный ID (не -1)") {
      import io.circe.parser.decode
      import pangea.service.state.states.events.item.FoundItemState.FoundItemData
      for {
        triple                    <- makeState()
        (state, renderer, heroDao) = triple
        _                         <- state.enter(testUser, renderer)
        sceneData                 <- heroDao.readSceneData(userId)
        item                       = sceneData.flatMap(json => decode[FoundItemData](json.noSpaces).toOption).map(_.item)
      } yield assertTrue(item.isDefined) &&
              assertTrue(item.get.id > 0L)
    },

    test("TakeItem → предмет в инвентаре имеет реальный ID") {
      for {
        renderer <- TestRenderer.make
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        journal  <- TestJournal.make
        content  <- ZIO.attempt(SceneContent.load())
        invRepo   = TestInventoryRepository.accepting
        itemRepo  = TestItemRepository.make
        state     = FoundItemState(heroDao, invRepo, itemRepo, journal, content)
        _        <- state.enter(testUser, renderer)
        _        <- state.action(testUser, tap("TakeItem"), renderer)
        items     = invRepo.snapshot
      } yield assertTrue(items.nonEmpty) &&
              assertTrue(items.forall(_.id > 0L))
    }
  )
}
