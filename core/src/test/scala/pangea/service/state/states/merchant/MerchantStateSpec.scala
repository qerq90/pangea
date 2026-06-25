package pangea.service.state.states.merchant

import pangea.engine.SceneContent
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.merchant.MerchantState.MerchantData
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.{Task, ZIO}
import zio.test._

object MerchantStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def tapIdx(key: String, idx: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","idx":"$idx"}"""))

  private def richHero = TestFixtures.hero(userId).copy(lvl = 10L, gold = 1000000L)
  private def poorHero = TestFixtures.hero(userId).copy(lvl = 10L, gold = 0L)

  private def makeState(hero: pangea.model.hero.Hero, items: List[Item] = Nil) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      invRepo   = TestInventoryRepository.withItems(items)
      itemRepo  = TestItemRepository.make
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (MerchantState(heroDao, invRepo, itemRepo, content), heroDao, invRepo, renderer)

  private def readMerchant(heroDao: TestHeroDao): Task[MerchantData] =
    heroDao.readMerchantData(userId)
      .map(_.flatMap(_.as[MerchantData].toOption))
      .flatMap(o => ZIO.fromOption(o).orElseFail(new Throwable("no merchant data")))

  override def spec = suite("MerchantState")(

    test("enter → раскладывает 3 предмета и кнопки Купить/Обновить/Продать/Назад") {
      for {
        t <- makeState(richHero)
        (state, heroDao, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
        data    <- readMerchant(heroDao)
        ids      = screens.last.choices.map(_.id)
      } yield assertTrue(data.items.size == 3) &&
              assertTrue(ids.count(_ == "Buy") == 3) &&
              assertTrue(ids.contains("Refresh") && ids.contains("Sell") && ids.contains("Back"))
    },

    test("предметы зафиксированы: повторный enter не меняет сток") {
      for {
        t <- makeState(richHero)
        (state, heroDao, _, renderer) = t
        _     <- state.enter(testUser, renderer)
        data1 <- readMerchant(heroDao)
        _     <- state.enter(testUser, renderer)
        data2 <- readMerchant(heroDao)
      } yield assertTrue(data1.items.map(_.item.name) == data2.items.map(_.item.name)) &&
              assertTrue(data1.refreshedAt == data2.refreshedAt)
    },

    test("Buy → экран подтверждения с ценой") {
      for {
        t <- makeState(richHero)
        (state, heroDao, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        data    <- readMerchant(heroDao)
        _       <- state.action(testUser, tapIdx("Buy", 0), renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.text.contains("уверены")) &&
              assertTrue(screens.last.text.contains(data.items.head.price.toString)) &&
              assertTrue(screens.last.choices.map(_.id).toSet == Set("ConfirmBuy", "CancelBuy"))
    },

    test("ConfirmBuy с достаточным золотом → списывает цену, кладёт предмет, помечает купленным") {
      for {
        t <- makeState(richHero)
        (state, heroDao, invRepo, renderer) = t
        _       <- state.enter(testUser, renderer)
        data    <- readMerchant(heroDao)
        price    = data.items.head.price
        _       <- state.action(testUser, tapIdx("ConfirmBuy", 0), renderer)
        hero    <- heroDao.getHeroByUserId(userId)
        data2   <- readMerchant(heroDao)
        screens <- renderer.sentScreens
      } yield assertTrue(invRepo.snapshot.size == 1) &&
              assertTrue(hero.exists(_.gold == 1000000L - price)) &&
              assertTrue(data2.items.head.bought) &&
              assertTrue(screens.exists(_.text.contains("купили")))
    },

    test("ConfirmBuy без золота → сообщение «как будет достаточно звонких монет», ничего не списано") {
      for {
        t <- makeState(poorHero)
        (state, heroDao, invRepo, renderer) = t
        _       <- state.enter(testUser, renderer)
        _       <- state.action(testUser, tapIdx("ConfirmBuy", 0), renderer)
        hero    <- heroDao.getHeroByUserId(userId)
        data    <- readMerchant(heroDao)
        screens <- renderer.sentScreens
      } yield assertTrue(invRepo.snapshot.isEmpty) &&
              assertTrue(hero.exists(_.gold == 0L)) &&
              assertTrue(!data.items.head.bought) &&
              assertTrue(screens.exists(_.text.contains("звонких монет")))
    },

    test("Refresh в пределах часа → сообщение про кулдаун, сток не меняется") {
      for {
        t <- makeState(richHero)
        (state, heroDao, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        data1   <- readMerchant(heroDao)
        _       <- state.action(testUser, tap("Refresh"), renderer)
        data2   <- readMerchant(heroDao)
        screens <- renderer.sentScreens
      } yield assertTrue(data1.items.map(_.item.name) == data2.items.map(_.item.name)) &&
              assertTrue(screens.exists(_.text.contains("Загляните")))
    },

    test("Sell → показывает предмет и цену продажи; SellItem начисляет золото и убирает предмет") {
      // green helmet lvl 5 → sellPrice = (5+5)*1.2*4 = 48
      val helmet = Item(7L, "Шлем", 5L, Rarity.Green, ItemType.Helmet,
        attack = 0, accuracy = 0, concentration = 0, armor = 10, defence = 1, evasion = 0)
      for {
        t <- makeState(richHero.copy(gold = 100L), items = List(helmet))
        (state, heroDao, invRepo, renderer) = t
        _       <- state.action(testUser, tap("Sell"), renderer)
        sellScr <- renderer.sentScreens.map(_.last)
        _       <- state.action(testUser, tap("SellItem"), renderer)
        hero    <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(sellScr.text.contains("48")) &&
              assertTrue(invRepo.snapshot.isEmpty) &&
              assertTrue(hero.exists(_.gold == 100L + 48L))
    },

    test("всё снаряжение куплено → сообщение «занят подготовкой новой партии», есть Продать/Назад") {
      for {
        t <- makeState(richHero)
        (state, _, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        _       <- state.action(testUser, tapIdx("ConfirmBuy", 0), renderer)
        _       <- state.action(testUser, tapIdx("ConfirmBuy", 1), renderer)
        _       <- state.action(testUser, tapIdx("ConfirmBuy", 2), renderer)
        screens <- renderer.sentScreens
        ids      = screens.last.choices.map(_.id)
      } yield assertTrue(screens.last.text.contains("занят подготовкой новой партии")) &&
              assertTrue(!ids.contains("Buy")) &&
              assertTrue(ids.contains("Sell") && ids.contains("Back"))
    },

    test("Back → переход в GlobalMap") {
      for {
        t <- makeState(richHero)
        (state, _, _, renderer) = t
        result <- state.action(testUser, tap("Back"), renderer)
      } yield assertTrue(result == StateType.GlobalMap)
    }
  )
}
