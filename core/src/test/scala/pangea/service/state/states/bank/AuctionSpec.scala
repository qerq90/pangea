package pangea.service.state.states.bank

import pangea.engine.{Choice, SceneContent}
import pangea.model.auction.{AuctionCurrency, AuctionLot, LotStatus}
import pangea.model.hero.HeroId
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.{ItemMenu, UserAction}
import pangea.test._
import zio.ZIO
import zio.test._

/** Аукцион Торгового дома: выставление за десятину, покупка с подтверждением,
  * гонка двух покупателей, свои лоты и непроданное. */
object AuctionSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val heroId   = HeroId(1L)
  private val seller   = HeroId(2L)
  private val sellerUs = UserId(2L)
  private val sellerUser = User(sellerUs, VkId("vk_seller"), TelegramId("tg_seller"))

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def text(t: String): UserAction = UserAction(t, None)

  private def gear(id: Long, name: String = "Меч"): Item =
    Item(id, name, lvl = 3L, Rarity.Blue, ItemType.Weapon,
      attack = 7, accuracy = 2, energy = 0, armor = 0, defence = 0, evasion = 0)

  private def lotOf(id: Long, price: Long = 100L, owner: HeroId = seller,
                    currency: AuctionCurrency = AuctionCurrency.Silver, expiresAt: Long = AuctionLot.LifetimeMs,
                    status: LotStatus = LotStatus.Active): AuctionLot =
    AuctionLot(id, owner, gear(50L + id, s"Топор $id"), price, currency, status, 0L, expiresAt, None)

  private def auction(
    inventory: List[Item] = Nil,
    lots:      List[AuctionLot] = Nil,
    silver:    Long = 0L,
    doubloons: Long = 0L
  ) =
    for {
      heroDao <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(silver = silver, doubloons = doubloons))
      // Второй герой — продавец: ему приходит выручка.
      _       <- heroDao.insertHero(TestFixtures.hero(sellerUs).copy(id = seller, silver = 0L, doubloons = 0L))
      invRepo  = TestInventoryRepository.withItems(inventory)
      aucRepo  = new TestAuctionRepository(lots)
      userRepo <- TestUserRepository.withUsers(testUser, sellerUser)
      players  = new TestPlayers
      renderer <- TestRenderer.make
      content <- ZIO.attempt(SceneContent.load())
    } yield (AuctionState(heroDao, invRepo, TestItemRepository.make, aucRepo, userRepo, players, content),
             heroDao, invRepo, aucRepo, players, renderer)

  override def spec = suite("Аукцион")(

    test("меню: лоты, выставить, мои лоты и назад") {
      for {
        t <- auction()
        (state, _, _, _, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
        back    <- state.action(testUser, tap("LeaveAuction"), renderer)
      } yield assertTrue(screens.last.choices.map(_.id) ==
                List("AuctionBrowse", "AuctionSell", "MyLots", "LeaveAuction")) &&
              assertTrue(back == StateType.TradeHouse)
    },

    test("витрина: восемь лотов на страницу, подписи в лимите кнопки") {
      val many = (1L to 12L).toList.map(lotOf(_))
      for {
        t <- auction(lots = many)
        (state, _, _, _, _, renderer) = t
        _       <- state.action(testUser, tap("AuctionBrowse"), renderer)
        first   <- renderer.sentScreens
        _       <- state.action(testUser, tap("AuctionNext"), renderer)
        second  <- renderer.sentScreens
      } yield assertTrue(first.last.choices.count(_.id == "BuyLot") == ItemMenu.DefaultPageSize) &&
              assertTrue(first.last.choices.forall(_.label.length <= Choice.MaxLabelLength)) &&
              assertTrue(first.last.choices.flatMap(_.row).max < 10) &&
              assertTrue(second.last.choices.count(_.id == "BuyLot") == 4)
    },

    test("номер лота текстом открывает карточку со всеми характеристиками") {
      for {
        t <- auction(lots = List(lotOf(7L)))
        (state, _, _, _, _, renderer) = t
        _       <- state.action(testUser, text("7"), renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.text.contains("№7") && screens.last.text.contains("⚔ +7")) &&
              assertTrue(screens.last.choices.map(_.id) == List("BuyLot", "Auction"))
    },

    suite("Выставление")(

      test("вещь уходит в лот, десятина — Рахадиму, объявление — в общий чат") {
        for {
          t <- auction(inventory = List(gear(42L, "Шлем")), silver = 1000L)
          (state, heroDao, inv, aucRepo, players, renderer) = t
          _     <- state.action(testUser, tap("AuctionSell"), renderer)
          _     <- state.action(testUser, tap("AucSell_42"), renderer)
          _     <- state.action(testUser, tap("SellCurrency", "cur" -> "Silver"), renderer)
          _     <- state.action(testUser, text("500"), renderer)
          _     <- state.action(testUser, tap("SellConfirm"), renderer)
          hero  <- heroDao.getHeroByUserId(userId)
          lots   = aucRepo.snapshot
        } yield assertTrue(lots.size == 1 && lots.head.price == 500L && lots.head.sellerId == heroId) &&
                assertTrue(lots.head.currency == AuctionCurrency.Silver && lots.head.item.id == 42L) &&
                assertTrue(inv.snapshot.isEmpty) &&
                assertTrue(hero.get.silver == 950L) &&   // сбор 10% от 500
                assertTrue(players.announced.exists(m => m.contains("Выставлен лот номер 1") && m.contains("Шлем")))
      },

      test("цена ниже минимума и не-число не проходят") {
        for {
          t <- auction(inventory = List(gear(42L)), silver = 1000L)
          (state, _, inv, aucRepo, _, renderer) = t
          _      <- state.action(testUser, tap("AucSell_42"), renderer)
          _      <- state.action(testUser, tap("SellCurrency", "cur" -> "Silver"), renderer)
          _      <- state.action(testUser, text("49"), renderer)
          low    <- renderer.sentScreens
          _      <- state.action(testUser, text("дорого"), renderer)
          nan    <- renderer.sentScreens
        } yield assertTrue(low.last.text.contains(AuctionLot.MinPrice.toString)) &&
                assertTrue(nan.last.text.contains("не число")) &&
                assertTrue(aucRepo.snapshot.isEmpty && inv.snapshot.size == 1)
      },

      test("невесомое добро на торги не попадает") {
        import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
        import pangea.model.skill.Skill
        val smallRune = RuneStone.item(Rune.Active(Skill.Bleeding), RuneStoneSize.Small).copy(id = 5L)
        for {
          t <- auction(inventory = List(smallRune, gear(42L)))
          (state, _, _, _, _, renderer) = t
          _       <- state.action(testUser, tap("AuctionSell"), renderer)
          screens <- renderer.sentScreens
        } yield assertTrue(screens.last.choices.map(_.id).contains("AucSell_42")) &&
                assertTrue(!screens.last.choices.map(_.id).contains("AucSell_5"))
      },

      test("нечем заплатить сбор — лота не будет") {
        for {
          t <- auction(inventory = List(gear(42L)), silver = 10L)
          (state, _, inv, aucRepo, _, renderer) = t
          _       <- state.action(testUser, tap("AucSell_42"), renderer)
          _       <- state.action(testUser, tap("SellCurrency", "cur" -> "Silver"), renderer)
          _       <- state.action(testUser, text("500"), renderer)
          _       <- state.action(testUser, tap("SellConfirm"), renderer)
          screens <- renderer.sentScreens
        } yield assertTrue(aucRepo.snapshot.isEmpty && inv.snapshot.size == 1) &&
                assertTrue(screens.last.text.contains("сбор"))
      }
    ),

    suite("Покупка")(

      test("подтверждение, деньги продавцу, вещь покупателю, колокольчик в личку") {
        for {
          t <- auction(lots = List(lotOf(1L, price = 300L)), silver = 1000L)
          (state, heroDao, inv, aucRepo, players, renderer) = t
          _        <- state.action(testUser, tap("BuyLot", "id" -> "1"), renderer)
          confirm  <- renderer.sentScreens
          _        <- state.action(testUser, tap("BuyLotYes", "id" -> "1"), renderer)
          buyer    <- heroDao.getHeroByUserId(userId)
          sellerH  <- heroDao.getHeroByUserId(sellerUs)
        } yield assertTrue(confirm.last.choices.map(_.id) == List("BuyLotYes", "Auction") && confirm.last.inline) &&
                assertTrue(buyer.get.silver == 700L && sellerH.get.silver == 300L) &&
                assertTrue(inv.snapshot.map(_.name) == List("Топор 1")) &&
                assertTrue(aucRepo.snapshot.head.status == LotStatus.Sold) &&
                // Продавцу — письмо с колокольчиком, покупателю — ничего лишнего.
                assertTrue(players.sentLetters.map(_._1) == List(sellerUs)) &&
                assertTrue(players.sentLetters.head._2.startsWith("🔔") &&
                           players.sentLetters.head._2.contains("№1") &&
                           players.sentLetters.head._2.contains("🪙 300"))
      },

      test("несостоявшаяся покупка писем не шлёт") {
        for {
          t <- auction(lots = List(lotOf(1L, status = LotStatus.Sold)), silver = 1000L)
          (state, _, _, _, players, renderer) = t
          _ <- state.action(testUser, tap("BuyLotYes", "id" -> "1"), renderer)
        } yield assertTrue(players.sentLetters.isEmpty)
      },

      test("лот, купленный секунду назад, второму не достанется и денег не спишет") {
        for {
          t <- auction(lots = List(lotOf(1L, price = 300L, status = LotStatus.Sold)), silver = 1000L)
          (state, heroDao, inv, _, _, renderer) = t
          _       <- state.action(testUser, tap("BuyLotYes", "id" -> "1"), renderer)
          screens <- renderer.sentScreens
          hero    <- heroDao.getHeroByUserId(userId)
        } yield assertTrue(screens.last.text.contains("уже ушёл")) &&
                assertTrue(hero.get.silver == 1000L && inv.snapshot.isEmpty)
      },

      test("свой лот не купишь") {
        for {
          t <- auction(lots = List(lotOf(1L, owner = heroId)), silver = 1000L)
          (state, heroDao, _, _, _, renderer) = t
          _       <- state.action(testUser, tap("BuyLotYes", "id" -> "1"), renderer)
          screens <- renderer.sentScreens
          hero    <- heroDao.getHeroByUserId(userId)
        } yield assertTrue(screens.last.text.contains("Свой же лот")) && assertTrue(hero.get.silver == 1000L)
      },

      test("не хватает денег и нет места в сумке — отказ без списаний") {
        val full = (1L to 20L).toList.map(i => gear(100L + i, s"Хлам $i"))
        for {
          t <- auction(lots = List(lotOf(1L, price = 300L)), silver = 100L)
          (state, heroDao, _, aucRepo, _, renderer) = t
          _      <- state.action(testUser, tap("BuyLotYes", "id" -> "1"), renderer)
          poor   <- renderer.sentScreens
          t2 <- auction(inventory = full, lots = List(lotOf(1L, price = 300L)), silver = 1000L)
          (state2, _, _, aucRepo2, _, renderer2) = t2
          _      <- state2.action(testUser, tap("BuyLotYes", "id" -> "1"), renderer2)
          noRoom <- renderer2.sentScreens
          hero   <- heroDao.getHeroByUserId(userId)
        } yield assertTrue(poor.last.text.contains("не наберётся") && hero.get.silver == 100L) &&
                assertTrue(aucRepo.snapshot.head.status == LotStatus.Active) &&
                assertTrue(noRoom.last.text.contains("нет места") && aucRepo2.snapshot.head.status == LotStatus.Active)
      },

      test("за дублоны: списываются дублоны, серебро не трогаем") {
        for {
          t <- auction(lots = List(lotOf(1L, price = 60L, currency = AuctionCurrency.Doubloons)),
                       silver = 500L, doubloons = 100L)
          (state, heroDao, _, _, _, renderer) = t
          _       <- state.action(testUser, tap("BuyLotYes", "id" -> "1"), renderer)
          buyer   <- heroDao.getHeroByUserId(userId)
          sellerH <- heroDao.getHeroByUserId(sellerUs)
        } yield assertTrue(buyer.get.doubloons == 40L && buyer.get.silver == 500L) &&
                assertTrue(sellerH.get.doubloons == 60L)
      }
    ),

    suite("Свои лоты")(

      test("свой лот снимается с торгов — вещь возвращается в сумку") {
        for {
          t <- auction(lots = List(lotOf(1L, owner = heroId)))
          (state, _, inv, aucRepo, _, renderer) = t
          _       <- state.action(testUser, tap("MyLots"), renderer)
          mine    <- renderer.sentScreens
          _       <- state.action(testUser, tap("Reclaim", "id" -> "1"), renderer)
        } yield assertTrue(mine.last.choices.map(_.id) == List("Reclaim", "Auction")) &&
                assertTrue(aucRepo.snapshot.head.status == LotStatus.Returned) &&
                assertTrue(inv.snapshot.map(_.name) == List("Топор 1"))
      },

      test("через неделю лот уходит в непроданные: не купить, но забрать можно") {
        val stale = lotOf(1L, owner = heroId, expiresAt = 0L)
        for {
          t <- auction(lots = List(stale))
          (state, _, inv, aucRepo, _, renderer) = t
          _        <- state.action(testUser, tap("AuctionBrowse"), renderer)
          showcase <- renderer.sentScreens
          _        <- state.action(testUser, tap("MyLots"), renderer)
          mine     <- renderer.sentScreens
          _        <- state.action(testUser, tap("Reclaim", "id" -> "1"), renderer)
        } yield assertTrue(showcase.last.text.contains("Прилавки пусты")) &&
                assertTrue(mine.last.text.contains("не продан")) &&
                assertTrue(aucRepo.snapshot.head.status == LotStatus.Returned && inv.snapshot.size == 1)
      },

      test("чужой лот не снять") {
        for {
          t <- auction(lots = List(lotOf(1L)))
          (state, _, inv, aucRepo, _, renderer) = t
          _       <- state.action(testUser, tap("Reclaim", "id" -> "1"), renderer)
          screens <- renderer.sentScreens
        } yield assertTrue(screens.last.text.contains("не ваш лот")) &&
                assertTrue(aucRepo.snapshot.head.status == LotStatus.Active && inv.snapshot.isEmpty)
      }
    ),

    test("цена, сбор и срок жизни лота") {
      assertTrue(AuctionLot.MinPrice == 50L && AuctionLot.FeePct == 10L) &&
      assertTrue(AuctionLot.fee(500L) == 50L && AuctionLot.fee(50L) == 5L && AuctionLot.fee(1L) == 1L) &&
      assertTrue(AuctionLot.LifetimeMs == 7L * 24L * 3600L * 1000L) &&
      assertTrue(AuctionLot.announcement(lotOf(3L)).startsWith("📢 Выставлен лот номер 3"))
    }
  )
}
