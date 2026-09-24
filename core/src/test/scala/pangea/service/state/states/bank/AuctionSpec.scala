package pangea.service.state.states.bank

import pangea.engine.{Choice, SceneContent}
import pangea.model.auction.{AuctionCurrency, AuctionLot}
import pangea.model.hero.HeroId
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.payout.Payouts
import pangea.service.state.{ItemMenu, UserAction}
import pangea.test._
import zio.ZIO
import zio.test._

/** Аукцион Торгового дома: торгуют только с ячейкой, лотов на руках не больше
  * десяти, выручка ложится в ячейку, а что в неё не влезло — ждёт в городе. */
object AuctionSpec extends ZIOSpecDefault {

  private val userId     = UserId(1L)
  private val testUser   = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val heroId     = HeroId(1L)
  private val seller     = HeroId(2L)
  private val sellerUs   = UserId(2L)
  private val sellerUser = User(sellerUs, VkId("vk_seller"), TelegramId("tg_seller"))

  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def text(t: String): UserAction = UserAction(t, None)

  private def gear(id: Long, name: String = "Меч"): Item =
    Item(id, name, lvl = 3L, Rarity.Blue, ItemType.Weapon,
      attack = 7, accuracy = 2, energy = 0, armor = 0, defence = 0, evasion = 0)

  private def lotOf(id: Long, price: Long = 100L, owner: HeroId = seller,
                    currency: AuctionCurrency = AuctionCurrency.Silver,
                    expiresAt: Long = AuctionLot.LifetimeMs): AuctionLot =
    AuctionLot(id, owner, gear(50L + id, s"Топор $id"), price, currency, 0L, expiresAt)

  private case class Fix(
    state:    AuctionState,
    heroDao:  TestHeroDao,
    inv:      TestInventoryRepository,
    lots:     TestAuctionRepository,
    bank:     TestBankRepository,
    payouts:  TestPayoutDao,
    players:  TestPlayers,
    renderer: TestRenderer
  )

  private def auction(
    inventory:   List[Item]       = Nil,
    lots:        List[AuctionLot] = Nil,
    silver:      Long             = 0L,
    doubloons:   Long             = 0L,
    cells:       Int              = 1,
    vaultSilver: Long             = 0L
  ) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(silver = silver, doubloons = doubloons))
      // Второй герой — продавец: ему приходит выручка.
      _        <- heroDao.insertHero(TestFixtures.hero(sellerUs).copy(id = seller, silver = 0L, doubloons = 0L))
      invRepo   = TestInventoryRepository.withItems(inventory)
      aucRepo   = new TestAuctionRepository(lots)
      bankRepo  = TestBankRepository.of(cells, silver = vaultSilver)
      payoutDao = TestPayoutDao.empty
      userRepo <- TestUserRepository.withUsers(testUser, sellerUser)
      players   = new TestPlayers
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
      payouts   = Payouts(payoutDao, heroDao, content)
    } yield Fix(
      AuctionState(heroDao, invRepo, TestItemRepository.make, aucRepo, userRepo, bankRepo, payouts, players, content),
      heroDao, invRepo, aucRepo, bankRepo, payoutDao, players, renderer)

  override def spec = suite("Аукцион")(

    test("без ячейки в банке на торги не пускают, но уйти можно") {
      for {
        f       <- auction(cells = 0)
        _       <- f.state.enter(testUser, f.renderer)
        screens <- f.renderer.sentScreens
        _       <- f.state.action(testUser, tap("AuctionBrowse"), f.renderer)
        browse  <- f.renderer.sentScreens
        back    <- f.state.action(testUser, tap("LeaveAuction"), f.renderer)
      } yield assertTrue(screens.last.text.contains("ячейка в Торговом доме")) &&
              assertTrue(screens.last.choices.map(_.id) == List("LeaveAuction")) &&
              assertTrue(browse.last.choices.map(_.id) == List("LeaveAuction")) &&
              assertTrue(back == StateType.TradeHouse)
    },

    test("меню: лоты, выставить, мои лоты и назад") {
      for {
        f       <- auction()
        _       <- f.state.enter(testUser, f.renderer)
        screens <- f.renderer.sentScreens
      } yield assertTrue(screens.last.choices.map(_.id) ==
                List("AuctionBrowse", "AuctionSell", "MyLots", "LeaveAuction"))
    },

    test("витрина: восемь лотов на страницу, подписи в лимите кнопки") {
      val many = (1L to 12L).toList.map(lotOf(_))
      for {
        f      <- auction(lots = many)
        _      <- f.state.action(testUser, tap("AuctionBrowse"), f.renderer)
        first  <- f.renderer.sentScreens
        _      <- f.state.action(testUser, tap("AuctionNext"), f.renderer)
        second <- f.renderer.sentScreens
      } yield assertTrue(first.last.choices.count(_.id == "BuyLot") == ItemMenu.DefaultPageSize) &&
              assertTrue(first.last.choices.forall(_.label.length <= Choice.MaxLabelLength)) &&
              assertTrue(first.last.choices.flatMap(_.row).max < 10) &&
              assertTrue(second.last.choices.count(_.id == "BuyLot") == 4)
    },

    test("номер лота текстом открывает карточку со всеми характеристиками") {
      for {
        f       <- auction(lots = List(lotOf(7L)))
        _       <- f.state.action(testUser, text("7"), f.renderer)
        screens <- f.renderer.sentScreens
      } yield assertTrue(screens.last.text.contains("№7") && screens.last.text.contains("⚔ +7")) &&
              assertTrue(screens.last.choices.map(_.id) == List("BuyLot", "Auction"))
    },

    suite("Выставление")(

      test("вещь уходит в лот, десятина — Рахадиму, объявление — в общий чат") {
        for {
          f    <- auction(inventory = List(gear(42L, "Шлем")), silver = 1000L)
          _    <- f.state.action(testUser, tap("AuctionSell"), f.renderer)
          _    <- f.state.action(testUser, tap("AucSell_42"), f.renderer)
          _    <- f.state.action(testUser, tap("SellCurrency", "cur" -> "Silver"), f.renderer)
          _    <- f.state.action(testUser, text("500"), f.renderer)
          _    <- f.state.action(testUser, tap("SellConfirm"), f.renderer)
          hero <- f.heroDao.getHeroByUserId(userId)
          lots  = f.lots.snapshot
        } yield assertTrue(lots.size == 1 && lots.head.price == 500L && lots.head.sellerId == heroId) &&
                assertTrue(lots.head.currency == AuctionCurrency.Silver && lots.head.item.id == 42L) &&
                assertTrue(f.inv.snapshot.isEmpty) &&
                assertTrue(hero.get.silver == 950L) &&   // сбор 10% от 500
                assertTrue(f.players.announced.exists(m => m.contains("Выставлен лот номер 1") && m.contains("Шлем")))
      },

      test("больше десяти лотов на торгах не держат") {
        val ten = (1L to AuctionLot.MaxLots.toLong).toList.map(lotOf(_, owner = heroId))
        for {
          f       <- auction(inventory = List(gear(42L)), lots = ten, silver = 1000L)
          _       <- f.state.action(testUser, tap("AucSell_42"), f.renderer)
          _       <- f.state.action(testUser, tap("SellCurrency", "cur" -> "Silver"), f.renderer)
          _       <- f.state.action(testUser, text("500"), f.renderer)
          _       <- f.state.action(testUser, tap("SellConfirm"), f.renderer)
          screens <- f.renderer.sentScreens
          hero    <- f.heroDao.getHeroByUserId(userId)
        } yield assertTrue(screens.last.text.contains(AuctionLot.MaxLots.toString)) &&
                assertTrue(f.lots.snapshot.size == AuctionLot.MaxLots) &&
                // ни вещи, ни сбора: отказ до всяких списаний
                assertTrue(f.inv.snapshot.map(_.id) == List(42L) && hero.get.silver == 1000L)
      },

      test("цена ниже минимума и не-число не проходят") {
        for {
          f   <- auction(inventory = List(gear(42L)), silver = 1000L)
          _   <- f.state.action(testUser, tap("AucSell_42"), f.renderer)
          _   <- f.state.action(testUser, tap("SellCurrency", "cur" -> "Silver"), f.renderer)
          _   <- f.state.action(testUser, text("49"), f.renderer)
          low <- f.renderer.sentScreens
          _   <- f.state.action(testUser, text("дорого"), f.renderer)
          nan <- f.renderer.sentScreens
        } yield assertTrue(low.last.text.contains(AuctionLot.MinPrice.toString)) &&
                assertTrue(nan.last.text.contains("не число")) &&
                assertTrue(f.lots.snapshot.isEmpty && f.inv.snapshot.size == 1)
      },

      test("невесомое добро на торги не попадает") {
        import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
        import pangea.model.skill.Skill
        val smallRune = RuneStone.item(Rune.Active(Skill.Bleeding), RuneStoneSize.Small).copy(id = 5L)
        for {
          f       <- auction(inventory = List(smallRune, gear(42L)))
          _       <- f.state.action(testUser, tap("AuctionSell"), f.renderer)
          screens <- f.renderer.sentScreens
        } yield assertTrue(screens.last.choices.map(_.id).contains("AucSell_42")) &&
                assertTrue(!screens.last.choices.map(_.id).contains("AucSell_5"))
      },

      test("нечем заплатить сбор — лота не будет") {
        for {
          f       <- auction(inventory = List(gear(42L)), silver = 10L)
          _       <- f.state.action(testUser, tap("AucSell_42"), f.renderer)
          _       <- f.state.action(testUser, tap("SellCurrency", "cur" -> "Silver"), f.renderer)
          _       <- f.state.action(testUser, text("500"), f.renderer)
          _       <- f.state.action(testUser, tap("SellConfirm"), f.renderer)
          screens <- f.renderer.sentScreens
        } yield assertTrue(f.lots.snapshot.isEmpty && f.inv.snapshot.size == 1) &&
                assertTrue(screens.last.text.contains("сбор"))
      }
    ),

    suite("Покупка")(

      test("выручка ложится в ячейку продавца, а лот пропадает с торгов") {
        for {
          f       <- auction(lots = List(lotOf(1L, price = 300L)), silver = 1000L)
          _       <- f.state.action(testUser, tap("BuyLot", "id" -> "1"), f.renderer)
          confirm <- f.renderer.sentScreens
          _       <- f.state.action(testUser, tap("BuyLotYes", "id" -> "1"), f.renderer)
          buyer   <- f.heroDao.getHeroByUserId(userId)
          sellerH <- f.heroDao.getHeroByUserId(sellerUs)
        } yield assertTrue(confirm.last.choices.map(_.id) == List("BuyLotYes", "Auction") && confirm.last.inline) &&
                assertTrue(buyer.get.silver == 700L) &&
                // продавцу на руки ничего не упало: деньги в ячейке
                assertTrue(sellerH.get.silver == 0L && f.bank.silverSnapshot == 300L) &&
                assertTrue(f.payouts.snapshot.isEmpty) &&
                assertTrue(f.inv.snapshot.map(_.name) == List("Топор 1")) &&
                assertTrue(f.lots.snapshot.isEmpty) &&
                assertTrue(f.players.sentLetters.map(_._1) == List(sellerUs)) &&
                assertTrue(f.players.sentLetters.head._2.startsWith("🔔") &&
                           f.players.sentLetters.head._2.contains("ячейке"))
      },

      test("ячейка полна — выручка ждёт продавца в городе") {
        for {
          f       <- auction(lots = List(lotOf(1L, price = 300L)), silver = 1000L, vaultSilver = 100000L)
          _       <- f.state.action(testUser, tap("BuyLotYes", "id" -> "1"), f.renderer)
          sellerH <- f.heroDao.getHeroByUserId(sellerUs)
        } yield assertTrue(f.payouts.snapshot.get(seller).contains((300L, 0L))) &&
                assertTrue(sellerH.get.silver == 0L && f.bank.silverSnapshot == 100000L) &&
                assertTrue(f.players.sentLetters.head._2.contains("будете в городе"))
      },

      test("лот, купленный секунду назад, второму не достанется и денег не спишет") {
        for {
          f       <- auction(silver = 1000L)   // лота с таким номером уже нет
          _       <- f.state.action(testUser, tap("BuyLotYes", "id" -> "1"), f.renderer)
          screens <- f.renderer.sentScreens
          hero    <- f.heroDao.getHeroByUserId(userId)
        } yield assertTrue(screens.last.text.contains("уже ушёл")) &&
                assertTrue(hero.get.silver == 1000L && f.inv.snapshot.isEmpty && f.players.sentLetters.isEmpty)
      },

      test("свой лот не купишь") {
        for {
          f       <- auction(lots = List(lotOf(1L, owner = heroId)), silver = 1000L)
          _       <- f.state.action(testUser, tap("BuyLotYes", "id" -> "1"), f.renderer)
          screens <- f.renderer.sentScreens
          hero    <- f.heroDao.getHeroByUserId(userId)
        } yield assertTrue(screens.last.text.contains("Свой же лот")) &&
                assertTrue(hero.get.silver == 1000L && f.lots.snapshot.size == 1)
      },

      test("не хватает денег и нет места в сумке — отказ без списаний") {
        val full = (1L to 20L).toList.map(i => gear(100L + i, s"Хлам $i"))
        for {
          f      <- auction(lots = List(lotOf(1L, price = 300L)), silver = 100L)
          _      <- f.state.action(testUser, tap("BuyLotYes", "id" -> "1"), f.renderer)
          poor   <- f.renderer.sentScreens
          hero   <- f.heroDao.getHeroByUserId(userId)
          g      <- auction(inventory = full, lots = List(lotOf(1L, price = 300L)), silver = 1000L)
          _      <- g.state.action(testUser, tap("BuyLotYes", "id" -> "1"), g.renderer)
          noRoom <- g.renderer.sentScreens
        } yield assertTrue(poor.last.text.contains("не наберётся") && hero.get.silver == 100L) &&
                assertTrue(f.lots.snapshot.size == 1) &&
                assertTrue(noRoom.last.text.contains("нет места") && g.lots.snapshot.size == 1)
      },

      test("за дублоны: списываются дублоны, а выручка ждёт продавца в городе") {
        for {
          f       <- auction(lots = List(lotOf(1L, price = 60L, currency = AuctionCurrency.Doubloons)),
                             silver = 500L, doubloons = 100L)
          _       <- f.state.action(testUser, tap("BuyLotYes", "id" -> "1"), f.renderer)
          buyer   <- f.heroDao.getHeroByUserId(userId)
          sellerH <- f.heroDao.getHeroByUserId(sellerUs)
        } yield assertTrue(buyer.get.doubloons == 40L && buyer.get.silver == 500L) &&
                assertTrue(sellerH.get.doubloons == 0L && f.payouts.snapshot.get(seller).contains((0L, 60L)))
      }
    ),

    suite("Свои лоты")(

      test("свой лот снимается с торгов — вещь возвращается в сумку, лот пропадает") {
        for {
          f    <- auction(lots = List(lotOf(1L, owner = heroId)))
          _    <- f.state.action(testUser, tap("MyLots"), f.renderer)
          mine <- f.renderer.sentScreens
          _    <- f.state.action(testUser, tap("Reclaim", "id" -> "1"), f.renderer)
        } yield assertTrue(mine.last.choices.map(_.id) == List("Reclaim", "Auction")) &&
                assertTrue(f.lots.snapshot.isEmpty) &&
                assertTrue(f.inv.snapshot.map(_.name) == List("Топор 1"))
      },

      test("через неделю лот уходит в непроданные: не купить, но забрать можно") {
        val stale = lotOf(1L, owner = heroId, expiresAt = 0L)
        for {
          f        <- auction(lots = List(stale))
          _        <- f.state.action(testUser, tap("AuctionBrowse"), f.renderer)
          showcase <- f.renderer.sentScreens
          _        <- f.state.action(testUser, tap("MyLots"), f.renderer)
          mine     <- f.renderer.sentScreens
          _        <- f.state.action(testUser, tap("Reclaim", "id" -> "1"), f.renderer)
        } yield assertTrue(showcase.last.text.contains("Прилавки пусты")) &&
                assertTrue(mine.last.text.contains("не продан")) &&
                assertTrue(f.lots.snapshot.isEmpty && f.inv.snapshot.size == 1)
      },

      test("чужой лот не снять") {
        for {
          f       <- auction(lots = List(lotOf(1L)))
          _       <- f.state.action(testUser, tap("Reclaim", "id" -> "1"), f.renderer)
          screens <- f.renderer.sentScreens
        } yield assertTrue(screens.last.text.contains("не ваш лот")) &&
                assertTrue(f.lots.snapshot.size == 1 && f.inv.snapshot.isEmpty)
      }
    ),

    test("отложенная выручка выдаётся герою разом и с уведомлением") {
      for {
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(silver = 10L, doubloons = 1L))
        dao       = TestPayoutDao.empty
        content  <- ZIO.attempt(SceneContent.load())
        payouts   = Payouts(dao, heroDao, content)
        renderer <- TestRenderer.make
        hero     <- heroDao.getHeroByUserId(userId).map(_.get)
        _        <- payouts.queue(heroId, silver = 300L)
        _        <- payouts.queue(heroId, doubloons = 5L)
        _        <- payouts.deliver(testUser, hero, renderer)
        after    <- heroDao.getHeroByUserId(userId)
        screens  <- renderer.sentScreens
        // Второй раз выдавать нечего — и молчим.
        hero2    <- heroDao.getHeroByUserId(userId).map(_.get)
        _        <- payouts.deliver(testUser, hero2, renderer)
        again    <- renderer.sentScreens
      } yield assertTrue(after.get.silver == 310L && after.get.doubloons == 6L) &&
              assertTrue(screens.last.text.contains("🪙 300") && screens.last.text.contains("🟡 5")) &&
              assertTrue(dao.snapshot.isEmpty && again.size == screens.size)
    },

    test("цена, сбор, потолок лотов и срок жизни") {
      assertTrue(AuctionLot.MinPrice == 50L && AuctionLot.FeePct == 10L && AuctionLot.MaxLots == 10) &&
      assertTrue(AuctionLot.fee(500L) == 50L && AuctionLot.fee(50L) == 5L && AuctionLot.fee(1L) == 1L) &&
      assertTrue(AuctionLot.LifetimeMs == 7L * 24L * 3600L * 1000L) &&
      assertTrue(AuctionLot.announcement(lotOf(3L)).startsWith("📢 Выставлен лот номер 3"))
    }
  )
}
