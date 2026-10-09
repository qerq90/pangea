package pangea.service.state.states.bank

import pangea.engine.SceneContent
import pangea.model.bank.BankVault
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.payment.{OrderId, Payment, PaymentStatus}
import pangea.model.state.StateType
import pangea.model.user.{ReceiptEmail, TelegramId, User, UserId, VkId}
import pangea.service.donation.Donations
import pangea.service.parcel.Parcels
import pangea.service.purse.{Purse, Wallet}
import pangea.service.state.UserAction
import pangea.test.{TestBankRepository, TestDonations, TestFixtures, TestHeroDao, TestInventoryRepository,
  TestParcelDao, TestRenderer, TestUserRepository}
import zio.{ZIO, durationInt}
import zio.test._

/** Торговый дом: ячейки у Рахадима, хранилище как бочка и общий кошель —
  * серебро из ячейки идёт в дело, когда кончилось своё. */
object TradeHouseSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  /** Кнопка пакета: выбранный sku едет в payload рядом с action. */
  private def pick(key: String, sku: String): UserAction =
    UserAction("", Some(s"""{"action":"$key","sku":"$sku"}"""))

  /** Заказ в полёте — ровно то, что отдаёт `Donations.active`. */
  private def order(user: UserId, kopecks: Long, doubloons: Long): Payment =
    Payment(
      id = 1L, orderId = OrderId("d-test"), userId = user, sku = "d-test",
      amountKopecks = kopecks, doubloons = doubloons, receiptEmail = "player@mail.ru",
      paymentId = Some("pay-1"), paymentUrl = Some("https://pay.test/1"),
      status = PaymentStatus.New, bankStatus = None, granted = false,
      createdAt = 0L, updatedAt = 0L
    )
  private def text(t: String):  UserAction = UserAction(t, None)

  private def gear(id: Long, name: String = "Меч"): Item =
    Item(id, name, lvl = 1L, Rarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  private def house(heroSilver: Long, cells: Int = 0, vaultSilver: Long = 0L,
                    donations: Donations = TestDonations.off) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(silver = heroSilver))
      userRepo <- TestUserRepository.withUser(testUser)
      bankRepo  = TestBankRepository.of(cells, silver = vaultSilver)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
      parcels   = Parcels(TestParcelDao.empty, bankRepo, content)
    } yield (TradeHouseState(heroDao, userRepo, bankRepo, parcels, TestInventoryRepository.accepting,
               donations, content),
             heroDao, bankRepo, renderer, userRepo)

  private def vaultState(
    inventory:   List[Item],
    vaultItems:  List[Item] = Nil,
    cells:       Int        = 1,
    vaultSilver: Long       = 0L,
    heroSilver:  Long       = 0L
  ) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(silver = heroSilver))
      invRepo   = TestInventoryRepository.withItems(inventory)
      bankRepo  = TestBankRepository.of(cells, vaultItems, vaultSilver)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BankVaultState(heroDao, invRepo, bankRepo, content), heroDao, invRepo, bankRepo, renderer)

  override def spec = suite("Торговый дом")(

    suite("Рахадим")(

      test("без ячеек: приветствие, кнопка покупки за 10 000, ни хранилища, ни аукциона") {
        for {
          t <- house(heroSilver = 0L)
          (state, _, _, renderer, _) = t
          _       <- state.enter(testUser, renderer)
          screens <- renderer.sentScreens
          menu     = screens.last
        } yield assertTrue(screens.head.text.contains("Рахадим")) &&
                assertTrue(menu.choices.map(_.id) ==
                  List("BuyCell", "BuyDoubloons", "DepositInterest", "FetShop", "RakhadimDaily",
                       "RakhQuest", "LeaveTradeHouse", "GoToCity")) &&
                assertTrue(menu.choices.head.label.contains(BankVault.FirstCellPrice.toString)) &&
                assertTrue(menu.choices.forall(_.label.length <= pangea.engine.Choice.MaxLabelLength))
      },

      test("«В город» уводит прямо на площадь — и от Рахадима, и из хранилища за его дверью") {
        for {
          t <- house(heroSilver = 0L, cells = 1)
          (state, _, _, renderer, _) = t
          _     <- state.enter(testUser, renderer)
          out   <- state.action(testUser, tap("GoToCity"), renderer)
          v     <- vaultState(inventory = Nil, cells = 1)
          (vault, _, _, _, vr) = v
          _     <- vault.enter(testUser, vr)
          fromVault <- vault.action(testUser, tap("GoToCity"), vr)
        } yield assertTrue(out == StateType.GlobalMap && fromVault == StateType.GlobalMap)
      },

      test("купленная ячейка открывает «Моё хранилище» и аукцион") {
        for {
          t <- house(heroSilver = 0L, cells = 1)
          (state, _, _, renderer, _) = t
          _       <- state.enter(testUser, renderer)
          screens <- renderer.sentScreens
          next    <- state.action(testUser, tap("MyVault"), renderer)
          auction <- state.action(testUser, tap("Auction"), renderer)
        } yield assertTrue(screens.last.choices.map(_.id) ==
                  List("BuyCell", "MyVault", "BuyDoubloons", "DepositInterest", "FetShop", "Auction",
                       "RakhadimDaily", "RakhQuest", "LeaveTradeHouse", "GoToCity")) &&
                assertTrue(next == StateType.BankVault && auction == StateType.Auction)
      },

      test("покупка первой ячейки: 10 000 с рук, 100 мест и 100 000 под серебро") {
        for {
          t <- house(heroSilver = 10000L)
          (state, heroDao, bank, renderer, _) = t
          _    <- state.action(testUser, tap("BuyCellConfirm"), renderer)
          hero <- heroDao.getHeroByUserId(userId)
        } yield assertTrue(bank.cellsSnapshot == 1) &&
                assertTrue(hero.get.silver == 0L) &&
                assertTrue(BankVault.empty(hero.get.id).copy(cells = 1).maxItems == 100L) &&
                assertTrue(BankVault.empty(hero.get.id).copy(cells = 1).maxSilver == 100000L)
      },

      test("вторая ячейка стоит 100 000, третья — 200 000") {
        for {
          t <- house(heroSilver = 100000L, cells = 1)
          (state, heroDao, bank, renderer, _) = t
          _       <- state.action(testUser, tap("BuyCell"), renderer)
          screens <- renderer.sentScreens
          _       <- state.action(testUser, tap("BuyCellConfirm"), renderer)
          hero    <- heroDao.getHeroByUserId(userId)
          after   <- renderer.sentScreens
        } yield assertTrue(screens.last.text.contains("100000")) &&
                assertTrue(bank.cellsSnapshot == 2 && hero.get.silver == 0L) &&
                assertTrue(after.last.choices.head.label.contains("200000")) &&
                assertTrue(BankVault.cellPrice(2) == 200000L && BankVault.cellPrice(3) == 300000L)
      },

      test("не хватает серебра → Рахадим разводит руками, ячейка не выдана") {
        for {
          t <- house(heroSilver = 9999L)
          (state, _, bank, renderer, _) = t
          _       <- state.action(testUser, tap("BuyCellConfirm"), renderer)
          screens <- renderer.sentScreens
        } yield assertTrue(bank.cellsSnapshot == 0) &&
                assertTrue(screens.last.text.contains("10000"))
      },

      test("дублоны: адреса ещё нет, проценты по вкладу — просто текст с «Назад»") {
        for {
          t <- house(heroSilver = 0L)
          (state, _, _, renderer, _) = t
          _        <- state.action(testUser, tap("BuyDoubloons"), renderer)
          doubloon <- renderer.sentScreens
          _        <- state.action(testUser, tap("DepositInterest"), renderer)
          interest <- renderer.sentScreens
        } yield assertTrue(doubloon.last.text.contains("для чеков: пока не записан")) &&
                assertTrue(doubloon.last.text.contains("покупка дублонов пока не работает")) &&
                assertTrue(doubloon.last.choices.map(_.id) == List("TradeHouse")) &&
                assertTrue(interest.last.text.contains("проценты по вкладу")) &&
                assertTrue(interest.last.choices.map(_.id) == List("TradeHouse"))
      },

      test("адрес для чека приходит сообщением: записан, показан на странице и правится новым") {
        for {
          t <- house(heroSilver = 0L)
          (state, _, _, renderer, userRepo) = t
          _      <- state.action(testUser, tap("BuyDoubloons"), renderer)
          _      <- state.action(testUser, text("  KMMG200@Yandex.RU "), renderer)
          saved  <- renderer.sentScreens
          stored <- userRepo.getUserById(userId).map(_.flatMap(_.receiptEmail))
          // Экран читает адрес из переданного `User`, а его StateHandler
          // перечитывает перед каждым действием — здесь делаем то же.
          fresh  <- userRepo.getUserById(userId).map(_.get)
          _      <- state.action(fresh, tap("BuyDoubloons"), renderer)
          page   <- renderer.sentScreens
          _      <- state.action(fresh, text("other@mail.ru"), renderer)
          after  <- userRepo.getUserById(userId).map(_.flatMap(_.receiptEmail))
        } yield assertTrue(stored.contains("kmmg200@yandex.ru")) &&
                assertTrue(saved.last.text.contains("kmmg200@yandex.ru")) &&
                assertTrue(saved.last.choices.map(_.id) == List("TradeHouse")) &&
                assertTrue(page.last.text.contains("для чеков: kmmg200@yandex.ru")) &&
                assertTrue(after.contains("other@mail.ru"))
      },

      test("не адрес — ничего не пишем; вне страницы дублонов текст просто вернёт в меню") {
        for {
          t <- house(heroSilver = 0L)
          (state, _, _, renderer, userRepo) = t
          _     <- state.action(testUser, tap("BuyDoubloons"), renderer)
          _     <- state.action(testUser, text("сколько стоит?"), renderer)
          bad   <- renderer.sentScreens
          empty <- userRepo.getUserById(userId).map(_.flatMap(_.receiptEmail))
          _     <- state.action(testUser, tap("TradeHouse"), renderer)
          _     <- state.action(testUser, text("ignored@mail.ru"), renderer)
          menu  <- renderer.sentScreens
          still <- userRepo.getUserById(userId).map(_.flatMap(_.receiptEmail))
        } yield assertTrue(empty.isEmpty && still.isEmpty) &&
                assertTrue(bad.last.text.contains("на адрес не похоже")) &&
                assertTrue(menu.last.choices.map(_.id).contains("BuyDoubloons"))
      },

      test("донат включён, но адреса нет: пакетов не показываем — чек отправить некуда") {
        for {
          dons <- TestDonations.on()
          t    <- house(heroSilver = 0L, donations = dons)
          (state, _, _, renderer, _) = t
          _      <- state.action(testUser, tap("BuyDoubloons"), renderer)
          screen <- renderer.sentScreens
        } yield assertTrue(screen.last.text.contains("Без адреса бумаги не оформить")) &&
                assertTrue(screen.last.choices.map(_.id) == List("TradeHouse"))
      },

      test("адрес записан: показываем пакеты с ценой в рублях") {
        for {
          dons <- TestDonations.on()
          t    <- house(heroSilver = 0L, donations = dons)
          (state, _, _, renderer, userRepo) = t
          _      <- state.action(testUser, tap("BuyDoubloons"), renderer)
          _      <- state.action(testUser, text("player@mail.ru"), renderer)
          fresh  <- userRepo.getUserById(userId).map(_.get)
          _      <- state.action(fresh, tap("BuyDoubloons"), renderer)
          screen <- renderer.sentScreens
        } yield assertTrue(screen.last.choices.map(_.id).count(_ == "DonPick") == 2) &&
                assertTrue(screen.last.choices.exists(_.label.contains("99"))) &&
                assertTrue(screen.last.choices.exists(_.data.get("sku").contains("d550")))
      },

      test("выбор пакета ведёт на подтверждение с ценой, почтой и офертой") {
        for {
          dons <- TestDonations.on()
          t    <- house(heroSilver = 0L, donations = dons)
          (state, _, _, renderer, userRepo) = t
          _      <- state.action(testUser, tap("BuyDoubloons"), renderer)
          _      <- state.action(testUser, text("player@mail.ru"), renderer)
          fresh  <- userRepo.getUserById(userId).map(_.get)
          _      <- state.action(fresh, pick("DonPick", "d100"), renderer)
          screen <- renderer.sentScreens
          // Подтверждение само ничего не заказывает — касса пока не тронута.
          started <- dons.started
        } yield assertTrue(screen.last.text.contains("player@mail.ru")) &&
                assertTrue(screen.last.text.contains("99")) &&
                assertTrue(screen.last.text.contains("https://example.test/offer")) &&
                assertTrue(screen.last.choices.map(_.id) == List("DonPay", "BuyDoubloons")) &&
                assertTrue(started.isEmpty)
      },

      test("«Оплатить» отдаёт ссылку текстом и кнопку проверки") {
        for {
          payment <- ZIO.succeed(order(userId, 9900L, 100L))
          dons    <- TestDonations.on(start = Donations.Start.Link(payment, "https://pay.test/1"))
          t       <- house(heroSilver = 0L, donations = dons)
          (state, _, _, renderer, userRepo) = t
          _       <- state.action(testUser, tap("BuyDoubloons"), renderer)
          _       <- state.action(testUser, text("player@mail.ru"), renderer)
          fresh   <- userRepo.getUserById(userId).map(_.get)
          _       <- state.action(fresh, pick("DonPay", "d100"), renderer)
          screen  <- renderer.sentScreens
          started <- dons.started
        } yield assertTrue(screen.last.text.contains("https://pay.test/1")) &&
                assertTrue(screen.last.choices.map(_.id) == List("DonCheck", "TradeHouse")) &&
                assertTrue(started.map(_.id) == List("d100"))
      },

      test("лимит и недоступная касса объясняются игроку, а не падают ошибкой") {
        for {
          limited <- TestDonations.on(start = Donations.Start.LimitReached)
          t1      <- house(heroSilver = 0L, donations = limited)
          (state1, _, _, renderer1, repo1) = t1
          _       <- state1.action(testUser, tap("BuyDoubloons"), renderer1)
          _       <- state1.action(testUser, text("player@mail.ru"), renderer1)
          fresh1  <- repo1.getUserById(userId).map(_.get)
          _       <- state1.action(fresh1, pick("DonPay", "d100"), renderer1)
          limit   <- renderer1.sentScreens

          closed  <- TestDonations.on(start = Donations.Start.Unavailable)
          t2      <- house(heroSilver = 0L, donations = closed)
          (state2, _, _, renderer2, repo2) = t2
          _       <- state2.action(testUser, tap("BuyDoubloons"), renderer2)
          _       <- state2.action(testUser, text("player@mail.ru"), renderer2)
          fresh2  <- repo2.getUserById(userId).map(_.get)
          _       <- state2.action(fresh2, pick("DonPay", "d100"), renderer2)
          down    <- renderer2.sentScreens
        } yield assertTrue(limit.last.text.contains("На сегодня с тебя хватит")) &&
                assertTrue(down.last.text.contains("Касса сейчас закрыта"))
      },

      test("«Проверить оплату»: зачислено, ещё ждём, не частить") {
        val payment = order(userId, 9900L, 100L)
        for {
          // Тестовые часы стартуют с нуля, а кулдаун считается от метки заказа:
          // отводим их вперёд, иначе любой опрос выглядит слишком частым.
          _    <- TestClock.adjust(10.seconds)
          dons <- TestDonations.on(active = Some(payment), settle = Donations.Settle.Granted(100L))
          t    <- house(heroSilver = 0L, donations = dons)
          (state, _, _, renderer, _) = t
          _      <- state.action(testUser, tap("DonCheck"), renderer)
          paid   <- renderer.sentScreens
          _      <- dons.setSettle(Donations.Settle.Pending)
          _      <- state.action(testUser, tap("DonCheck"), renderer)
          waited <- renderer.sentScreens
          // Заказ, который только что опрашивали, второй раз банк не трогает.
          _      <- dons.setActive(Some(payment.copy(updatedAt = 10000L)))
          _      <- state.action(testUser, tap("DonCheck"), renderer)
          often  <- renderer.sentScreens
        } yield assertTrue(paid.last.text.contains("Зачислено")) &&
                assertTrue(waited.last.text.contains("Оплата ещё не прошла")) &&
                assertTrue(often.last.text.contains("Не части"))
      },

      test("нотификация успела первой — кнопка говорит «уже зачислено», а не «платежей нет»") {
        val payment = order(userId, 9900L, 100L)
        for {
          _    <- TestClock.adjust(10.seconds)
          dons <- TestDonations.on(active = Some(payment.copy(granted = true)),
                    settle = Donations.Settle.AlreadyGranted)
          t    <- house(heroSilver = 0L, donations = dons)
          (state, _, _, renderer, _) = t
          _      <- state.action(testUser, tap("DonCheck"), renderer)
          screen <- renderer.sentScreens
        } yield assertTrue(screen.last.text.contains("уже зачислены"))
      },

      test("незавершённый платёж виден на экране покупки вместе с кнопкой проверки") {
        val payment = order(userId, 49900L, 550L)
        for {
          dons <- TestDonations.on(active = Some(payment))
          t    <- house(heroSilver = 0L, donations = dons)
          (state, _, _, renderer, userRepo) = t
          _      <- state.action(testUser, tap("BuyDoubloons"), renderer)
          _      <- state.action(testUser, text("player@mail.ru"), renderer)
          fresh  <- userRepo.getUserById(userId).map(_.get)
          _      <- state.action(fresh, tap("BuyDoubloons"), renderer)
          screen <- renderer.sentScreens
        } yield assertTrue(screen.last.text.contains("начатый платёж на 499 ₽")) &&
                assertTrue(screen.last.choices.map(_.id).contains("DonCheck"))
      },

      test("адресом считаем только похожее на адрес") {
        assertTrue(ReceiptEmail.parse(" Kmmg200@Yandex.ru\n").contains("kmmg200@yandex.ru")) &&
        assertTrue(ReceiptEmail.parse("имя.фамилия@почта.рф").contains("имя.фамилия@почта.рф")) &&
        assertTrue(ReceiptEmail.parse("ага").isEmpty) &&
        assertTrue(ReceiptEmail.parse("два слова@mail.ru").isEmpty) &&
        assertTrue(ReceiptEmail.parse("@mail.ru").isEmpty) &&
        assertTrue(ReceiptEmail.parse("a@b@mail.ru").isEmpty) &&
        assertTrue(ReceiptEmail.parse("a@mail").isEmpty) &&
        assertTrue(ReceiptEmail.parse("a@mail.").isEmpty) &&
        assertTrue(ReceiptEmail.parse("a@.ru").isEmpty) &&
        assertTrue(ReceiptEmail.parse("a@" + "x" * 300 + ".ru").isEmpty)
      }
    ),

    suite("Хранилище")(

      test("меню: вещи, серебро, «Положить всё» с настройкой; вместимость растёт с числом ячеек") {
        for {
          t <- vaultState(inventory = Nil, cells = 2)
          (state, _, _, _, renderer) = t
          _       <- state.enter(testUser, renderer)
          screens <- renderer.sentScreens
        } yield assertTrue(screens.last.choices.map(_.id).toSet ==
                  Set("VaultDepositItems", "VaultWithdrawItems", "VaultDepositSilver", "VaultWithdrawSilver",
                      "VaultStowAll", "VaultStowSettings", "LeaveVault", "GoToCity")) &&
                assertTrue(screens.last.text.contains("200") && screens.last.text.contains("200000"))
      },

      test("без ячеек хранилища нет") {
        for {
          t <- vaultState(inventory = Nil, cells = 0)
          (state, _, _, _, renderer) = t
          _       <- state.enter(testUser, renderer)
          screens <- renderer.sentScreens
        } yield assertTrue(screens.last.choices.map(_.id) == List("LeaveVault"))
      },

      test("вещь кладётся в ячейку и забирается обратно") {
        for {
          t <- vaultState(inventory = List(gear(42L, "Шлем")))
          (state, _, invRepo, bank, renderer) = t
          _     <- state.action(testUser, tap("VaultPut_42"), renderer)
          stored = (invRepo.snapshot.size, bank.itemsSnapshot.map(_.id))
          _     <- state.action(testUser, tap("VaultTake_42"), renderer)
        } yield assertTrue(stored == (0, List(42L))) &&
                assertTrue(bank.itemsSnapshot.isEmpty && invRepo.snapshot.map(_.id) == List(42L))
      },

      test("ячейка забита → отказ, вещь остаётся в сумке") {
        val full = (1L to 100L).toList.map(i => gear(i, s"X$i"))
        for {
          t <- vaultState(inventory = List(gear(999L)), vaultItems = full, cells = 1)
          (state, _, invRepo, bank, renderer) = t
          _ <- state.action(testUser, tap("VaultPut_999"), renderer)
        } yield assertTrue(invRepo.snapshot.map(_.id) == List(999L) && bank.itemsSnapshot.size == 100)
      },

      test("серебро: кладём суммой, забираем всё") {
        for {
          t <- vaultState(inventory = Nil, heroSilver = 5000L)
          (state, heroDao, _, bank, renderer) = t
          _     <- state.action(testUser, tap("VaultDepositSilver"), renderer)
          _     <- state.action(testUser, text("3000"), renderer)
          mid   <- heroDao.getHeroByUserId(userId)
          _     <- state.action(testUser, tap("VaultWithdrawAll"), renderer)
          after <- heroDao.getHeroByUserId(userId)
        } yield assertTrue(mid.get.silver == 2000L) &&
                assertTrue(bank.silverSnapshot == 0L && after.get.silver == 5000L)
      },

      test("больше, чем влезает в ячейки → отказ") {
        for {
          t <- vaultState(inventory = Nil, cells = 1, heroSilver = 200000L)
          (state, heroDao, _, bank, renderer) = t
          _       <- state.action(testUser, tap("VaultDepositSilver"), renderer)
          _       <- state.action(testUser, text("150000"), renderer)
          screens <- renderer.sentScreens
          hero    <- heroDao.getHeroByUserId(userId)
        } yield assertTrue(bank.silverSnapshot == 0L && hero.get.silver == 200000L) &&
                assertTrue(screens.last.text.contains("нет столько места"))
      }
    ),

    suite("Общий кошель")(

      test("делёж цены: сперва своё, остаток — из ячейки") {
        assertTrue(Wallet(100L, 900L).total == 1000L) &&
        assertTrue(Wallet(100L, 900L).split(300L).contains((100L, 200L))) &&
        assertTrue(Wallet(500L, 100L).split(300L).contains((300L, 0L))) &&
        assertTrue(Wallet(100L, 100L).split(300L).isEmpty) &&
        assertTrue(!Wallet(0L, 0L).canAfford(1L) && Wallet(0L, 0L).canAfford(0L))
      },

      test("charge: своё в ноль, нехватка уходит из ячейки") {
        for {
          heroDao <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(silver = 100L))
          bank     = TestBankRepository.of(1, silver = 900L)
          purse    = Purse(heroDao, Some(bank))
          hero    <- heroDao.getHeroByUserId(userId).map(_.get)
          paid    <- purse.charge(userId, hero, 300L)
          after   <- heroDao.getHeroByUserId(userId)
        } yield assertTrue(paid.map(_.silver).contains(0L)) &&
                assertTrue(after.get.silver == 0L && bank.silverSnapshot == 700L)
      },

      test("charge без банка и при нехватке не трогает ни кошелёк, ни ячейку") {
        for {
          heroDao <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(silver = 100L))
          bank     = TestBankRepository.of(1, silver = 50L)
          hero    <- heroDao.getHeroByUserId(userId).map(_.get)
          none    <- Purse.heroOnly(heroDao).charge(userId, hero, 200L)
          short   <- Purse(heroDao, Some(bank)).charge(userId, hero, 200L)
          after   <- heroDao.getHeroByUserId(userId)
        } yield assertTrue(none.isEmpty && short.isEmpty) &&
                assertTrue(after.get.silver == 100L && bank.silverSnapshot == 50L)
      },

      test("покупка у Рахадима идёт из ячейки, когда своего не хватило") {
        for {
          t <- house(heroSilver = 40000L, cells = 1, vaultSilver = 70000L)
          (state, heroDao, bank, renderer, _) = t
          _    <- state.action(testUser, tap("BuyCellConfirm"), renderer)
          hero <- heroDao.getHeroByUserId(userId)
        } yield assertTrue(bank.cellsSnapshot == 2) &&
                assertTrue(hero.get.silver == 0L && bank.silverSnapshot == 10000L)
      }
    )
  )
}
