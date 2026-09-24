package pangea.service.state.states.bank

import pangea.engine.SceneContent
import pangea.model.bank.BankVault
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.purse.{Purse, Wallet}
import pangea.service.state.UserAction
import pangea.test.{TestBankRepository, TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Торговый дом: ячейки у Рахадима, хранилище как бочка и общий кошель —
  * серебро из ячейки идёт в дело, когда кончилось своё. */
object TradeHouseSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def text(t: String):  UserAction = UserAction(t, None)

  private def gear(id: Long, name: String = "Меч"): Item =
    Item(id, name, lvl = 1L, Rarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  private def house(heroSilver: Long, cells: Int = 0, vaultSilver: Long = 0L) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(silver = heroSilver))
      bankRepo  = TestBankRepository.of(cells, silver = vaultSilver)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (TradeHouseState(heroDao, bankRepo, content), heroDao, bankRepo, renderer)

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
          (state, _, _, renderer) = t
          _       <- state.enter(testUser, renderer)
          screens <- renderer.sentScreens
          menu     = screens.last
        } yield assertTrue(screens.head.text.contains("Рахадим")) &&
                assertTrue(menu.choices.map(_.id) ==
                  List("BuyCell", "BuyDoubloons", "DepositInterest", "FetShop", "LeaveTradeHouse")) &&
                assertTrue(menu.choices.head.label.contains(BankVault.FirstCellPrice.toString)) &&
                assertTrue(menu.choices.forall(_.label.length <= pangea.engine.Choice.MaxLabelLength))
      },

      test("купленная ячейка открывает «Моё хранилище» и аукцион") {
        for {
          t <- house(heroSilver = 0L, cells = 1)
          (state, _, _, renderer) = t
          _       <- state.enter(testUser, renderer)
          screens <- renderer.sentScreens
          next    <- state.action(testUser, tap("MyVault"), renderer)
          auction <- state.action(testUser, tap("Auction"), renderer)
        } yield assertTrue(screens.last.choices.map(_.id) ==
                  List("BuyCell", "MyVault", "BuyDoubloons", "DepositInterest", "FetShop", "Auction", "LeaveTradeHouse")) &&
                assertTrue(next == StateType.BankVault && auction == StateType.Auction)
      },

      test("покупка первой ячейки: 10 000 с рук, 100 мест и 100 000 под серебро") {
        for {
          t <- house(heroSilver = 10000L)
          (state, heroDao, bank, renderer) = t
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
          (state, heroDao, bank, renderer) = t
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
          (state, _, bank, renderer) = t
          _       <- state.action(testUser, tap("BuyCellConfirm"), renderer)
          screens <- renderer.sentScreens
        } yield assertTrue(bank.cellsSnapshot == 0) &&
                assertTrue(screens.last.text.contains("10000"))
      },

      test("дублоны и проценты по вкладу — просто текст с «Назад»") {
        for {
          t <- house(heroSilver = 0L)
          (state, _, _, renderer) = t
          _        <- state.action(testUser, tap("BuyDoubloons"), renderer)
          doubloon <- renderer.sentScreens
          _        <- state.action(testUser, tap("DepositInterest"), renderer)
          interest <- renderer.sentScreens
        } yield assertTrue(doubloon.last.text.contains("Пока не работает")) &&
                assertTrue(doubloon.last.choices.map(_.id) == List("TradeHouse")) &&
                assertTrue(interest.last.text.contains("проценты по вкладу")) &&
                assertTrue(interest.last.choices.map(_.id) == List("TradeHouse"))
      }
    ),

    suite("Хранилище")(

      test("меню: пять кнопок, вместимость растёт с числом ячеек") {
        for {
          t <- vaultState(inventory = Nil, cells = 2)
          (state, _, _, _, renderer) = t
          _       <- state.enter(testUser, renderer)
          screens <- renderer.sentScreens
        } yield assertTrue(screens.last.choices.map(_.id).toSet ==
                  Set("VaultDepositItems", "VaultWithdrawItems", "VaultDepositSilver", "VaultWithdrawSilver", "LeaveVault")) &&
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
          (state, heroDao, bank, renderer) = t
          _    <- state.action(testUser, tap("BuyCellConfirm"), renderer)
          hero <- heroDao.getHeroByUserId(userId)
        } yield assertTrue(bank.cellsSnapshot == 2) &&
                assertTrue(hero.get.silver == 0L && bank.silverSnapshot == 10000L)
      }
    )
  )
}
