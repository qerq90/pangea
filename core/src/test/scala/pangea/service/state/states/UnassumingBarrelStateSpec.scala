package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestBarrelRepository, TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._

object UnassumingBarrelStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def text(t: String):  UserAction = UserAction(t, None)

  private def gearItem(id: Long, name: String = "Меч"): Item =
    Item(id, name, lvl = 1L, Rarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, concentration = 0, armor = 0, defence = 0, evasion = 0)

  private def makeState(
    inventory: List[Item],
    barrelItems: List[Item] = Nil,
    barrelGold: Long = 0L,
    heroGold: Long = 0L
  ) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(gold = heroGold))
      invRepo   = TestInventoryRepository.withItems(inventory)
      barrelRepo = TestBarrelRepository.of(barrelItems, barrelGold)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (UnassumingBarrelState(heroDao, invRepo, barrelRepo, content), heroDao, invRepo, barrelRepo, renderer)

  override def spec = suite("UnassumingBarrelState")(

    test("enter → меню с 5 кнопками") {
      for {
        t <- makeState(inventory = Nil)
        (state, _, _, _, renderer) = t
        _      <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.choices.map(_.id).toSet ==
        Set("DepositItemsMenu", "WithdrawItemsMenu", "DepositGoldMenu", "WithdrawGoldMenu", "LeaveBarrel"))
    },

    test("LeaveBarrel → переход в HarborQuarter") {
      for {
        t <- makeState(Nil)
        (state, _, _, _, renderer) = t
        result <- state.action(testUser, tap("LeaveBarrel"), renderer)
      } yield assertTrue(result == StateType.HarborQuarter)
    },

    test("DepositItem_<id> → предмет уходит из инвентаря в бочку") {
      for {
        t <- makeState(inventory = List(gearItem(42L, "Шлем")))
        (state, _, invRepo, barrelRepo, renderer) = t
        _ <- state.action(testUser, tap("DepositItem_42"), renderer)
      } yield assertTrue(invRepo.snapshot.isEmpty) &&
              assertTrue(barrelRepo.itemsSnapshot.map(_.id) == List(42L))
    },

    test("DepositItem в полную бочку → отказ, инвентарь не тронут") {
      val full = (1L to 10L).toList.map(i => gearItem(i, s"X$i"))
      for {
        t <- makeState(inventory = List(gearItem(99L)), barrelItems = full)
        (state, _, invRepo, barrelRepo, renderer) = t
        _ <- state.action(testUser, tap("DepositItem_99"), renderer)
      } yield assertTrue(invRepo.snapshot.map(_.id) == List(99L)) &&
              assertTrue(barrelRepo.itemsSnapshot.size == 10)
    },

    test("WithdrawItem_<id> → предмет возвращается в инвентарь") {
      for {
        t <- makeState(inventory = Nil, barrelItems = List(gearItem(7L, "Меч")))
        (state, _, invRepo, barrelRepo, renderer) = t
        _ <- state.action(testUser, tap("WithdrawItem_7"), renderer)
      } yield assertTrue(barrelRepo.itemsSnapshot.isEmpty) &&
              assertTrue(invRepo.snapshot.map(_.id) == List(7L))
    },

    test("Положить золото: вход в режим + текст → списываем у героя, кладём в бочку") {
      for {
        t <- makeState(Nil, heroGold = 5000L)
        (state, heroDao, _, barrelRepo, renderer) = t
        _    <- state.action(testUser, tap("DepositGoldMenu"), renderer)
        _    <- state.action(testUser, text("500"), renderer)
        hero <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(hero.exists(_.gold == 4500L)) &&
              assertTrue(barrelRepo.goldSnapshot == 500L)
    },

    test("Положить золото больше, чем у героя → отказ, ничего не меняем") {
      for {
        t <- makeState(Nil, heroGold = 100L)
        (state, heroDao, _, barrelRepo, renderer) = t
        _    <- state.action(testUser, tap("DepositGoldMenu"), renderer)
        _    <- state.action(testUser, text("500"), renderer)
        hero <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(hero.exists(_.gold == 100L)) &&
              assertTrue(barrelRepo.goldSnapshot == 0L)
    },

    test("Положить золото больше, чем влезает в бочку → отказ") {
      for {
        t <- makeState(Nil, heroGold = 10000L, barrelGold = 9999L)
        (state, _, _, barrelRepo, renderer) = t
        _ <- state.action(testUser, tap("DepositGoldMenu"), renderer)
        _ <- state.action(testUser, text("500"), renderer)
      } yield assertTrue(barrelRepo.goldSnapshot == 9999L)
    },

    test("Не число в режиме ввода → ошибка, режим сохраняется") {
      for {
        t <- makeState(Nil, heroGold = 1000L)
        (state, _, _, barrelRepo, renderer) = t
        _       <- state.action(testUser, tap("DepositGoldMenu"), renderer)
        _       <- state.action(testUser, text("abc"), renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(barrelRepo.goldSnapshot == 0L) &&
              assertTrue(screens.exists(_.text.contains("Это не число")))
    },

    test("Отрицательное число → ошибка") {
      for {
        t <- makeState(Nil, heroGold = 1000L)
        (state, _, _, barrelRepo, renderer) = t
        _ <- state.action(testUser, tap("DepositGoldMenu"), renderer)
        _ <- state.action(testUser, text("-50"), renderer)
      } yield assertTrue(barrelRepo.goldSnapshot == 0L)
    },

    test("Забрать золото: списываем из бочки, добавляем герою") {
      for {
        t <- makeState(Nil, heroGold = 100L, barrelGold = 800L)
        (state, heroDao, _, barrelRepo, renderer) = t
        _    <- state.action(testUser, tap("WithdrawGoldMenu"), renderer)
        _    <- state.action(testUser, text("300"), renderer)
        hero <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(hero.exists(_.gold == 400L)) &&
              assertTrue(barrelRepo.goldSnapshot == 500L)
    },

    test("Забрать больше золота, чем в бочке → отказ") {
      for {
        t <- makeState(Nil, heroGold = 0L, barrelGold = 100L)
        (state, heroDao, _, barrelRepo, renderer) = t
        _    <- state.action(testUser, tap("WithdrawGoldMenu"), renderer)
        _    <- state.action(testUser, text("500"), renderer)
        hero <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(hero.exists(_.gold == 0L)) &&
              assertTrue(barrelRepo.goldSnapshot == 100L)
    },

    test("DepositItemsMenu при 12 предметах → 8 кнопок и кнопка След.") {
      val twelve = (1L to 12L).toList.map(i => gearItem(i, s"X$i"))
      for {
        t <- makeState(inventory = twelve)
        (state, _, _, _, renderer) = t
        _       <- state.action(testUser, tap("DepositItemsMenu"), renderer)
        screens <- renderer.sentScreens
      } yield {
        val ids = screens.last.choices.map(_.id)
        assertTrue(ids.count(_.startsWith("DepositItem_")) == 8) &&
        assertTrue(ids.contains("DepositItemsNext")) &&
        assertTrue(!ids.contains("DepositItemsPrev"))
      }
    },

    test("Next → следующая страница показывает остаток и кнопку Пред.") {
      val twelve = (1L to 12L).toList.map(i => gearItem(i, s"X$i"))
      for {
        t <- makeState(inventory = twelve)
        (state, _, _, _, renderer) = t
        _       <- state.action(testUser, tap("DepositItemsMenu"), renderer)
        _       <- state.action(testUser, tap("DepositItemsNext"), renderer)
        screens <- renderer.sentScreens
      } yield {
        val ids = screens.last.choices.map(_.id)
        assertTrue(ids.count(_.startsWith("DepositItem_")) == 4) &&
        assertTrue(ids.contains("DepositItemsPrev")) &&
        assertTrue(!ids.contains("DepositItemsNext"))
      }
    }
  )
}
