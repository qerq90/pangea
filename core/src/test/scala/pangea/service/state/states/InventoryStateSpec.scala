package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.generator.item.TreasureMapGenerator
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType, MapZone, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

object InventoryStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  /** Имитация нажатия на динамическую кнопку конкретного предмета. */
  private def selectItem(itemId: Long): UserAction =
    UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}$itemId"}"""))

  private val sword = Item(10L, "Старый меч", 1L, Rarity.Gray, ItemType.Weapon,
    attack = 5, accuracy = 0, concentration = 0, armor = 0, defence = 0, evasion = 0)
  private val helm  = Item(20L, "Шлем стража", 2L, Rarity.White, ItemType.Helmet,
    attack = 0, accuracy = 2, concentration = 0, armor = 3, defence = 1, evasion = 0)
  private val oldSword = Item(99L, "Ржавый меч", 1L, Rarity.Gray, ItemType.Weapon,
    attack = 2, accuracy = 0, concentration = 0, armor = 0, defence = 0, evasion = 0)

  // Две половинки карты Кинэт (dropLevel 10 и 20 → одна зона 1..25) и половинка
  // другой зоны (Ущелье мертвецов, 51..75) для проверки требования совпадения.
  private val kinetHalfA = TreasureMapGenerator.create(dropLevel = 10, half = true).copy(id = 30L)
  private val kinetHalfB = TreasureMapGenerator.create(dropLevel = 20, half = true).copy(id = 31L)
  private val gorgeHalf  = TreasureMapGenerator.create(dropLevel = 60, half = true).copy(id = 32L)

  private val baseHero = TestFixtures.hero(userId)

  private def makeState(hero: Hero, items: List[Item]) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      invRepo   = TestInventoryRepository.withItems(items)
      itemRepo  = TestItemRepository.make
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (InventoryState(heroDao, invRepo, itemRepo, content), heroDao, invRepo, renderer)

  override def spec = suite("InventoryState")(

    test("enter с пустым инвентарём → показывает 'пуст'") {
      for {
        quad                    <- makeState(baseHero, Nil)
        (state, _, _, renderer)  = quad
        _                       <- state.enter(testUser, renderer)
        screens                 <- renderer.sentScreens
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.contains("пуст"))
    },

    test("enter с предметами → показывает экран-список с кнопками-предметами") {
      for {
        quad                    <- makeState(baseHero, List(sword, helm))
        (state, _, _, renderer)  = quad
        _                       <- state.enter(testUser, renderer)
        screens                 <- renderer.sentScreens
      } yield assertTrue(
                screens.last.choices.exists(_.id == s"${InventoryState.ItemActionPrefix}${sword.id}")) &&
              assertTrue(
                screens.last.choices.exists(_.id == s"${InventoryState.ItemActionPrefix}${helm.id}"))
    },

    test("выбор предмета кнопкой → открывает детальный экран с Equip/Drop") {
      for {
        quad                    <- makeState(baseHero, List(sword))
        (state, _, _, renderer)  = quad
        _                       <- state.enter(testUser, renderer)
        _                       <- state.action(testUser, selectItem(sword.id), renderer)
        screens                 <- renderer.sentScreens
      } yield assertTrue(screens.last.text.contains(sword.name)) &&
              assertTrue(screens.last.choices.exists(_.id == "Equip")) &&
              assertTrue(screens.last.choices.exists(_.id == "Drop"))
    },

    test("осмотр половинки → кнопка Объединить (без Надеть), уровень не показан") {
      for {
        quad                    <- makeState(baseHero, List(kinetHalfA, kinetHalfB))
        (state, _, _, renderer)  = quad
        _                       <- state.enter(testUser, renderer)
        _                       <- state.action(testUser, selectItem(kinetHalfA.id), renderer)
        screens                 <- renderer.sentScreens
      } yield assertTrue(screens.last.choices.exists(_.id == "CombineMap")) &&
              assertTrue(!screens.last.choices.exists(_.id == "Equip")) &&
              assertTrue(!screens.last.text.contains("Ур."))
    },

    test("Объединить две половинки одной зоны → одна целая карта, половинки удалены") {
      for {
        quad                          <- makeState(baseHero, List(kinetHalfA, kinetHalfB))
        (state, _, invRepo, renderer)  = quad
        _                             <- state.enter(testUser, renderer)
        _                             <- state.action(testUser, selectItem(kinetHalfA.id), renderer)
        _                             <- state.action(testUser, tap("CombineMap"), renderer)
        items                          = invRepo.snapshot
        screens                       <- renderer.sentScreens
      } yield assertTrue(items.size == 1) &&
              assertTrue(items.head.itemType == ItemType.TreasureMap) &&
              assertTrue(items.head.name == MapZone.Kinet.mapName) &&
              assertTrue(screens.exists(_.text.contains("сложили две половинки")))
    },

    test("Объединить без второй половины этой зоны → сообщение, половинки на месте") {
      for {
        quad                          <- makeState(baseHero, List(kinetHalfA, gorgeHalf))
        (state, _, invRepo, renderer)  = quad
        _                             <- state.enter(testUser, renderer)
        _                             <- state.action(testUser, selectItem(kinetHalfA.id), renderer)
        _                             <- state.action(testUser, tap("CombineMap"), renderer)
        items                          = invRepo.snapshot
        screens                       <- renderer.sentScreens
      } yield assertTrue(items.size == 2) &&
              assertTrue(items.forall(_.itemType == ItemType.TreasureMapHalf)) &&
              assertTrue(screens.exists(_.text.contains("Нужна вторая половина")))
    },

    test("BackFromInventory → возврат в HeroStats") {
      for {
        quad                    <- makeState(baseHero, List(sword))
        (state, _, _, renderer)  = quad
        _                       <- state.enter(testUser, renderer)
        result                  <- state.action(testUser, tap("BackFromInventory"), renderer)
      } yield assertTrue(result == StateType.HeroStats)
    },

    test("Equip выбранного оружия в пустой слот → надет, удалён из инвентаря") {
      for {
        quad                             <- makeState(baseHero, List(sword))
        (state, heroDao, invRepo, renderer) = quad
        _                                <- state.enter(testUser, renderer)
        _                                <- state.action(testUser, selectItem(sword.id), renderer)
        _                                <- state.action(testUser, tap("Equip"), renderer)
        updatedHero                      <- heroDao.getHeroByUserId(userId)
        items                             = invRepo.snapshot
      } yield assertTrue(updatedHero.exists(_.equipment.weapon.name == sword.name)) &&
              assertTrue(items.isEmpty) &&
              assertTrue(updatedHero.exists(_.fightStats.atk == baseHero.fightStats.atk + sword.attack))
    },

    test("Equip с заменой старого → старое попадает обратно в инвентарь") {
      val heroWithWeapon = baseHero.copy(
        equipment  = TestFixtures.emptyEquipment.copy(weapon = oldSword),
        fightStats = baseHero.fightStats.copy(atk = baseHero.fightStats.atk + oldSword.attack)
      )
      for {
        quad                             <- makeState(heroWithWeapon, List(sword))
        (state, heroDao, invRepo, renderer) = quad
        _                                <- state.enter(testUser, renderer)
        _                                <- state.action(testUser, selectItem(sword.id), renderer)
        _                                <- state.action(testUser, tap("Equip"), renderer)
        updatedHero                      <- heroDao.getHeroByUserId(userId)
        items                             = invRepo.snapshot
      } yield assertTrue(updatedHero.exists(_.equipment.weapon.name == sword.name)) &&
              assertTrue(items.exists(_.id == oldSword.id)) &&
              assertTrue(updatedHero.exists(_.fightStats.atk == baseHero.fightStats.atk + sword.attack))
    },

    test("Drop выбранного → предмет удалён из инвентаря") {
      for {
        quad                             <- makeState(baseHero, List(sword, helm))
        (state, _, invRepo, renderer)    = quad
        _                                <- state.enter(testUser, renderer)
        _                                <- state.action(testUser, selectItem(sword.id), renderer)
        _                                <- state.action(testUser, tap("Drop"), renderer)
        items                             = invRepo.snapshot
        screens                          <- renderer.sentScreens
      } yield assertTrue(items.forall(_.id != sword.id)) &&
              assertTrue(items.size == 1) &&
              assertTrue(screens.exists(_.text.contains("выброшен")))
    },

    test("Drop последнего предмета → показывает 'пуст'") {
      for {
        quad                    <- makeState(baseHero, List(sword))
        (state, _, _, renderer)  = quad
        _                       <- state.enter(testUser, renderer)
        _                       <- state.action(testUser, selectItem(sword.id), renderer)
        _                       <- state.action(testUser, tap("Drop"), renderer)
        screens                 <- renderer.sentScreens
      } yield assertTrue(screens.last.text.contains("пуст"))
    },

    test("Equip предмета выше уровня героя → ошибка, предмет не надет") {
      val highLvlSword = Item(30L, "Легендарный меч", 99L, Rarity.Gray, ItemType.Weapon,
        attack = 100, accuracy = 0, concentration = 0, armor = 0, defence = 0, evasion = 0)
      val lvl1Hero = baseHero
      for {
        quad                             <- makeState(lvl1Hero, List(highLvlSword))
        (state, heroDao, invRepo, renderer) = quad
        _                                <- state.enter(testUser, renderer)
        _                                <- state.action(testUser, selectItem(highLvlSword.id), renderer)
        _                                <- state.action(testUser, tap("Equip"), renderer)
        updatedHero                      <- heroDao.getHeroByUserId(userId)
        items                             = invRepo.snapshot
        screens                          <- renderer.sentScreens
      } yield assertTrue(updatedHero.exists(_.equipment.weapon.itemType == pangea.model.item.ItemType.NoItem)) &&
              assertTrue(items.exists(_.id == highLvlSword.id)) &&
              assertTrue(screens.exists(_.text.contains("уровень")))
    }
  )
}
