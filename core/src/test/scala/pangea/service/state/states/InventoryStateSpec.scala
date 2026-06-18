package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._

object InventoryStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val sword = Item(10L, "Старый меч", 1L, Rarity.Gray, ItemType.Weapon,
    attack = 5, accuracy = 0, concentration = 0, armor = 0, defence = 0, evasion = 0)
  private val helm  = Item(20L, "Шлем стража", 2L, Rarity.White, ItemType.Helmet,
    attack = 0, accuracy = 2, concentration = 0, armor = 3, defence = 1, evasion = 0)
  private val oldSword = Item(99L, "Ржавый меч", 1L, Rarity.Gray, ItemType.Weapon,
    attack = 2, accuracy = 0, concentration = 0, armor = 0, defence = 0, evasion = 0)

  private val baseHero = TestFixtures.hero(userId)  // fightStats.atk = 10, пустое снаряжение

  private def makeState(hero: Hero, items: List[Item]) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      invRepo   = TestInventoryRepository.withItems(items)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (InventoryState(heroDao, invRepo, content), heroDao, invRepo, renderer)

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

    test("enter с предметами → показывает первый предмет") {
      for {
        quad                    <- makeState(baseHero, List(sword, helm))
        (state, _, _, renderer)  = quad
        _                       <- state.enter(testUser, renderer)
        screens                 <- renderer.sentScreens
      } yield assertTrue(screens.last.text.contains(sword.name))
    },

    test("Next → переходит к следующему предмету") {
      for {
        quad                    <- makeState(baseHero, List(sword, helm))
        (state, _, _, renderer)  = quad
        _                       <- state.enter(testUser, renderer)
        result                  <- state.action(testUser, tap("Next"), renderer)
        screens                 <- renderer.sentScreens
      } yield assertTrue(result == StateType.Inventory) &&
              assertTrue(screens.last.text.contains(helm.name))
    },

    test("Prev на первом предмете → остаётся на первом") {
      for {
        quad                    <- makeState(baseHero, List(sword, helm))
        (state, _, _, renderer)  = quad
        _                       <- state.enter(testUser, renderer)
        _                       <- state.action(testUser, tap("Prev"), renderer)
        screens                 <- renderer.sentScreens
      } yield assertTrue(screens.last.text.contains(sword.name))
    },

    test("BackFromInventory → возврат в Dungeon") {
      for {
        quad                    <- makeState(baseHero, List(sword))
        (state, _, _, renderer)  = quad
        _                       <- state.enter(testUser, renderer)
        result                  <- state.action(testUser, tap("BackFromInventory"), renderer)
      } yield assertTrue(result == StateType.Dungeon)
    },

    test("Equip оружия в пустой слот → надет, удалён из инвентаря, atk обновлён") {
      for {
        quad                             <- makeState(baseHero, List(sword))
        (state, heroDao, invRepo, renderer) = quad
        _                                <- state.enter(testUser, renderer)
        _                                <- state.action(testUser, tap("Equip"), renderer)
        updatedHero                      <- heroDao.getHeroByUserId(userId)
        items                             = invRepo.snapshot
      } yield assertTrue(updatedHero.exists(_.equipment.weapon.name == sword.name)) &&
              assertTrue(items.isEmpty) &&
              assertTrue(updatedHero.exists(_.fightStats.atk == baseHero.fightStats.atk + sword.attack))
    },

    test("Equip оружия с заменой старого → старое попадает обратно в инвентарь") {
      val heroWithWeapon = baseHero.copy(
        equipment  = TestFixtures.emptyEquipment.copy(weapon = oldSword),
        fightStats = baseHero.fightStats.copy(atk = baseHero.fightStats.atk + oldSword.attack)
      )
      for {
        quad                             <- makeState(heroWithWeapon, List(sword))
        (state, heroDao, invRepo, renderer) = quad
        _                                <- state.enter(testUser, renderer)
        _                                <- state.action(testUser, tap("Equip"), renderer)
        updatedHero                      <- heroDao.getHeroByUserId(userId)
        items                             = invRepo.snapshot
      } yield assertTrue(updatedHero.exists(_.equipment.weapon.name == sword.name)) &&
              assertTrue(items.exists(_.id == oldSword.id)) &&
              assertTrue(updatedHero.exists(_.fightStats.atk == baseHero.fightStats.atk + sword.attack))
    },

    test("Drop → предмет удалён из инвентаря, показывается следующий") {
      for {
        quad                             <- makeState(baseHero, List(sword, helm))
        (state, _, invRepo, renderer)    = quad
        _                                <- state.enter(testUser, renderer)
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
        _                       <- state.action(testUser, tap("Drop"), renderer)
        screens                 <- renderer.sentScreens
      } yield assertTrue(screens.last.text.contains("пуст"))
    },

    test("Equip предмета выше уровня героя → ошибка, предмет не надет") {
      val highLvlSword = Item(30L, "Легендарный меч", 99L, Rarity.Gray, ItemType.Weapon,
        attack = 100, accuracy = 0, concentration = 0, armor = 0, defence = 0, evasion = 0)
      val lvl1Hero = baseHero  // lvl = 1
      for {
        quad                             <- makeState(lvl1Hero, List(highLvlSword))
        (state, heroDao, invRepo, renderer) = quad
        _                                <- state.enter(testUser, renderer)
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
