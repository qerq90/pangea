package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._

object EquipmentStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val sword = Item(10L, "Меч судьбы", 1L, Rarity.Blue, ItemType.Weapon,
                           attack = 20, accuracy = 5, concentration = 0,
                           armor = 0, defence = 0, evasion = 0)

  private val ring1 = Item(11L, "Кольцо силы",   1L, Rarity.Gray, ItemType.Ring,
                           attack = 3, accuracy = 0, concentration = 0,
                           armor = 0, defence = 0, evasion = 0)
  private val ring2 = Item(12L, "Кольцо ловкости", 1L, Rarity.Gray, ItemType.Ring,
                           attack = 0, accuracy = 0, concentration = 0,
                           armor = 0, defence = 2, evasion = 0)

  private def makeState(hero: pangea.model.hero.Hero, invItems: List[Item] = Nil) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      invRepo   = TestInventoryRepository.withItems(invItems)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (EquipmentState(heroDao, invRepo, content), heroDao, invRepo, renderer)

  override def spec = suite("EquipmentState")(

    test("enter → показывает первый слот (Шлем)") {
      for {
        quad                          <- makeState(TestFixtures.hero(userId))
        (state, _, _, renderer)        = quad
        _                             <- state.enter(testUser, renderer)
        screens                       <- renderer.sentScreens
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.contains("Шлем"))
    },

    test("Next → переходит к следующему слоту") {
      for {
        quad                          <- makeState(TestFixtures.hero(userId))
        (state, _, _, renderer)        = quad
        _                             <- state.enter(testUser, renderer)
        _                             <- state.action(testUser, tap("Next"), renderer)
        screens                       <- renderer.sentScreens
      } yield assertTrue(screens.last.text.contains("Наплечники"))
    },

    test("BackFromEquip → возврат в HeroStats") {
      for {
        quad                          <- makeState(TestFixtures.hero(userId))
        (state, _, _, renderer)        = quad
        _                             <- state.enter(testUser, renderer)
        result                        <- state.action(testUser, tap("BackFromEquip"), renderer)
      } yield assertTrue(result == StateType.HeroStats)
    },

    test("Unequip оружия → предмет идёт в инвентарь, слот пустеет, atk снижается") {
      val heroWithSword = TestFixtures.hero(userId).copy(
        equipment  = TestFixtures.emptyEquipment.copy(weapon = sword),
        fightStats = TestFixtures.hero(userId).fightStats.copy(atk = sword.attack.toLong)
      )
      for {
        quad                          <- makeState(heroWithSword)
        (state, heroDao, invRepo, renderer) = quad
        _                             <- state.enter(testUser, renderer)
        // перейти к слоту «Оружие» (индекс 12)
        _                             <- ZIO.foreachDiscard(1 to 12)(_ => state.action(testUser, tap("Next"), renderer))
        _                             <- state.action(testUser, tap("Unequip"), renderer)
        updated                       <- heroDao.getHeroByUserId(userId)
        screens                       <- renderer.sentScreens
      } yield assertTrue(updated.exists(_.equipment.weapon.itemType == ItemType.NoItem)) &&
              assertTrue(updated.exists(_.fightStats.atk == 0L)) &&
              assertTrue(invRepo.snapshot.exists(_.id == sword.id)) &&
              assertTrue(screens.exists(_.text.contains("снят")))
    },

    test("Unequip пустого слота → сообщение об ошибке") {
      for {
        quad                          <- makeState(TestFixtures.hero(userId))
        (state, _, _, renderer)        = quad
        _                             <- state.enter(testUser, renderer)
        _                             <- state.action(testUser, tap("Unequip"), renderer)
        screens                       <- renderer.sentScreens
      } yield assertTrue(screens.exists(_.text.contains("пуст")))
    },

    test("Unequip при полном инвентаре → сообщение, предмет остаётся") {
      val heroWithSword = TestFixtures.hero(userId).copy(
        equipment = TestFixtures.emptyEquipment.copy(weapon = sword)
      )
      for {
        heroDao  <- TestHeroDao.withHero(userId, heroWithSword)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        // canAdd = false: addItem всегда возвращает ошибку
        invRepo   = TestInventoryRepository.full
        state     = EquipmentState(heroDao, invRepo, content)
        _        <- state.enter(testUser, renderer)
        _        <- ZIO.foreachDiscard(1 to 12)(_ => state.action(testUser, tap("Next"), renderer))
        _        <- state.action(testUser, tap("Unequip"), renderer)
        updated  <- heroDao.getHeroByUserId(userId)
        screens  <- renderer.sentScreens
      } yield assertTrue(updated.exists(_.equipment.weapon.itemType == ItemType.Weapon)) &&
              assertTrue(screens.exists(_.text.contains("полон")))
    },

    test("надеть два кольца из инвентаря → оба в разных слотах") {
      val heroBase = TestFixtures.hero(userId)
      for {
        heroDao  <- TestHeroDao.withHero(userId, heroBase)
        invRepo   = TestInventoryRepository.withItems(List(ring1, ring2))
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        invState  = InventoryState(heroDao, invRepo, content)
        _        <- invState.enter(testUser, renderer)          // видим ring1
        _        <- invState.action(testUser, tap("Equip"), renderer)  // надеваем ring1 → firstRing
        _        <- invState.action(testUser, tap("Equip"), renderer)  // надеваем ring2 → secondRing
        updated  <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updated.exists(_.equipment.firstRing.id  == ring1.id)) &&
              assertTrue(updated.exists(_.equipment.secondRing.id == ring2.id))
    }
  )
}
