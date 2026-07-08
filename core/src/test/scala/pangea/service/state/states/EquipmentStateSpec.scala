package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

object EquipmentStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def selectSlot(idx: Int): UserAction =
    UserAction("", Some(s"""{"action":"${EquipmentState.SlotPrefix}$idx"}"""))
  private def selectItem(itemId: Long): UserAction =
    UserAction("", Some(s"""{"action":"${InventoryState.ItemActionPrefix}$itemId"}"""))

  // Индекс слотов в [[EquipmentState.slots]]
  private val WeaponSlotIdx = 12

  private val sword = Item(10L, "Меч судьбы", 1L, Rarity.Blue, ItemType.Weapon,
                           attack = 20, accuracy = 5, energy = 0,
                           armor = 0, defence = 0, evasion = 0)

  private val ring1 = Item(11L, "Кольцо силы",   1L, Rarity.Gray, ItemType.Ring,
                           attack = 3, accuracy = 0, energy = 0,
                           armor = 0, defence = 0, evasion = 0)
  private val ring2 = Item(12L, "Кольцо ловкости", 1L, Rarity.Gray, ItemType.Ring,
                           attack = 0, accuracy = 0, energy = 0,
                           armor = 0, defence = 2, evasion = 0)

  private def makeState(hero: pangea.model.hero.Hero, invItems: List[Item] = Nil) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      invRepo   = TestInventoryRepository.withItems(invItems)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (EquipmentState(heroDao, invRepo, content), heroDao, invRepo, renderer)

  override def spec = suite("EquipmentState")(

    test("enter → показывает список слотов кнопками") {
      for {
        quad                          <- makeState(TestFixtures.hero(userId))
        (state, _, _, renderer)        = quad
        _                             <- state.enter(testUser, renderer)
        screens                       <- renderer.sentScreens
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.choices.exists(_.id == s"${EquipmentState.SlotPrefix}0"))
    },

    test("BackFromEquip → возврат в HeroStats") {
      for {
        quad                          <- makeState(TestFixtures.hero(userId))
        (state, _, _, renderer)        = quad
        _                             <- state.enter(testUser, renderer)
        result                        <- state.action(testUser, tap("BackFromEquip"), renderer)
      } yield assertTrue(result == StateType.HeroStats)
    },

    test("выбор слота → открывает детальный экран с Unequip (если предмет надет)") {
      val heroWithSword = TestFixtures.hero(userId).copy(
        equipment = TestFixtures.emptyEquipment.copy(weapon = sword)
      )
      for {
        quad                          <- makeState(heroWithSword)
        (state, _, _, renderer)        = quad
        _                             <- state.enter(testUser, renderer)
        _                             <- state.action(testUser, selectSlot(WeaponSlotIdx), renderer)
        screens                       <- renderer.sentScreens
      } yield assertTrue(screens.last.text.contains(sword.name)) &&
              assertTrue(screens.last.choices.exists(_.id == "Unequip"))
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
        _                             <- state.action(testUser, selectSlot(WeaponSlotIdx), renderer)
        _                             <- state.action(testUser, tap("Unequip"), renderer)
        updated                       <- heroDao.getHeroByUserId(userId)
        screens                       <- renderer.sentScreens
      } yield assertTrue(updated.exists(_.equipment.weapon.itemType == ItemType.NoItem)) &&
              assertTrue(updated.exists(_.fightStats.atk == 0L)) &&
              assertTrue(invRepo.snapshot.exists(_.id == sword.id)) &&
              assertTrue(screens.exists(_.text.contains("снят")))
    },

    test("Unequip пустого слота → у детального экрана нет кнопки Unequip") {
      for {
        quad                          <- makeState(TestFixtures.hero(userId))
        (state, _, _, renderer)        = quad
        _                             <- state.enter(testUser, renderer)
        _                             <- state.action(testUser, selectSlot(WeaponSlotIdx), renderer)
        screens                       <- renderer.sentScreens
      } yield assertTrue(screens.last.choices.forall(_.id != "Unequip"))
    },

    test("Unequip при полном инвентаре → сообщение, предмет остаётся") {
      val heroWithSword = TestFixtures.hero(userId).copy(
        equipment = TestFixtures.emptyEquipment.copy(weapon = sword)
      )
      for {
        heroDao  <- TestHeroDao.withHero(userId, heroWithSword)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        invRepo   = TestInventoryRepository.full
        state     = EquipmentState(heroDao, invRepo, content)
        _        <- state.enter(testUser, renderer)
        _        <- state.action(testUser, selectSlot(WeaponSlotIdx), renderer)
        _        <- state.action(testUser, tap("Unequip"), renderer)
        updated  <- heroDao.getHeroByUserId(userId)
        screens  <- renderer.sentScreens
      } yield assertTrue(updated.exists(_.equipment.weapon.itemType == ItemType.Weapon)) &&
              assertTrue(screens.exists(_.text.contains("Сумка странника переполнена")))
    },

    test("надеть два кольца из инвентаря через детальные экраны → оба в разных слотах") {
      val heroBase = TestFixtures.hero(userId)
      for {
        heroDao  <- TestHeroDao.withHero(userId, heroBase)
        invRepo   = TestInventoryRepository.withItems(List(ring1, ring2))
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        invState  = InventoryState(heroDao, invRepo, TestItemRepository.make, content)
        _        <- invState.enter(testUser, renderer)
        _        <- invState.action(testUser, selectItem(ring1.id), renderer)
        _        <- invState.action(testUser, tap("Equip"), renderer)
        _        <- invState.action(testUser, selectItem(ring2.id), renderer)
        _        <- invState.action(testUser, tap("Equip"), renderer)
        updated  <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updated.exists(_.equipment.firstRing.id  == ring1.id)) &&
              assertTrue(updated.exists(_.equipment.secondRing.id == ring2.id))
    }
  )
}
