package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.model.item.{Gem, GemKind, Item, ItemType, MaterialKind, Rarity}
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
  private val Ring2SlotIdx  = 9

  private def wearItem(itemId: Long): UserAction =
    UserAction("", Some(s"""{"action":"EquipWear","id":"$itemId"}"""))

  private def blade(id: Long, name: String, lvl: Long, atk: Int): Item =
    Item(id, name, lvl, Rarity.Gray, ItemType.Weapon,
         attack = atk, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  private val sword = Item(10L, "Меч судьбы", 1L, Rarity.Blue, ItemType.Weapon,
                           attack = 20, accuracy = 5, energy = 0,
                           armor = 0, defence = 0, evasion = 0)

  private val ring1 = Item(11L, "Кольцо силы",   1L, Rarity.Gray, ItemType.Ring,
                           attack = 3, accuracy = 0, energy = 0,
                           armor = 0, defence = 0, evasion = 0)
  private val ring2 = Item(12L, "Кольцо ловкости", 1L, Rarity.Gray, ItemType.Ring,
                           attack = 0, accuracy = 0, energy = 0,
                           armor = 0, defence = 2, evasion = 0)

  private def heroWith(weapon: Item) =
    TestFixtures.hero(userId).copy(
      equipment = TestFixtures.emptyEquipment.copy(weapon = weapon))

  private def makeState(hero: pangea.model.hero.Hero, invItems: List[Item] = Nil) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      invRepo   = TestInventoryRepository.withItems(invItems)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (EquipmentState(heroDao, invRepo, content), heroDao, invRepo, renderer)

  override def spec = suite("EquipmentState")(

    // ── Ломка камней прямо на надетом ────────────────────────────────────────
    test("надетая вещь с камнем даёт красную кнопку ломки, без камней — нет") {
      val gemSword = sword.copy(sockets = List(Some(Gem(GemKind.Emerald, 4))))
      for {
        quad                    <- makeState(heroWith(gemSword))
        (state, _, _, renderer)  = quad
        _        <- state.action(testUser, selectSlot(WeaponSlotIdx), renderer)
        withGem  <- renderer.sentScreens.map(_.last.choices)
        quad2                   <- makeState(heroWith(sword))
        (state2, _, _, r2)       = quad2
        _        <- state2.action(testUser, selectSlot(WeaponSlotIdx), r2)
        without  <- r2.sentScreens.map(_.last.choices)
      } yield assertTrue(withGem.exists(_.id == "BreakWorn")) &&
              assertTrue(!without.exists(_.id == "BreakWorn"))
    },

    test("ломка на надетом: гнездо пустеет, пыль падает в сумку, вещь остаётся надетой") {
      val gemSword = sword.copy(sockets = List(Some(Gem(GemKind.Emerald, 4))))
      for {
        quad                          <- makeState(heroWith(gemSword))
        (state, heroDao, invRepo, renderer) = quad
        _       <- state.action(testUser, selectSlot(WeaponSlotIdx), renderer)
        _       <- state.action(testUser, tap("BreakWorn"), renderer)
        confirm <- renderer.sentScreens.map(_.last)
        _       <- state.action(testUser, UserAction("", Some("""{"action":"BreakWornDo","slot":"0"}""")), renderer)
        updated <- heroDao.getHeroByUserId(userId).map(_.get)
        inv     <- invRepo.get(updated.id)
      } yield assertTrue(confirm.text.contains("Сломать")) &&
              assertTrue(updated.equipment.weapon.id == gemSword.id) &&
              assertTrue(updated.equipment.weapon.sockets == List(None)) &&
              // безупречный изумруд — грейд 4, значит четыре горсти пыли
              assertTrue(inv.items.data.count(_.material.contains(MaterialKind.EmeraldDust)) == 4)
    },

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
              assertTrue(screens.exists(_.text.contains("надо бы очистить место прежде чем снять с себя")))
    },

    // ── Примерка из карточки слота ───────────────────────────────

    test("карточка слота: характеристики надетого, кнопками — подходящее из сумки по уровню") {
      val hero   = TestFixtures.hero(userId).copy(lvl = 5L,
                     equipment = TestFixtures.emptyEquipment.copy(weapon = sword))
      val fit1   = blade(21L, "Топор", lvl = 5L, atk = 30)
      val fit2   = blade(22L, "Кинжал", lvl = 1L, atk = 10)
      val tooBig = blade(23L, "Алебарда", lvl = 9L, atk = 90)
      val helmet = Item(24L, "Шлем", 1L, Rarity.Gray, ItemType.Helmet,
                        attack = 0, accuracy = 0, energy = 0, armor = 5, defence = 0, evasion = 0)
      for {
        quad                    <- makeState(hero, List(fit1, fit2, tooBig, helmet))
        (state, _, _, renderer)  = quad
        _      <- state.action(testUser, selectSlot(WeaponSlotIdx), renderer)
        screen <- renderer.sentScreens.map(_.last)
        worn    = screen.choices.filter(_.id == "EquipWear").flatMap(_.data.get("id"))
      } yield assertTrue(screen.text.contains(sword.name)) &&          // статы — надетого
              assertTrue(screen.text.contains("Подходит из сумки: 2")) &&
              // старшие сверху; не по уровню и чужой слот не предлагаются
              assertTrue(worn == List("21", "22")) &&
              assertTrue(screen.choices.exists(_.id == "Unequip"))
    },

    test("кнопка примерки надевает вещь, прежнюю кладёт в сумку и пересчитывает статы") {
      val hero  = TestFixtures.hero(userId).copy(lvl = 5L,
                    equipment  = TestFixtures.emptyEquipment.copy(weapon = sword),
                    fightStats = TestFixtures.hero(userId).fightStats.copy(atk = sword.attack.toLong))
      val axe   = blade(21L, "Топор", lvl = 5L, atk = 30)
      for {
        quad                              <- makeState(hero, List(axe))
        (state, heroDao, invRepo, renderer) = quad
        _       <- state.action(testUser, selectSlot(WeaponSlotIdx), renderer)
        _       <- state.action(testUser, wearItem(axe.id), renderer)
        updated <- heroDao.getHeroByUserId(userId).map(_.get)
        screens <- renderer.sentScreens.map(_.map(_.text))
      } yield assertTrue(updated.equipment.weapon.id == axe.id) &&
              assertTrue(updated.fightStats.atk == axe.attack.toLong) &&
              assertTrue(invRepo.snapshot.map(_.id) == List(sword.id)) &&
              assertTrue(screens.exists(_.contains("В сумку"))) &&
              // остались в карточке, и в ней уже новое оружие
              assertTrue(screens.last.contains(axe.name))
    },

    test("кольцо из карточки «Кольцо 2» меняет именно второе") {
      val hero    = TestFixtures.hero(userId).copy(
                      equipment = TestFixtures.emptyEquipment.copy(firstRing = ring1, secondRing = ring2))
      val newRing = Item(25L, "Кольцо воли", 1L, Rarity.Blue, ItemType.Ring,
                         attack = 7, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)
      for {
        quad                              <- makeState(hero, List(newRing))
        (state, heroDao, invRepo, renderer) = quad
        _       <- state.action(testUser, selectSlot(Ring2SlotIdx), renderer)
        _       <- state.action(testUser, wearItem(newRing.id), renderer)
        updated <- heroDao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(updated.equipment.firstRing.id  == ring1.id) &&
              assertTrue(updated.equipment.secondRing.id == newRing.id) &&
              assertTrue(invRepo.snapshot.map(_.id) == List(ring2.id))
    },

    test("подбор длиннее страницы листается") {
      val hero   = TestFixtures.hero(userId).copy(lvl = 5L)
      val blades = (1L to 9L).toList.map(i => blade(30L + i, s"Меч $i", lvl = 1L, atk = i.toInt))
      for {
        quad                    <- makeState(hero, blades)
        (state, _, _, renderer)  = quad
        _      <- state.action(testUser, selectSlot(WeaponSlotIdx), renderer)
        first  <- renderer.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("EquipPickNext"), renderer)
        second <- renderer.sentScreens.map(_.last)
      } yield assertTrue(first.choices.count(_.id == "EquipWear") == EquipmentState.PickPerPage) &&
              assertTrue(first.text.contains("стр. 1/2")) &&
              assertTrue(first.choices.exists(_.id == "EquipPickNext")) &&
              assertTrue(second.choices.count(_.id == "EquipWear") == 2) &&
              assertTrue(second.text.contains("стр. 2/2")) &&
              assertTrue(second.choices.exists(_.id == "EquipPickPrev"))
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
