package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.generator.item.TreasureMapGenerator
import pangea.model.hero.Hero
import pangea.engine.ChoiceColor
import pangea.generator.item.{GemGenerator, MaterialGenerator}
import pangea.model.item.{Gem, GemKind, Item, ItemType, MapZone, MaterialKind, Rarity}
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
    attack = 5, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)
  private val helm  = Item(20L, "Шлем стража", 2L, Rarity.White, ItemType.Helmet,
    attack = 0, accuracy = 2, energy = 0, armor = 3, defence = 1, evasion = 0)
  private val oldSword = Item(99L, "Ржавый меч", 1L, Rarity.Gray, ItemType.Weapon,
    attack = 2, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  // Две половинки карты Кинэт (dropLevel 10 и 20 → одна зона 1..25) и половинка
  // другой зоны (Ущелье мертвецов, 51..75) для проверки требования совпадения.
  private val kinetHalfA = TreasureMapGenerator.create(dropLevel = 10, half = true).copy(id = 30L)
  private val kinetHalfB = TreasureMapGenerator.create(dropLevel = 20, half = true).copy(id = 31L)
  private val gorgeHalf  = TreasureMapGenerator.create(dropLevel = 60, half = true).copy(id = 32L)

  private val baseHero = TestFixtures.hero(userId)

  // Меч с двумя занятыми гнёздами — на нём проверяется ломка камней.
  private val socketedSword = Item(50L, "Меч с камнями", 1L, Rarity.Blue, ItemType.Weapon,
    attack = 5, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
    sockets = List(Some(Gem(GemKind.Ruby, 3)), Some(Gem(GemKind.Topaz, 1))))

  private def dustItem(kind: MaterialKind, id: Long): Item =
    MaterialGenerator.item(kind).copy(id = id)

  private def breakSlot(slot: Int): UserAction =
    UserAction("", Some(s"""{"action":"BreakGemDo","slot":"$slot"}"""))

  private def ring(id: Long, name: String, evasion: Long) =
    Item(id, name, 1L, Rarity.Blue, ItemType.Ring,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = evasion)

  // Названия короткие: подписи кнопок режутся по лимиту длины (ItemMenu.truncate).
  private val wornRing1 = ring(41L, "Перстень", 1L)
  private val wornRing2 = ring(42L, "Печатка", 2L)
  private val newRing   = ring(43L, "Оникс", 5L)

  /** Нажатие на кнопку выбора слота кольца на экране замены. */
  private def chooseRingSlot(itemId: Long, slot: Int): UserAction =
    UserAction("", Some(s"""{"action":"EquipRing","id":"$itemId","slot":"$slot"}"""))

  // Герой с обоими занятыми слотами колец и новым кольцом в сумке.
  private def heroWithBothRings = baseHero.copy(
    equipment = TestFixtures.emptyEquipment.copy(firstRing = wornRing1, secondRing = wornRing2))

  private def makeState(hero: Hero, items: List[Item]) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      invRepo   = TestInventoryRepository.withItems(items)
      itemRepo  = TestItemRepository.make
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (InventoryState(heroDao, invRepo, itemRepo, content), heroDao, invRepo, renderer)

  override def spec = suite("InventoryState")(

    // ── Ломка камней ─────────────────────────────────────────────────────────
    test("у вещи с камнями есть красная кнопка ломки, у пустой — нет") {
      for {
        quad                    <- makeState(baseHero, List(socketedSword, sword))
        (state, _, _, renderer)  = quad
        _        <- state.action(testUser, selectItem(socketedSword.id), renderer)
        withGems <- renderer.sentScreens.map(_.last.choices)
        _        <- state.action(testUser, selectItem(sword.id), renderer)
        without  <- renderer.sentScreens.map(_.last.choices)
      } yield assertTrue(withGems.exists(c => c.id == "BreakGem" && c.color == ChoiceColor.Negative)) &&
              assertTrue(!without.exists(_.id == "BreakGem"))
    },

    test("камней несколько → сначала спрашиваем какой, потом подтверждение") {
      for {
        quad                    <- makeState(baseHero, List(socketedSword))
        (state, _, _, renderer)  = quad
        _      <- state.action(testUser, selectItem(socketedSword.id), renderer)
        _      <- state.action(testUser, tap("BreakGem"), renderer)
        which  <- renderer.sentScreens.map(_.last)
      } yield assertTrue(which.text.contains("несколько камней")) &&
              assertTrue(which.choices.count(_.id == "BreakGemPick") == 2) &&
              // в подписях — имена камней с грейдом
              assertTrue(which.choices.exists(_.label.contains("Рубин"))) &&
              assertTrue(which.choices.exists(_.label.contains("Надколотый топаз")))
    },

    test("ломка камня из вещи: гнездо пустеет, в сумке появляется пыль по грейду") {
      for {
        quad                       <- makeState(baseHero, List(socketedSword))
        (state, _, invRepo, renderer) = quad
        _     <- state.action(testUser, selectItem(socketedSword.id), renderer)
        _     <- state.action(testUser, breakSlot(0), renderer)   // рубин грейда 3
        inv   <- invRepo.get(baseHero.id)
        blade  = inv.items.data.find(_.id == socketedSword.id).get
        dusts  = inv.items.data.filter(_.material.contains(MaterialKind.RubyDust))
        log   <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(blade.sockets == List(None, Some(Gem(GemKind.Topaz, 1)))) &&
              assertTrue(dusts.size == 3) &&
              assertTrue(log.contains("рассыпался"))
    },

    test("камень в сумке крошится целиком: сам пропадает, остаётся его пыль") {
      val stone = GemGenerator.item(GemKind.Sapphire, 2).copy(id = 77L)
      for {
        quad                       <- makeState(baseHero, List(stone))
        (state, _, invRepo, renderer) = quad
        _     <- state.action(testUser, selectItem(stone.id), renderer)
        _     <- state.action(testUser, tap("CrushGem"), renderer)
        offer <- renderer.sentScreens.map(_.last)
        _     <- state.action(testUser, tap("CrushGemDo"), renderer)
        inv   <- invRepo.get(baseHero.id)
      } yield assertTrue(offer.text.contains("Растолочь")) &&
              assertTrue(!inv.items.data.exists(_.id == stone.id)) &&
              assertTrue(inv.items.data.count(_.material.contains(MaterialKind.SapphireDust)) == 2)
    },

    // ── Пыль на оружие ───────────────────────────────────────────────────────
    test("у пыли есть кнопка «Применить на оружие», покрытие ложится слоем") {
      val hero = baseHero.copy(equipment = TestFixtures.emptyEquipment.copy(weapon = sword))
      for {
        quad                      <- makeState(hero, List(dustItem(MaterialKind.TopazDust, 60L)))
        (state, heroDao, invRepo, renderer) = quad
        _       <- state.action(testUser, selectItem(60L), renderer)
        choices <- renderer.sentScreens.map(_.last.choices)
        _       <- state.action(testUser, tap("DustWeapon"), renderer)
        updated <- heroDao.getHeroByUserId(userId).map(_.get)
        inv     <- invRepo.get(hero.id)
        log     <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(choices.exists(_.id == "DustWeapon")) &&
              assertTrue(updated.weaponDust.layers == List(MaterialKind.TopazDust)) &&
              // горсть потрачена
              assertTrue(inv.items.data.isEmpty) &&
              assertTrue(log.contains("осела"))
    },

    test("сапфировая пыль на оружие с рубином → всполох и штраф вместо эффекта") {
      val rubySword = sword.copy(sockets = List(Some(Gem(GemKind.Ruby, 2))))
      val hero = baseHero.copy(equipment = TestFixtures.emptyEquipment.copy(weapon = rubySword))
      for {
        quad                      <- makeState(hero, List(dustItem(MaterialKind.SapphireDust, 61L)))
        (state, heroDao, _, renderer) = quad
        _       <- state.action(testUser, selectItem(61L), renderer)
        _       <- state.action(testUser, tap("DustWeapon"), renderer)
        updated <- heroDao.getHeroByUserId(userId).map(_.get)
        log     <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(log.contains("всполох магии")) &&
              assertTrue(updated.weaponDust.layers.isEmpty) &&
              assertTrue(updated.weaponDust.penalty)
    },

    test("без надетого оружия сыпать некуда — пыль остаётся в сумке") {
      for {
        quad                      <- makeState(baseHero, List(dustItem(MaterialKind.RubyDust, 62L)))
        (state, heroDao, invRepo, renderer) = quad
        _       <- state.action(testUser, selectItem(62L), renderer)
        _       <- state.action(testUser, tap("DustWeapon"), renderer)
        updated <- heroDao.getHeroByUserId(userId).map(_.get)
        inv     <- invRepo.get(baseHero.id)
        log     <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(log.contains("оружие не надето")) &&
              assertTrue(updated.weaponDust.isEmpty) &&
              assertTrue(inv.items.data.size == 1)
    },

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

    // ── Кольца: выбор слота при двух занятых ──────────────────────────────────
    test("Equip кольца при двух занятых слотах → экран выбора, кольцо пока не надето") {
      for {
        quad                                <- makeState(heroWithBothRings, List(newRing))
        (state, heroDao, invRepo, renderer)  = quad
        _        <- state.action(testUser, selectItem(newRing.id), renderer)
        _        <- state.action(testUser, tap("Equip"), renderer)
        screens  <- renderer.sentScreens
        hero     <- heroDao.getHeroByUserId(userId)
        btns      = screens.last.choices
      } yield assertTrue(btns.count(_.id == "EquipRing") == 2) &&
              // в подписях видно, какое кольцо снимаем
              assertTrue(btns.exists(_.label.contains(wornRing1.name))) &&
              assertTrue(btns.exists(_.label.contains(wornRing2.name))) &&
              assertTrue(screens.last.text.contains(newRing.name)) &&
              // ничего ещё не произошло: кольца на местах, новое в сумке
              assertTrue(hero.exists(_.equipment.firstRing.id == wornRing1.id)) &&
              assertTrue(hero.exists(_.equipment.secondRing.id == wornRing2.id)) &&
              assertTrue(invRepo.snapshot.map(_.id) == List(newRing.id))
    },

    test("выбор первого слота → меняется первое кольцо, второе не тронуто") {
      for {
        quad                                <- makeState(heroWithBothRings, List(newRing))
        (state, heroDao, invRepo, renderer)  = quad
        _    <- state.action(testUser, selectItem(newRing.id), renderer)
        _    <- state.action(testUser, tap("Equip"), renderer)
        _    <- state.action(testUser, chooseRingSlot(newRing.id, 1), renderer)
        hero <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(hero.exists(_.equipment.firstRing.id == newRing.id)) &&
              assertTrue(hero.exists(_.equipment.secondRing.id == wornRing2.id)) &&
              assertTrue(invRepo.snapshot.map(_.id) == List(wornRing1.id)) // снятое вернулось в сумку
    },

    test("выбор второго слота → меняется второе кольцо, первое не тронуто") {
      for {
        quad                                <- makeState(heroWithBothRings, List(newRing))
        (state, heroDao, invRepo, renderer)  = quad
        _    <- state.action(testUser, selectItem(newRing.id), renderer)
        _    <- state.action(testUser, tap("Equip"), renderer)
        _    <- state.action(testUser, chooseRingSlot(newRing.id, 2), renderer)
        hero <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(hero.exists(_.equipment.firstRing.id == wornRing1.id)) &&
              assertTrue(hero.exists(_.equipment.secondRing.id == newRing.id)) &&
              assertTrue(invRepo.snapshot.map(_.id) == List(wornRing2.id))
    },

    test("статы пересчитываются по реально снятому кольцу, а не по второму слоту") {
      // Слот 1 даёт +1 уклонения, новое кольцо +5: замена первого → +4 к уклонению.
      val hero0 = heroWithBothRings.copy(
        fightStats = baseHero.fightStats.copy(
          evasion = baseHero.fightStats.evasion + wornRing1.evasion + wornRing2.evasion))
      for {
        quad                          <- makeState(hero0, List(newRing))
        (state, heroDao, _, renderer)  = quad
        _    <- state.action(testUser, selectItem(newRing.id), renderer)
        _    <- state.action(testUser, tap("Equip"), renderer)
        _    <- state.action(testUser, chooseRingSlot(newRing.id, 1), renderer)
        hero <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(hero.exists(_.fightStats.evasion ==
                hero0.fightStats.evasion - wornRing1.evasion + newRing.evasion))
    },

    test("при свободном слоте кольцо надевается сразу, без экрана выбора") {
      val heroOneRing = baseHero.copy(
        equipment = TestFixtures.emptyEquipment.copy(firstRing = wornRing1))
      for {
        quad                                <- makeState(heroOneRing, List(newRing))
        (state, heroDao, invRepo, renderer)  = quad
        _       <- state.action(testUser, selectItem(newRing.id), renderer)
        _       <- state.action(testUser, tap("Equip"), renderer)
        screens <- renderer.sentScreens
        hero    <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(!screens.exists(_.choices.exists(_.id == "EquipRing"))) &&
              assertTrue(hero.exists(_.equipment.firstRing.id == wornRing1.id)) &&
              assertTrue(hero.exists(_.equipment.secondRing.id == newRing.id)) &&
              assertTrue(invRepo.snapshot.isEmpty)
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
        attack = 100, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)
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
