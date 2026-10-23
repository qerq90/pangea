package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.generator.item.TreasureMapGenerator
import pangea.model.hero.Hero
import pangea.engine.ChoiceColor
import pangea.generator.item.{GemGenerator, MaterialGenerator}
import pangea.model.item.{BeltSlot, FlaskKind, Gem, GemKind, Grip, Item, ItemDetails, ItemType, MapZone, MaterialKind, Rarity}
import pangea.model.artifact.ArtifactKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.{BeltContents, UserAction}
import pangea.test.{TestArtifactRepository, TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
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

  // Пояс с содержимым и ячейки для него.
  private def beltItem(id: Long, name: String, rarity: Rarity, slots: List[BeltSlot] = Nil): Item =
    Item(id, name, 1L, rarity, ItemType.Belt,
      attack = 0, accuracy = 0, energy = 0, armor = 1, defence = 0, evasion = 0, details = ItemDetails.Belt(slots))
  private val healer = BeltSlot.Flask(FlaskKind.Healer, Rarity.Blue, 3)
  private val ruby   = BeltSlot.Dust(MaterialKind.RubyDust)
  private def wearingBelt(belt: Item): Hero = baseHero.copy(equipment = baseHero.equipment.copy(belt = belt))
  private def beltSwap(slot: Int): UserAction =
    UserAction("", Some(s"""{"action":"BeltSwap","slot":"$slot"}"""))

  private def breakSlot(slot: Int): UserAction =
    UserAction("", Some(s"""{"action":"BreakGemDo","slot":"$slot"}"""))

  private def ring(id: Long, name: String, evasion: Long) =
    Item(id, name, 1L, Rarity.Blue, ItemType.Ring,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = evasion)

  // Названия короткие: подписи кнопок режутся по лимиту длины (ItemMenu.truncate).
  private val wornRing1 = ring(41L, "Перстень", 1L)
  private val wornRing2 = ring(42L, "Печатка", 2L)
  private val newRing   = ring(43L, "Оникс", 5L)

  /** Нажатие на кнопку выбора руки на экране замены оружия. */
  private def chooseHand(itemId: Long, hand: Int): UserAction =
    UserAction("", Some(s"""{"action":"EquipHand","id":"$itemId","slot":"$hand"}"""))

  /** Нажатие на кнопку выбора слота кольца на экране замены. */
  private def chooseRingSlot(itemId: Long, slot: Int): UserAction =
    UserAction("", Some(s"""{"action":"EquipRing","id":"$itemId","slot":"$slot"}"""))

  // Герой с обоими занятыми слотами колец и новым кольцом в сумке.
  private def heroWithBothRings = baseHero.copy(
    equipment = TestFixtures.emptyEquipment.copy(firstRing = wornRing1, secondRing = wornRing2))

  private def makeState(hero: Hero, items: List[Item],
                        artifacts: Option[TestArtifactRepository] = None) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      invRepo   = TestInventoryRepository.withItems(items)
      itemRepo  = TestItemRepository.make
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (InventoryState(heroDao, invRepo, itemRepo, content, artifacts), heroDao, invRepo, renderer)

  override def spec = suite("InventoryState")(

    // ── Складывание одинаковых вещей ─────────────────────────────────────────
    test("«Назад» с карточки возвращает на ту же страницу, а не в начало") {
      // Три страницы разных вещей: имена разные, иначе они сложатся в стопку.
      val many = (1L to 20L).toList.map(i =>
        Item(100L + i, s"Вещь $i", 1L, Rarity.Gray, ItemType.Weapon,
          attack = 1, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0))
      for {
        t <- makeState(baseHero, many)
        (state, _, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        _       <- state.action(testUser, tap("InventoryNext"), renderer)
        _       <- state.action(testUser, tap("InventoryNext"), renderer)
        third   <- renderer.sentScreens.map(_.last)
        // открываем вещь с третьей страницы и просто уходим назад
        onPage   = third.choices.map(_.id).filter(_.startsWith(InventoryState.ItemActionPrefix))
        _       <- state.action(testUser, UserAction("", Some(s"""{"action":"${onPage.head}"}""")), renderer)
        card    <- renderer.sentScreens.map(_.last)
        _       <- state.action(testUser, tap("InventoryList"), renderer)
        back    <- renderer.sentScreens.map(_.last)
      } yield assertTrue(third.text.contains("(3/3)") && onPage.nonEmpty) &&
              assertTrue(card.choices.map(_.id).contains("InventoryList")) &&
              // та же третья страница с теми же кнопками, а не первая
              assertTrue(back.text.contains("(3/3)")) &&
              assertTrue(back.choices.map(_.id).filter(_.startsWith(InventoryState.ItemActionPrefix)) == onPage)
    },

    test("сюжетный предмет в списке и на карточке — со звёздочкой перед именем, без редкости и уровня") {
      val letter = pangea.model.item.QuestItemKind.item(pangea.model.item.QuestItemKind.MarisaLetter).copy(id = 77L)
      for {
        t <- makeState(baseHero, List(letter))
        (state, _, _, renderer) = t
        _    <- state.enter(testUser, renderer)
        list <- renderer.sentScreens.map(_.last)
        _    <- state.action(testUser, tap(s"${InventoryState.ItemActionPrefix}77"), renderer)
        card <- renderer.sentScreens.map(_.last)
      } yield assertTrue(list.choices.exists(_.label == "★ Старое письмо Марисе")) &&
              assertTrue(card.text.startsWith("★ Старое письмо Марисе")) &&
              assertTrue(letter.displayTitle == "★ Старое письмо Марисе")
    },

    test("три одинаковых камня — одна кнопка с количеством") {
      val stones = (1 to 3).map(i =>
        GemGenerator.item(GemKind.Amethyst, 1).copy(id = 200L + i)).toList
      for {
        quad                    <- makeState(baseHero, stones :+ sword)
        (state, _, _, renderer)  = quad
        _       <- state.enter(testUser, renderer)
        choices <- renderer.sentScreens.map(_.last.choices)
        labels   = choices.map(_.label)
      } yield assertTrue(labels.exists(_.contains("(3 шт)"))) &&
              // камень + меч = две кнопки предметов, а не четыре
              assertTrue(choices.count(_.id.startsWith(InventoryState.ItemActionPrefix)) == 2)
    },

    test("рунные камни одного узора — одна кнопка; малые и большие порознь") {
      val quick = pangea.model.rune.Rune.Active(pangea.model.skill.Skill.QuickStrike)
      val runes = (1 to 5).map(i =>
        pangea.model.rune.RuneStone.item(quick, pangea.model.rune.RuneStoneSize.Small).copy(id = 300L + i)).toList :+
        pangea.model.rune.RuneStone.item(quick, pangea.model.rune.RuneStoneSize.Big).copy(id = 320L)
      for {
        quad                    <- makeState(baseHero, runes)
        (state, _, _, renderer)  = quad
        _       <- state.enter(testUser, renderer)
        choices <- renderer.sentScreens.map(_.last.choices)
        labels   = choices.filter(_.id.startsWith(InventoryState.ItemActionPrefix)).map(_.label)
      } yield assertTrue(labels.size == 2) &&
              assertTrue(labels.exists(l => l.contains("Малая руна") && l.contains("(5 шт)"))) &&
              assertTrue(labels.exists(l => l.contains("Большая руна") && !l.contains("шт")))
    },

    test("разные грейды одного камня стоят порознь") {
      val stones = List(
        GemGenerator.item(GemKind.Ruby, 1).copy(id = 210L),
        GemGenerator.item(GemKind.Ruby, 1).copy(id = 211L),
        GemGenerator.item(GemKind.Ruby, 3).copy(id = 212L))
      for {
        quad                    <- makeState(baseHero, stones)
        (state, _, _, renderer)  = quad
        _       <- state.enter(testUser, renderer)
        choices <- renderer.sentScreens.map(_.last.choices)
        labels   = choices.filter(_.id.startsWith(InventoryState.ItemActionPrefix)).map(_.label)
      } yield assertTrue(labels.size == 2) &&
              assertTrue(labels.exists(l => l.contains("Надколотый рубин") && l.contains("(2 шт)"))) &&
              assertTrue(labels.exists(l => l.contains("Рубин") && !l.contains("шт")))
    },

    test("экран стопки говорит, сколько таких в сумке, а выбрасывается один") {
      val stones = (1 to 3).map(i =>
        GemGenerator.item(GemKind.Emerald, 2).copy(id = 220L + i)).toList
      for {
        quad                          <- makeState(baseHero, stones)
        (state, _, invRepo, renderer)  = quad
        _      <- state.action(testUser, selectItem(221L), renderer)
        screen <- renderer.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("Drop"), renderer)
        left    = invRepo.snapshot
        after  <- renderer.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("Drop"), renderer)
        _      <- state.action(testUser, tap("Drop"), renderer)
        last   <- renderer.sentScreens.map(_.last)
      } yield assertTrue(screen.text.contains("В сумке таких: 3")) &&
              // ушёл ровно один камень из стопки — последний: показанный первым
              // остаётся, и карточка стоит на месте
              assertTrue(left.size == 2) &&
              assertTrue(!left.exists(_.id == 223L) && left.exists(_.id == 221L)) &&
              // и мы остались на карточке стопки — с тем же камнем и счётом 2
              assertTrue(after.text.contains("В сумке таких: 2") && after.choices.exists(_.id == "Drop")) &&
              // выбросили последний — только теперь список
              assertTrue(invRepo.snapshot.isEmpty && !last.choices.exists(_.id == "Drop"))
    },

    test("кошелёк из стопки: вернуть и развязать — остаёмся на карточке, уходит последний") {
      import pangea.service.state.states.events.thieves.ThievesState
      import pangea.model.item.TrophyKind
      // Кошельки разного уровня лежат одной стопкой; благодарность — по тому,
      // что ушёл, то есть по последнему.
      val purses = List(ThievesState.purse(10L).copy(id = 401L),
                        ThievesState.purse(20L).copy(id = 402L),
                        ThievesState.purse(30L).copy(id = 403L))
      for {
        quad                             <- makeState(baseHero, purses)
        (state, heroDao, invRepo, renderer) = quad
        _       <- state.action(testUser, selectItem(403L), renderer)   // кнопка из списка
        opened  <- renderer.sentScreens.map(_.last)
        _       <- state.action(testUser, tap("PurseReturn"), renderer)
        hero    <- heroDao.getHeroByUserId(userId).map(_.get)
        after1  <- renderer.sentScreens.map(_.last)
        left1    = invRepo.snapshot.map(_.id)
        _       <- state.action(testUser, tap("PurseOpen"), renderer)
        after2  <- renderer.sentScreens.map(_.last)
        left2    = invRepo.snapshot.map(_.id)
        _       <- state.action(testUser, tap("PurseOpen"), renderer)
        gone    <- renderer.sentScreens.map(_.last)
      } yield assertTrue(opened.text.contains("В сумке таких: 3")) &&
              assertTrue(hero.guildReputation == TrophyKind.reputationFor(TrophyKind.Relic.coef, 30L)) &&
              // ушёл последний, а мы на той же карточке — с меньшим счётом
              assertTrue(left1 == List(401L, 402L) && after1.choices.exists(_.id == "PurseReturn")) &&
              assertTrue(after1.text.contains("В сумке таких: 2")) &&
              assertTrue(left2 == List(401L) && after2.choices.exists(_.id == "PurseOpen")) &&
              // развязали последний — карточки больше нет, список
              assertTrue(invRepo.snapshot.isEmpty && !gone.choices.exists(_.id == "PurseOpen"))
    },

    test("малые руны: складываются последние пять, карточка стопки остаётся") {
      import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
      import pangea.model.item.PassiveKind
      val rune  = Rune.Passive(PassiveKind.QuickHands)
      val small = (1 to 7).map(i => RuneStone.item(rune, RuneStoneSize.Small).copy(id = 500L + i)).toList
      for {
        quad                          <- makeState(baseHero, small)
        (state, _, invRepo, renderer)  = quad
        _      <- state.action(testUser, selectItem(507L), renderer)
        _      <- state.action(testUser, tap("CombineRune"), renderer)
        folded <- renderer.sentScreens.map(_.last)
        left    = invRepo.snapshot
        // двух не хватает на большую — отказ, но с карточки не уходим
        _      <- state.action(testUser, tap("CombineRune"), renderer)
        refused <- renderer.sentScreens.map(_.last)
      } yield assertTrue(left.count(_.isSmallRune) == 2 && left.exists(_.id == 501L) && left.exists(_.id == 502L)) &&
              assertTrue(left.exists(_.runeStone.exists(_.size == RuneStoneSize.Big))) &&
              assertTrue(folded.choices.exists(_.id == "CombineRune") && folded.text.contains("В сумке таких: 2")) &&
              assertTrue(refused.choices.exists(_.id == "CombineRune"))
    },

    test("«Нет» в подтверждении — назад на карточку стопки, а не в список") {
      val stones = (1 to 2).map(i => GemGenerator.item(GemKind.Ruby, 2).copy(id = 600L + i)).toList
      for {
        quad                          <- makeState(baseHero, stones)
        (state, _, invRepo, renderer)  = quad
        _      <- state.action(testUser, selectItem(602L), renderer)
        _      <- state.action(testUser, tap("CrushGem"), renderer)
        ask    <- renderer.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("ItemCard"), renderer)
        back   <- renderer.sentScreens.map(_.last)
      } yield assertTrue(ask.choices.exists(_.id == "ItemCard")) &&
              assertTrue(back.text.contains("В сумке таких: 2") && back.choices.exists(_.id == "CrushGem")) &&
              assertTrue(invRepo.snapshot.size == 2)
    },

    test("вставка камня из стопки: уходит последний, а вернёмся на карточку стопки") {
      import pangea.service.state.UiScene
      val stones = (1 to 3).map(i => GemGenerator.item(GemKind.Topaz, 1).copy(id = 700L + i)).toList
      for {
        quad                              <- makeState(baseHero, stones)
        (state, heroDao, _, renderer)      = quad
        _      <- state.action(testUser, selectItem(703L), renderer)
        next   <- state.action(testUser, tap("SocketInsert"), renderer)
        sock   <- heroDao.readSceneData(userId).map(_.flatMap(_.hcursor.downField(UiScene.Socketing).get[Long]("gemId").toOption))
        // экран вставки вернул нас в сумку
        _      <- state.enter(testUser, renderer)
        back   <- renderer.sentScreens.map(_.last)
        // а обычный вход в сумку — по-прежнему список
        _      <- state.enter(testUser, renderer)
        list   <- renderer.sentScreens.map(_.last)
      } yield assertTrue(next == StateType.Socketing && sock.contains(703L)) &&
              assertTrue(back.text.contains("В сумке таких: 3") && back.choices.exists(_.id == "SocketInsert")) &&
              assertTrue(!list.choices.exists(_.id == "SocketInsert"))
    },

    test("пыль из стопки сыплется по одной, не выкидывая из карточки") {
      val heroWithSword = baseHero.copy(equipment = TestFixtures.emptyEquipment.copy(weapon = sword))
      val dusts = (1 to 2).map(i => dustItem(MaterialKind.RubyDust, 300L + i)).toList
      for {
        quad                          <- makeState(heroWithSword, dusts)
        (state, _, invRepo, renderer)  = quad
        _      <- state.action(testUser, selectItem(302L), renderer)
        _      <- state.action(testUser, tap("DustWeapon"), renderer)
        after  <- renderer.sentScreens.map(_.last)
      } yield assertTrue(invRepo.snapshot.size == 1) &&
              assertTrue(after.choices.exists(_.id == "DustWeapon") && after.text.contains("Рубиновая пыль"))
    },

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

    test("у фляги нет «Надеть», только «На пояс»; старая кнопка «Надеть» её никуда не надевает") {
      val flask = pangea.generator.item.FlaskGenerator.item(pangea.model.item.FlaskKind.Healer, Rarity.Blue).copy(id = 60L)
      for {
        quad                                <- makeState(baseHero, List(flask))
        (state, heroDao, invRepo, renderer)  = quad
        _    <- state.action(testUser, selectItem(flask.id), renderer)
        card <- renderer.sentScreens.map(_.last)
        // «Надеть» со старой карточки фляги
        _    <- state.action(testUser, tap("Equip"), renderer)
        said <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
        hero <- heroDao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(!card.choices.exists(_.id == "Equip") && card.choices.exists(_.id == "BeltPut")) &&
              assertTrue(said.contains("Флягу не надевают")) &&
              // фляга осталась в сумке, снаряжение не тронуто
              assertTrue(hero.equipment == baseHero.equipment) &&
              assertTrue(invRepo.snapshot.map(_.id) == List(flask.id))
    },

    test("пояс поверх пояса: содержимое переезжает, не поместившееся — в сумку, старый пояс — пустым") {
      val worn  = beltItem(80L, "Старый пояс", Rarity.Green, List(healer, ruby))
      val small = beltItem(81L, "Новый пояс", Rarity.Gray)
      for {
        quad <- makeState(wearingBelt(worn), List(small))
        (state, heroDao, invRepo, renderer) = quad
        _    <- state.action(testUser, selectItem(small.id), renderer)
        _    <- state.action(testUser, tap("Equip"), renderer)
        hero <- heroDao.getHeroByUserId(userId).map(_.get)
        said <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(hero.equipment.belt.id == 81L && BeltContents.slotsOf(hero.equipment.belt) == List(healer)) &&
              assertTrue(invRepo.snapshot.exists(i => i.id == 80L && BeltContents.slotsOf(i).isEmpty)) &&
              // пыль — сохранённой вещью с настоящим id, а не свежей с -1
              assertTrue(invRepo.snapshot.exists(i => i.name == MaterialKind.RubyDust.displayName && i.id > 0L)) &&
              assertTrue(said.contains("не поместившееся — в сумку: Рубиновая пыль"))
    },

    test("пояс меньше прежнего, а лишнему нет места в сумке → не надевается, своим отказом") {
      val worn  = beltItem(80L, "Старый пояс", Rarity.Green, List(ruby, healer))
      val small = beltItem(81L, "Новый пояс", Rarity.Gray)
      val bag   = small :: (1L to 19L).toList.map(i => helm.copy(id = 200L + i))
      for {
        quad <- makeState(wearingBelt(worn), bag)
        (state, heroDao, invRepo, renderer) = quad
        _    <- state.action(testUser, selectItem(small.id), renderer)
        _    <- state.action(testUser, tap("Equip"), renderer)
        hero <- heroDao.getHeroByUserId(userId).map(_.get)
        said <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(hero.equipment.belt.id == 80L && BeltContents.slotsOf(hero.equipment.belt) == List(ruby, healer)) &&
              assertTrue(invRepo.snapshot.size == 20) &&
              assertTrue(said.contains("Новый пояс меньше"))
    },

    test("«На пояс» при полном поясе → выбор, что снять; снятое — в сумку, новое — на его место") {
      val dust = dustItem(MaterialKind.RubyDust, 90L)
      for {
        quad   <- makeState(wearingBelt(beltItem(80L, "Пояс", Rarity.Gray, List(healer))), List(dust))
        (state, heroDao, invRepo, renderer) = quad
        _      <- state.action(testUser, selectItem(dust.id), renderer)
        _      <- state.action(testUser, tap("BeltPut"), renderer)
        choice <- renderer.sentScreens.map(_.last)
        _      <- state.action(testUser, beltSwap(0), renderer)
        hero   <- heroDao.getHeroByUserId(userId).map(_.get)
        said   <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(choice.text.contains("Что снять, чтобы положить Рубиновая пыль")) &&
              assertTrue(choice.choices.map(_.id) == List("BeltSwap", "BeltSwapBack")) &&
              assertTrue(choice.choices.headOption.map(_.label).contains(s"Вместо: ${FlaskKind.Healer.label}")) &&
              assertTrue(BeltContents.slotsOf(hero.equipment.belt) == List(ruby)) &&
              assertTrue(!invRepo.snapshot.exists(_.id == 90L)) &&
              assertTrue(invRepo.snapshot.exists(i => i.name == FlaskKind.Healer.itemName(Rarity.Blue) && i.id > 0L)) &&
              assertTrue(said.contains("на пояс, ") && said.contains("— в сумку"))
    },

    test("замена на поясе: сумке негде взять снятое → отказ; пустую ячейку меняют и при полной сумке") {
      val dust   = dustItem(MaterialKind.RubyDust, 90L)
      val full   = (1L to 20L).toList.map(i => helm.copy(id = 200L + i))
      val flask  = pangea.generator.item.FlaskGenerator.item(FlaskKind.Healer, Rarity.Blue).copy(id = 95L)
      for {
        // пыль невесома: уйдя с полки, места не освободит, а фляге его нужно
        q1   <- makeState(wearingBelt(beltItem(80L, "Пояс", Rarity.Gray, List(healer))), dust :: full)
        (s1, d1, i1, r1) = q1
        _    <- s1.action(testUser, selectItem(dust.id), r1)
        _    <- s1.action(testUser, beltSwap(0), r1)
        h1   <- d1.getHeroByUserId(userId).map(_.get)
        t1   <- r1.sentScreens.map(_.map(_.text).mkString("\n"))
        // высыпанная пыль — пустая ячейка: вернуть нечего, место в сумке не нужно
        q2   <- makeState(wearingBelt(beltItem(80L, "Пояс", Rarity.Gray, List(ruby.poured))), flask :: full.drop(1))
        (s2, d2, _, r2) = q2
        _    <- s2.action(testUser, selectItem(flask.id), r2)
        _    <- s2.action(testUser, tap("BeltPut"), r2)
        c2   <- r2.sentScreens.map(_.last)
        _    <- s2.action(testUser, beltSwap(0), r2)
        h2   <- d2.getHeroByUserId(userId).map(_.get)
        t2   <- r2.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(t1.contains("Сумка полна — снятой вещи некуда деться")) &&
              assertTrue(BeltContents.slotsOf(h1.equipment.belt) == List(healer) && i1.snapshot.exists(_.id == 90L)) &&
              assertTrue(c2.choices.headOption.map(_.label).contains("Вместо: Рубиновая пыль (пусто)")) &&
              assertTrue(BeltContents.slotsOf(h2.equipment.belt).headOption.exists(_.isInstanceOf[BeltSlot.Flask])) &&
              assertTrue(t2.contains("вместо пустой ячейки"))
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

    test("пять малых рун одного узора складываются в большую; четырёх мало, а в полную сумку большую не сложить") {
      import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
      import pangea.model.skill.Skill
      val rune  = Rune.Active(Skill.SweepingStrike)
      def small(id: Long) = RuneStone.item(rune, RuneStoneSize.Small).copy(id = id)
      val four  = (1L to 4L).toList.map(small)
      val five  = (1L to 5L).toList.map(small)
      for {
        // четырёх осколков мало
        few                     <- makeState(baseHero, four)
        (fs, _, finv, fr)        = few
        _                       <- fs.enter(testUser, fr)
        _                       <- fs.action(testUser, selectItem(1L), fr)
        card                    <- fr.sentScreens.map(_.last)
        _                       <- fs.action(testUser, tap("CombineRune"), fr)
        fewTexts                <- fr.sentScreens.map(_.map(_.text).mkString(" | "))
        // пятерых хватает
        ok                      <- makeState(baseHero, five)
        (os, _, oinv, or)        = ok
        _                       <- os.enter(testUser, or)
        _                       <- os.action(testUser, selectItem(1L), or)
        _                       <- os.action(testUser, tap("CombineRune"), or)
        okTexts                 <- or.sentScreens.map(_.map(_.text).mkString(" | "))
        // сумка забита — большую руну класть некуда
        full                    <- makeState(baseHero, five ++ (10L to 29L).toList.map(i => sword.copy(id = i)))
        (ps, _, pinv, pr)        = full
        _                       <- ps.enter(testUser, pr)
        _                       <- ps.action(testUser, selectItem(1L), pr)
        _                       <- ps.action(testUser, tap("CombineRune"), pr)
        fullTexts               <- pr.sentScreens.map(_.map(_.text).mkString(" | "))
      } yield assertTrue(card.choices.exists(_.id == "CombineRune") && !card.choices.exists(_.id == "Equip")) &&
              assertTrue(finv.snapshot.size == 4 && fewTexts.contains("осколков всего 4, а нужно 5")) &&
              assertTrue(oinv.snapshot.size == 1 && oinv.snapshot.head.runeStone.exists(_.size == RuneStoneSize.Big)) &&
              assertTrue(okTexts.contains("узор смыкается")) &&
              assertTrue(pinv.snapshot.count(_.isSmallRune) == 5 && fullTexts.contains("переполнена"))
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

    test("BackFromInventory без хранилищ → сразу в «Персонаж»: рюкзака такому герою не показывают") {
      for {
        quad                    <- makeState(baseHero, List(sword), Some(TestArtifactRepository.empty))
        (state, _, _, renderer)  = quad
        _                       <- state.enter(testUser, renderer)
        result                  <- state.action(testUser, tap("BackFromInventory"), renderer)
      } yield assertTrue(result == StateType.HeroStats)
    },

    test("BackFromInventory с ларцом → возврат в рюкзак, откуда сюда и зашли") {
      val casket = TestArtifactRepository.of(
        casket = TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1))
      for {
        quad                    <- makeState(baseHero, List(sword), Some(casket))
        (state, _, _, renderer)  = quad
        _                       <- state.enter(testUser, renderer)
        result                  <- state.action(testUser, tap("BackFromInventory"), renderer)
      } yield assertTrue(result == StateType.Backpack)
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
      // Двуручное занимает обе руки: одноручное встаёт вместо него, а не рядом.
      val heroWithWeapon = baseHero.copy(
        equipment  = TestFixtures.emptyEquipment.copy(weapon = oldSword.copy(grip = Some(Grip.TwoHanded))),
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

    // ── Руки: свободная, выбор руки, двуручное ───────────────────────────────
    test("свободная левая рука: второе одноручное встаёт в неё, первое остаётся") {
      val heroWithWeapon = baseHero.copy(equipment = TestFixtures.emptyEquipment.copy(weapon = oldSword))
      for {
        quad                                <- makeState(heroWithWeapon, List(sword))
        (state, heroDao, invRepo, renderer)  = quad
        _    <- state.action(testUser, selectItem(sword.id), renderer)
        _    <- state.action(testUser, tap("Equip"), renderer)
        hero <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(hero.exists(h => h.equipment.weapon.id == oldSword.id && h.equipment.leftHand.id == sword.id)) &&
              assertTrue(invRepo.snapshot.isEmpty)
    },

    test("обе руки заняты одноручным → экран выбора руки; выбор левой меняет левую") {
      val third = sword.copy(id = 11L, name = "Кинжал")
      val armed = baseHero.copy(equipment = TestFixtures.emptyEquipment.copy(weapon = oldSword, leftHand = sword))
      for {
        quad                                <- makeState(armed, List(third))
        (state, heroDao, invRepo, renderer)  = quad
        _       <- state.action(testUser, selectItem(third.id), renderer)
        _       <- state.action(testUser, tap("Equip"), renderer)
        screens <- renderer.sentScreens
        before  <- heroDao.getHeroByUserId(userId)
        _       <- state.action(testUser, chooseHand(third.id, 2), renderer)
        after   <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(screens.last.choices.count(_.id == "EquipHand") == 2) &&
              assertTrue(screens.last.choices.exists(_.label.contains(EquipmentState.LeftHandName))) &&
              // пока руку не выбрали, ничего не поменялось
              assertTrue(before.exists(_.equipment.leftHand.id == sword.id)) &&
              assertTrue(after.exists(h => h.equipment.weapon.id == oldSword.id && h.equipment.leftHand.id == third.id)) &&
              assertTrue(invRepo.snapshot.map(_.id) == List(sword.id))
    },

    test("двуручное снимает оба одноручных; в полную сумку его не надеть") {
      val big   = Item(12L, "Секира", 1L, Rarity.Gray, ItemType.Weapon,
        attack = 9, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, grip = Some(Grip.TwoHanded))
      val armed = baseHero.copy(equipment = TestFixtures.emptyEquipment.copy(weapon = oldSword, leftHand = sword))
      // Сумка на двадцать мест: секира и девятнадцать шлемов — второму снятому некуда.
      val junk  = (1L to 19L).toList.map(i => helm.copy(id = 100L + i))
      for {
        roomy                               <- makeState(armed, List(big))
        (state, heroDao, invRepo, renderer)  = roomy
        _     <- state.action(testUser, selectItem(big.id), renderer)
        _     <- state.action(testUser, tap("Equip"), renderer)
        hero  <- heroDao.getHeroByUserId(userId)
        full                                <- makeState(armed, big :: junk)
        (state2, heroDao2, invRepo2, r2)     = full
        _     <- state2.action(testUser, selectItem(big.id), r2)
        _     <- state2.action(testUser, tap("Equip"), r2)
        said  <- r2.sentScreens.map(_.map(_.text).mkString("\n"))
        hero2 <- heroDao2.getHeroByUserId(userId)
      } yield assertTrue(hero.exists(h => h.equipment.weapon.id == big.id && h.equipment.leftHand.itemType == ItemType.NoItem)) &&
              assertTrue(invRepo.snapshot.map(_.id).toSet == Set(oldSword.id, sword.id)) &&
              assertTrue(said.contains("второй вещи в сумке нет места")) &&
              assertTrue(hero2.exists(h => h.equipment.weapon.id == oldSword.id && h.equipment.leftHand.id == sword.id)) &&
              assertTrue(invRepo2.snapshot.exists(_.id == big.id))
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
