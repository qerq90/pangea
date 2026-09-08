package pangea.service.state.states.merchant

import pangea.engine.SceneContent
import pangea.generator.item.GemGenerator
import pangea.model.item.{Item, ItemDetails, ItemType, GemKind, Rarity, TrophyKind}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.model.skill.Skill
import pangea.service.state.states.merchant.MerchantState.{JunkRarityGroups, JunkSaleSettings, MerchantData}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.{durationInt, Task, ZIO}
import zio.test._

object MerchantStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def tapIdx(key: String, idx: Int): UserAction =
    UserAction("", Some(s"""{"action":"$key","idx":"$idx"}"""))
  // Нажатие на переключатель редкости в настройке продажи хлама.
  private def tapRarity(group: String): UserAction =
    UserAction("", Some(s"""{"action":"JunkRarity","g":"$group"}"""))

  private def gear(id: Long, rarity: Rarity, details: ItemDetails = ItemDetails.Plain,
                   itemType: ItemType = ItemType.Helmet): Item =
    Item(id, s"Предмет $id", 1L, rarity, itemType,
      attack = 0, accuracy = 0, energy = 0, armor = 1, defence = 0, evasion = 0, details = details)

  // Трофей: редкость у них всегда формально Серая, вид/раса для продажи не важны.
  private def trophyItem(id: Long): Item =
    Item(id, "Голова (Человек)", 5L, Rarity.Gray, ItemType.Trophy,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Trophy("Human", TrophyKind.Head))

  private def richHero = TestFixtures.hero(userId).copy(lvl = 10L, silver = 1000000L)
  private def poorHero = TestFixtures.hero(userId).copy(lvl = 10L, silver = 0L)

  private def makeState(hero: pangea.model.hero.Hero, items: List[Item] = Nil) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      invRepo   = TestInventoryRepository.withItems(items)
      itemRepo  = TestItemRepository.make
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (MerchantState(heroDao, invRepo, itemRepo, content), heroDao, invRepo, renderer)

  private def readMerchant(heroDao: TestHeroDao): Task[MerchantData] =
    heroDao.readMerchantData(userId)
      .map(_.flatMap(_.as[MerchantData].toOption))
      .flatMap(o => ZIO.fromOption(o).orElseFail(new Throwable("no merchant data")))

  override def spec = suite("MerchantState")(

    test("enter → раскладывает 3 предмета и кнопки Купить/Обновить/Продать/Назад") {
      for {
        t <- makeState(richHero)
        (state, heroDao, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
        data    <- readMerchant(heroDao)
        ids      = screens.last.choices.map(_.id)
      } yield assertTrue(data.items.size == 3) &&
              assertTrue(ids.count(_ == "Buy") == 3) &&
              assertTrue(ids.contains("Refresh") && ids.contains("Sell") && ids.contains("Back"))
    },

    test("предметы зафиксированы: повторный enter не меняет сток") {
      for {
        t <- makeState(richHero)
        (state, heroDao, _, renderer) = t
        _     <- state.enter(testUser, renderer)
        data1 <- readMerchant(heroDao)
        _     <- state.enter(testUser, renderer)
        data2 <- readMerchant(heroDao)
      } yield assertTrue(data1.items.map(_.item.name) == data2.items.map(_.item.name)) &&
              assertTrue(data1.refreshedAt == data2.refreshedAt)
    },

    test("Buy → экран подтверждения с ценой") {
      for {
        t <- makeState(richHero)
        (state, heroDao, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        data    <- readMerchant(heroDao)
        _       <- state.action(testUser, tapIdx("Buy", 0), renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.text.contains("уверены")) &&
              assertTrue(screens.last.text.contains(data.items.head.price.toString)) &&
              assertTrue(screens.last.choices.map(_.id).toSet == Set("ConfirmBuy", "CancelBuy"))
    },

    test("ConfirmBuy с достаточным серебром → списывает цену, кладёт предмет, помечает купленным") {
      for {
        t <- makeState(richHero)
        (state, heroDao, invRepo, renderer) = t
        _       <- state.enter(testUser, renderer)
        data    <- readMerchant(heroDao)
        price    = data.items.head.price
        _       <- state.action(testUser, tapIdx("ConfirmBuy", 0), renderer)
        hero    <- heroDao.getHeroByUserId(userId)
        data2   <- readMerchant(heroDao)
        screens <- renderer.sentScreens
      } yield assertTrue(invRepo.snapshot.size == 1) &&
              assertTrue(hero.exists(_.silver == 1000000L - price)) &&
              assertTrue(data2.items.head.bought) &&
              assertTrue(screens.exists(_.text.contains("купили")))
    },

    test("ConfirmBuy без серебра → сообщение «как будет достаточно звонких монет», ничего не списано") {
      for {
        t <- makeState(poorHero)
        (state, heroDao, invRepo, renderer) = t
        _       <- state.enter(testUser, renderer)
        _       <- state.action(testUser, tapIdx("ConfirmBuy", 0), renderer)
        hero    <- heroDao.getHeroByUserId(userId)
        data    <- readMerchant(heroDao)
        screens <- renderer.sentScreens
      } yield assertTrue(invRepo.snapshot.isEmpty) &&
              assertTrue(hero.exists(_.silver == 0L)) &&
              assertTrue(!data.items.head.bought) &&
              assertTrue(screens.exists(_.text.contains("звонких монет")))
    },

    test("Refresh в пределах часа → сообщение про кулдаун, сток не меняется") {
      for {
        t <- makeState(richHero)
        (state, heroDao, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        data1   <- readMerchant(heroDao)
        _       <- state.action(testUser, tap("Refresh"), renderer)
        data2   <- readMerchant(heroDao)
        screens <- renderer.sentScreens
      } yield assertTrue(data1.items.map(_.item.name) == data2.items.map(_.item.name)) &&
              assertTrue(screens.exists(_.text.contains("Загляните")))
    },

    test("Sell → показывает список предметов; выбор → цена; ConfirmSellItem начисляет серебро и убирает предмет") {
      // green helmet lvl 5 → sellPrice = (5+5)*1.2*4 = 48
      val helmet = Item(7L, "Шлем", 5L, Rarity.Green, ItemType.Helmet,
        attack = 0, accuracy = 0, energy = 0, armor = 10, defence = 1, evasion = 0)
      val selectHelmet = UserAction("", Some(s"""{"action":"${MerchantState.SellItemPrefix}${helmet.id}"}"""))
      for {
        t <- makeState(richHero.copy(silver = 100L), items = List(helmet))
        (state, heroDao, invRepo, renderer) = t
        _        <- state.action(testUser, tap("Sell"), renderer)
        listScr  <- renderer.sentScreens.map(_.last)
        _        <- state.action(testUser, selectHelmet, renderer)
        confScr  <- renderer.sentScreens.map(_.last)
        _        <- state.action(testUser, tap("ConfirmSellItem"), renderer)
        hero     <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(listScr.choices.exists(_.id == s"${MerchantState.SellItemPrefix}${helmet.id}")) &&
              assertTrue(confScr.text.contains("48")) &&
              assertTrue(invRepo.snapshot.isEmpty) &&
              assertTrue(hero.exists(_.silver == 100L + 48L))
    },

    test("SellJunk продаёт только серое/белое снаряжение, не трогая трофеи и камни (даже Серой редкости)") {
      val junkHelmet = Item(1L, "Ржавый шлем", 1L, Rarity.Gray, ItemType.Helmet,
        attack = 0, accuracy = 0, energy = 0, armor = 2, defence = 0, evasion = 0)
      val gem = GemGenerator.item(GemKind.Skull, 1).copy(id = 2L) // всегда Rarity.Gray
      val trophy = Item(3L, "Голова (Человек)", 5L, Rarity.Gray, ItemType.Trophy,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
        details = ItemDetails.Trophy("Human", TrophyKind.Head))
      for {
        t <- makeState(richHero, items = List(junkHelmet, gem, trophy))
        (state, heroDao, invRepo, renderer) = t
        _    <- state.action(testUser, tap("SellJunk"), renderer)
        hero <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(!invRepo.snapshot.exists(_.id == junkHelmet.id)) && // хлам продан
              assertTrue(invRepo.snapshot.exists(_.id == gem.id)) &&         // камень остался
              assertTrue(invRepo.snapshot.exists(_.id == trophy.id)) &&      // трофей остался
              assertTrue(hero.exists(_.silver > richHero.silver))
    },

    // ── Настройка продажи хлама ───────────────────────────────────────────────
    test("JunkSettings → 6 переключателей редкости, 2 способностей и Назад; по умолчанию вкл ⚫ и ⚪") {
      for {
        t <- makeState(richHero)
        (state, _, _, renderer) = t
        _       <- state.action(testUser, tap("JunkSettings"), renderer)
        screens <- renderer.sentScreens
        btns     = screens.last.choices
        rarity   = btns.filter(_.id == "JunkRarity")
      } yield assertTrue(rarity.size == JunkRarityGroups.size) &&
              assertTrue(btns.map(_.id).count(id => id == "JunkPassives" || id == "JunkActives") == 2) &&
              assertTrue(btns.exists(_.id == "BackFromJunk")) &&
              // серая и белая включены, остальные выключены
              assertTrue(rarity.take(2).forall(_.label.endsWith("Вкл"))) &&
              assertTrue(rarity.drop(2).forall(_.label.endsWith("Выкл"))) &&
              // способности по умолчанию включены (прежнее поведение сохранено)
              assertTrue(btns.filter(_.id == "JunkPassives").forall(_.label.endsWith("Вкл")))
    },

    test("нажатие на переключатель редкости сохраняется в merchant_data и переключает обратно") {
      for {
        t <- makeState(richHero)
        (state, heroDao, _, renderer) = t
        _      <- state.action(testUser, tap("JunkSettings"), renderer)
        _      <- state.action(testUser, tapRarity("Green"), renderer)
        after  <- readMerchant(heroDao).map(_.junkSettings)
        _      <- state.action(testUser, tapRarity("Green"), renderer)
        back   <- readMerchant(heroDao).map(_.junkSettings)
      } yield assertTrue(after.rarities.contains(Rarity.Green)) &&
              assertTrue(!back.rarities.contains(Rarity.Green)) &&
              assertTrue(after.rarities.contains(Rarity.Gray)) // остальное не тронуто
    },

    test("фиолетовый переключатель накрывает сразу Purple и Violet (у них общий значок)") {
      for {
        t <- makeState(richHero)
        (state, heroDao, _, renderer) = t
        _ <- state.action(testUser, tapRarity("Purple"), renderer)
        s <- readMerchant(heroDao).map(_.junkSettings)
      } yield assertTrue(s.rarities.contains(Rarity.Purple)) &&
              assertTrue(s.rarities.contains(Rarity.Violet))
    },

    test("SellJunk продаёт по настройке: включённая зелёная уходит, выключенная белая остаётся") {
      for {
        t <- makeState(richHero, items = List(gear(1L, Rarity.Gray), gear(2L, Rarity.White), gear(3L, Rarity.Green)))
        (state, _, invRepo, renderer) = t
        _ <- state.action(testUser, tapRarity("Green"), renderer) // зелёную включили
        _ <- state.action(testUser, tapRarity("White"), renderer) // белую выключили
        _ <- state.action(testUser, tap("SellJunk"), renderer)
      } yield assertTrue(invRepo.snapshot.map(_.id) == List(2L)) // остался только белый
    },

    test("выключенные пассивки защищают предмет, даже если его редкость включена") {
      val plain   = gear(1L, Rarity.Gray)
      val passive = gear(2L, Rarity.Gray, ItemDetails.Passive(pangea.model.item.PassiveKind.Jeweler), ItemType.Ring)
      for {
        t <- makeState(richHero, items = List(plain, passive))
        (state, _, invRepo, renderer) = t
        _ <- state.action(testUser, tap("JunkPassives"), renderer) // выключаем пассивки
        _ <- state.action(testUser, tap("SellJunk"), renderer)
      } yield assertTrue(invRepo.snapshot.map(_.id) == List(2L)) // серый с пассивкой уцелел
    },

    test("выключенные активные способности защищают предмет включённой редкости") {
      val plain  = gear(1L, Rarity.Gray)
      val active = gear(2L, Rarity.Gray, ItemDetails.Armor(Skill.MinorHeal), ItemType.ChestPlate)
      for {
        t <- makeState(richHero, items = List(plain, active))
        (state, _, invRepo, renderer) = t
        _ <- state.action(testUser, tap("JunkActives"), renderer)
        _ <- state.action(testUser, tap("SellJunk"), renderer)
      } yield assertTrue(invRepo.snapshot.map(_.id) == List(2L))
    },

    test("настройка переживает обновление стока (Refresh её не сбрасывает)") {
      for {
        t <- makeState(richHero)
        (state, heroDao, _, renderer) = t
        _      <- state.action(testUser, tapRarity("Orange"), renderer)
        before <- readMerchant(heroDao)
        // переводим часы за кулдаун, чтобы Refresh реально перекатил сток
        _      <- TestClock.adjust(2.hours)
        _      <- state.action(testUser, tap("Refresh"), renderer)
        after  <- readMerchant(heroDao)
      } yield assertTrue(after.junkSettings.rarities.contains(Rarity.Orange)) &&
              assertTrue(after.refreshedAt > before.refreshedAt) // сток действительно обновился
    },

    test("isJunk: по умолчанию трофеи и камни не продаются даже когда их редкость включена") {
      val all = JunkRarityGroups.flatMap(_.rarities).toSet
      val s   = JunkSaleSettings(rarities = all, passives = true, actives = true)
      assertTrue(!s.trophies) && // трофеи выключены по умолчанию
      assertTrue(!MerchantState.isJunk(trophyItem(1L), s)) &&
      assertTrue(!MerchantState.isJunk(GemGenerator.item(GemKind.Skull, 1), s)) &&
      assertTrue(MerchantState.isJunk(gear(3L, Rarity.Orange), s)) // обычное снаряжение — да
    },

    test("трофеи: переключатель по умолчанию выключен, после нажатия трофеи уходят в продажу") {
      for {
        t <- makeState(richHero, items = List(gear(1L, Rarity.Gray), trophyItem(2L)))
        (state, heroDao, invRepo, renderer) = t
        _        <- state.action(testUser, tap("JunkSettings"), renderer)
        screens  <- renderer.sentScreens
        btn       = screens.last.choices.find(_.id == "JunkTrophies")
        _        <- state.action(testUser, tap("JunkTrophies"), renderer)
        settings <- readMerchant(heroDao).map(_.junkSettings)
        _        <- state.action(testUser, tap("SellJunk"), renderer)
      } yield assertTrue(btn.exists(_.label == "Трофеи: Выкл")) && // по умолчанию выключено
              assertTrue(settings.trophies) &&
              assertTrue(invRepo.snapshot.isEmpty) // ушли и серый предмет, и трофей
    },

    test("трофеи не смотрят на редкости: продаются даже с выключенной серой") {
      val s = JunkSaleSettings(rarities = Set.empty, trophies = true)
      for {
        t <- makeState(richHero, items = List(gear(1L, Rarity.Gray), trophyItem(2L)))
        (state, _, invRepo, renderer) = t
        _ <- state.action(testUser, tapRarity("Gray"), renderer)  // выключаем серую
        _ <- state.action(testUser, tap("JunkTrophies"), renderer) // включаем трофеи
        _ <- state.action(testUser, tap("SellJunk"), renderer)
      } yield assertTrue(invRepo.snapshot.map(_.id) == List(1L)) && // серый предмет уцелел
              assertTrue(MerchantState.isJunk(trophyItem(2L), s))   // а трофей — нет
    },

    test("камни не продаются даже при включённых трофеях") {
      val s = JunkSaleSettings(rarities = Set(Rarity.Gray), trophies = true)
      assertTrue(!MerchantState.isJunk(GemGenerator.item(GemKind.Skull, 1), s))
    },

    test("всё снаряжение куплено → сообщение «занят подготовкой новой партии», есть Продать/Назад") {
      for {
        t <- makeState(richHero)
        (state, _, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        _       <- state.action(testUser, tapIdx("ConfirmBuy", 0), renderer)
        _       <- state.action(testUser, tapIdx("ConfirmBuy", 1), renderer)
        _       <- state.action(testUser, tapIdx("ConfirmBuy", 2), renderer)
        screens <- renderer.sentScreens
        ids      = screens.last.choices.map(_.id)
      } yield assertTrue(screens.last.text.contains("занят подготовкой новой партии")) &&
              assertTrue(!ids.contains("Buy")) &&
              assertTrue(ids.contains("Sell") && ids.contains("Back"))
    },

    test("Back → переход в MarketSquare") {
      for {
        t <- makeState(richHero)
        (state, _, _, renderer) = t
        result <- state.action(testUser, tap("Back"), renderer)
      } yield assertTrue(result == StateType.MarketSquare)
    }
  )
}
