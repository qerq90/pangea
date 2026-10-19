package pangea.service.state.states

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.GemGenerator
import pangea.model.hero.Equipment
import pangea.model.item.{GemKind, Item, ItemDetails, ItemType, Rarity, TrophyKind}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.LootState.{LootData, MonsterLoot}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestJournal, TestRenderer}
import zio.ZIO
import zio.test._

object LootStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def gear(name: String) =
    Item(-1L, name, 5L, Rarity.Green, ItemType.Helmet,
         attack = 0, accuracy = 0, energy = 0, armor = 7, defence = 1, evasion = 0)

  private def trophy =
    Item(-1L, "Голова (Человек)", 5L, Rarity.Gray, ItemType.Trophy,
         attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
         details = ItemDetails.Trophy("Human", TrophyKind.Head))

  // Нажатие с данными — как кнопки «Надеть: …» и выбора слота.
  private def tapWith(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((Map("action" -> key) ++ data).asJson.noSpaces))

  private def makeState(loot: LootData, canAdd: Boolean = true, bag: List[Item] = Nil,
                        eq: Equipment => Equipment = identity) =
    for {
      renderer <- TestRenderer.make
      base      = TestFixtures.hero(userId).copy(silver = 100L)
      heroDao  <- TestHeroDao.withHero(userId, base.copy(equipment = eq(base.equipment)))
      _        <- heroDao.writeSceneData(userId, loot.asJson)
      journal  <- TestJournal.make
      content  <- ZIO.attempt(SceneContent.load())
      invRepo   = if (canAdd) TestInventoryRepository.withItems(bag) else TestInventoryRepository.full
      itemRepo  = TestItemRepository.make
      state     = LootState(heroDao, invRepo, itemRepo, journal, content)
    } yield (state, renderer, heroDao, invRepo)

  override def spec = suite("LootState")(

    test("enter с серебром и предметами → серебро начисляется сразу, превью с inline Забрать/Оставить") {
      for {
        t <- makeState(LootData(items = List(gear("Шлем"), trophy), silvers = List(30L, 12L)))
        (state, renderer, heroDao, invRepo) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
        hero    <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(screens.exists(_.text.contains("Осматривая добычу"))) &&
              assertTrue(screens.exists(s => s.choices.map(_.id).toSet == Set("Take", "Leave"))) &&
              assertTrue(invRepo.snapshot.isEmpty) &&        // предметы ждут решения, не положены
              assertTrue(hero.exists(_.silver == 100L + 42L))  // серебро забрано сразу
    },

    test("enter с камнем в добыче → показывает только название, без описания граней") {
      val gem = GemGenerator.item(GemKind.Skull, 3)
      for {
        t <- makeState(LootData(items = List(gem), silvers = Nil))
        (state, renderer, _, _) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.exists(_.text.contains(gem.name))) &&
              assertTrue(!screens.exists(_.text.contains("В гнезде")))
    },

    test("enter с ингредиентом минибосса и пылью → только названия, без описаний") {
      val iron = pangea.generator.item.MaterialGenerator.item(pangea.model.item.MaterialKind.EverburningIron)
      val dust = pangea.generator.item.MaterialGenerator.item(pangea.model.item.MaterialKind.RubyDust)
      for {
        t <- makeState(LootData(items = List(iron, dust), silvers = Nil))
        (state, renderer, _, _) = t
        _       <- state.enter(testUser, renderer)
        text    <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(text.contains(iron.name) && text.contains(dust.name)) &&
              assertTrue(!text.contains(pangea.model.item.MaterialKind.EverburningIron.description)) &&
              assertTrue(!text.contains(pangea.model.item.MaterialKind.RubyDust.description))
    },

    test("enter только с серебром → серебро начислено, кнопка Continue, без Take/Leave") {
      for {
        t <- makeState(LootData(items = Nil, silvers = List(50L)))
        (state, renderer, heroDao, _) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
        hero    <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(hero.exists(_.silver == 100L + 50L)) &&
              assertTrue(screens.exists(_.choices.map(_.id).contains("Continue"))) &&
              assertTrue(!screens.exists(_.choices.map(_.id).contains("Take")))
    },

    test("Take → предметы кладутся в инвентарь с реальным id, серебро не трогает, уход в Dungeon") {
      for {
        t <- makeState(LootData(items = List(gear("Шлем"), trophy), silvers = List(30L, 12L)))
        (state, renderer, heroDao, invRepo) = t
        result  <- state.action(testUser, tap("Take"), renderer)
        screens <- renderer.sentScreens
        hero    <- heroDao.getHeroByUserId(userId)
        items    = invRepo.snapshot
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(items.size == 2) &&
              assertTrue(items.forall(_.id > 0L)) &&
              assertTrue(hero.exists(_.silver == 100L)) &&     // Take серебро не меняет (забрано в enter)
              assertTrue(screens.exists(_.text.contains("Вы забираете добычу")))
    },

    test("Take с переполненным инвентарём → предмет теряется, сообщение про переполненную сумку") {
      for {
        t <- makeState(LootData(items = List(gear("Шлем")), silvers = Nil), canAdd = false)
        (state, renderer, _, invRepo) = t
        _       <- state.action(testUser, tap("Take"), renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(invRepo.snapshot.isEmpty) &&
              assertTrue(screens.exists(_.text.contains("Сумка странника переполнена")))
    },

    test("пыль ложится и в полную сумку, а сверх сотни горстей одного вида осыпается со своей строкой") {
      import pangea.generator.item.MaterialGenerator
      import pangea.model.item.{GemKind, MaterialKind}
      val ruby = MaterialKind.dustOf(GemKind.Ruby)
      val full = (1L to 100L).toList.map(i => MaterialGenerator.item(ruby).copy(id = i))
      for {
        // сумка забита экипировкой — пыль всё равно ложится
        tight <- makeState(LootData(items = List(MaterialGenerator.item(ruby)), silvers = Nil),
                           bag = (1L to 20L).toList.map(i => gear(s"Шлем $i").copy(id = i)))
        (ts, tr, _, tinv) = tight
        _     <- ts.action(testUser, tap("Take"), tr)
        tight2 <- ZIO.succeed(tinv.snapshot.count(_.isDust))
        // сотня рубиновой уже есть — сто первая осыпается
        over  <- makeState(LootData(items = List(MaterialGenerator.item(ruby)), silvers = Nil), bag = full)
        (os, or, _, oinv) = over
        _     <- os.action(testUser, tap("Take"), or)
        texts <- or.sentScreens.map(_.map(_.text).mkString(" | "))
      } yield assertTrue(tight2 == 1) &&                                   // в полную сумку пыль легла
              assertTrue(oinv.snapshot.count(_.isDust) == 100) &&          // сверх сотни не приняли
              assertTrue(texts.contains("у вас уже 100 горстей")) &&
              assertTrue(!texts.contains("Сумка странника переполнена"))
    },

    test("Leave → предметы не кладутся, серебро не трогается, сообщение «оставляете», уход в Dungeon") {
      for {
        t <- makeState(LootData(items = List(gear("Шлем")), silvers = List(50L)))
        (state, renderer, heroDao, invRepo) = t
        result  <- state.action(testUser, tap("Leave"), renderer)
        screens <- renderer.sentScreens
        hero    <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(invRepo.snapshot.isEmpty) &&
              assertTrue(hero.exists(_.silver == 100L)) &&
              assertTrue(screens.exists(_.text.contains("оставляете")))
    },

    test("пустой лут → enter показывает «ничего ценного» с кнопкой Continue") {
      for {
        t <- makeState(LootData(Nil, Nil))
        (state, renderer, heroDao, _) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
        hero    <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(screens.exists(_.text.contains("ничего ценного"))) &&
              assertTrue(screens.exists(_.choices.map(_.id).contains("Continue"))) &&
              assertTrue(hero.exists(_.silver == 100L))
    },

    test("Continue → переход в Dungeon") {
      for {
        t <- makeState(LootData(Nil, Nil))
        (state, renderer, _, _) = t
        result <- state.action(testUser, tap("Continue"), renderer)
      } yield assertTrue(result == StateType.Dungeon)
    },

    test("неизвестный ввод → уходит в Dungeon") {
      for {
        t <- makeState(LootData(Nil, Nil))
        (state, renderer, _, _) = t
        result <- state.action(testUser, UserAction("что угодно", None), renderer)
      } yield assertTrue(result == StateType.Dungeon)
    },

    test("группа: над добычей имя павшего, «Забрать» ведёт к следующему; павший без вещей проскакивает сам; последний — наружу") {
      val loot = LootData(
        items = List(gear("Шлем орка")), silvers = List(10L),
        monsterName = Some("Орк-первый"),
        queue = List(MonsterLoot("Орк-второй", Nil, List(5L), 0L), MonsterLoot("Орк-третий", Nil, Nil, 0L)))
      for {
        t <- makeState(loot)
        (state, renderer, heroDao, invRepo) = t
        _        <- state.enter(testUser, renderer)
        first    <- renderer.sentScreens.map(_.last)
        afterTake <- state.action(testUser, tap("Take"), renderer)
        screens  <- renderer.sentScreens
        second    = screens(screens.size - 2)
        third    <- renderer.sentScreens.map(_.last)
        stored   <- heroDao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption).get)
        afterLast <- state.action(testUser, tap("Continue"), renderer)
        hero     <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(first.text.contains("Орк-первый")) &&
              assertTrue(afterTake == StateType.Loot) &&           // в очереди ещё двое — остаёмся
              assertTrue(invRepo.snapshot.size == 1) &&
              // у второго только серебро: сообщение без кнопок и сразу третий
              assertTrue(second.text.contains("Орк-второй") && second.choices.isEmpty) &&
              assertTrue(third.text.contains("Орк-третий") && third.text.contains("Ничего ценного")) &&
              assertTrue(third.choices.map(_.label) == List("Продолжить")) && // последний — наружу
              assertTrue(stored.monsterName.contains("Орк-третий") && stored.queue.isEmpty) &&
              assertTrue(hero.exists(_.silver == 100L + 10L + 5L)) &&  // серебро со всех
              assertTrue(afterLast == StateType.Dungeon)
    },

    test("группа: «Оставить» тоже ведёт к следующему павшему") {
      val loot = LootData(
        items = List(gear("Шлем")), silvers = Nil,
        monsterName = Some("Орк-первый"),
        queue = List(MonsterLoot("Орк-второй", List(gear("Сапоги")), Nil, 0L)))
      for {
        t <- makeState(loot)
        (state, renderer, _, invRepo) = t
        _      <- state.enter(testUser, renderer)
        result <- state.action(testUser, tap("Leave"), renderer)
        last   <- renderer.sentScreens.map(_.last)
      } yield assertTrue(result == StateType.Loot) &&
              assertTrue(invRepo.snapshot.isEmpty) &&
              assertTrue(last.text.contains("Орк-второй")) &&
              assertTrue(last.choices.map(_.id).toSet == Set("Take", "Leave"))
    },

    test("вещь встаёт в пустой слот → «Забрать и надеть» второй кнопкой, под «Забрать»; не по уровню — кнопки нет") {
      for {
        fit     <- makeState(LootData(items = List(gear("Шлем").copy(lvl = 1L), trophy), silvers = Nil))
        (fs, fr, _, _) = fit
        _       <- fs.enter(testUser, fr)
        fitIds  <- fr.sentScreens.map(_.last.choices.map(_.id))
        // шлем 5-го уровня, а герой 1-го — надеть нельзя
        high    <- makeState(LootData(items = List(gear("Шлем")), silvers = Nil))
        (hs, hr, _, _) = high
        _       <- hs.enter(testUser, hr)
        highIds <- hr.sentScreens.map(_.last.choices.map(_.id))
      } yield assertTrue(fitIds == List("Take", "TakeWear", "Leave")) &&
              assertTrue(highIds == List("Take", "Leave"))
    },

    test("«Забрать и надеть» → шлем на герое, мимо сумки; трофей в сумке; строка «Надето»") {
      for {
        t <- makeState(LootData(items = List(gear("Шлем").copy(lvl = 1L), trophy), silvers = Nil))
        (state, renderer, heroDao, invRepo) = t
        result <- state.action(testUser, tap("TakeWear"), renderer)
        hero   <- heroDao.getHeroByUserId(userId)
        text   <- renderer.sentScreens.map(_.map(_.text).mkString(" | "))
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(hero.exists(h => h.equipment.helmet.name == "Шлем" && h.equipment.helmet.id > 0L)) &&
              assertTrue(hero.exists(_.fightStats.armor == 7L)) && // броня шлема — в статах
              assertTrue(invRepo.snapshot.map(_.itemType) == List(ItemType.Trophy)) &&
              assertTrue(text.contains("Вы забираете добычу") && text.contains("✅ Надето: Шлем"))
    },

    test("«Забрать и надеть» при полной сумке → шлем всё равно надет, трофей теряется как раньше") {
      for {
        t <- makeState(LootData(items = List(gear("Шлем").copy(lvl = 1L), trophy), silvers = Nil), canAdd = false)
        (state, renderer, heroDao, invRepo) = t
        _    <- state.action(testUser, tap("TakeWear"), renderer)
        hero <- heroDao.getHeroByUserId(userId)
        text <- renderer.sentScreens.map(_.map(_.text).mkString(" | "))
      } yield assertTrue(hero.exists(_.equipment.helmet.name == "Шлем")) &&
              assertTrue(invRepo.snapshot.isEmpty) &&
              assertTrue(text.contains("Сумка странника переполнена"))
    },

    test("«Забрать и надеть» с двумя шлемами → надет более редкий, второй в сумке") {
      val gray  = gear("Серый шлем").copy(lvl = 1L, rarity = Rarity.Gray)
      val green = gear("Зелёный шлем").copy(lvl = 1L)
      for {
        t <- makeState(LootData(items = List(gray, green), silvers = Nil))
        (state, renderer, heroDao, invRepo) = t
        _    <- state.action(testUser, tap("TakeWear"), renderer)
        hero <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(hero.exists(_.equipment.helmet.name == "Зелёный шлем")) &&
              assertTrue(invRepo.snapshot.map(_.name) == List("Серый шлем"))
    },

    test("«Забрать и надеть» со старой кнопки, когда надеть уже нечего → всё в сумку, без строки «Надето»") {
      for {
        t <- makeState(LootData(items = List(gear("Шлем")), silvers = Nil))
        (state, renderer, heroDao, invRepo) = t
        _    <- state.action(testUser, tap("TakeWear"), renderer)
        hero <- heroDao.getHeroByUserId(userId)
        text <- renderer.sentScreens.map(_.map(_.text).mkString(" | "))
      } yield assertTrue(invRepo.snapshot.map(_.name) == List("Шлем")) &&
              assertTrue(hero.exists(_.equipment.helmet.itemType == ItemType.NoItem)) &&
              assertTrue(!text.contains("Надето"))
    },

    test("вещь встанет вместо надетой → кнопка «Надеть: …» с номером и именем; «Забрать и надеть» нет") {
      val old = gear("Старый шлем").copy(id = 9L, lvl = 1L)
      for {
        t <- makeState(LootData(items = List(gear("Шлем").copy(lvl = 1L)), silvers = Nil), eq = _.copy(helmet = old))
        (state, renderer, _, _) = t
        _       <- state.enter(testUser, renderer)
        choices <- renderer.sentScreens.map(_.last.choices)
      } yield assertTrue(choices.map(_.id) == List("Take", "TakeSwap", "Leave")) &&
              assertTrue(choices(1).label == "Надеть: Шлем") &&
              assertTrue(choices(1).data == Map("i" -> "0", "n" -> "Шлем"))
    },

    test("«Надеть: …» → новый шлем на герое, старый и трофей в сумке, строки «Надето» и «Снято в сумку»") {
      val old = gear("Старый шлем").copy(id = 9L, lvl = 1L, armor = 2L)
      for {
        t <- makeState(LootData(items = List(gear("Шлем").copy(lvl = 1L), trophy), silvers = Nil), eq = _.copy(helmet = old))
        (state, renderer, heroDao, invRepo) = t
        result <- state.action(testUser, tapWith("TakeSwap", "i" -> "0", "n" -> "Шлем"), renderer)
        hero   <- heroDao.getHeroByUserId(userId)
        text   <- renderer.sentScreens.map(_.map(_.text).mkString(" | "))
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(hero.exists(_.equipment.helmet.name == "Шлем")) &&
              assertTrue(invRepo.snapshot.map(_.name).toSet == Set("Старый шлем", "Голова (Человек)")) &&
              assertTrue(text.contains("✅ Надето: Шлем") && text.contains("🎒 Снято в сумку: Старый шлем"))
    },

    test("«Надеть: …» при полной сумке → отказ, ничего не взято, добыча снова на экране, серебро не задвоилось") {
      val old  = gear("Старый шлем").copy(id = 9L, lvl = 1L)
      val full = (1L to 20L).toList.map(i => gear(s"Сапог $i").copy(id = 100L + i, itemType = ItemType.Boots))
      for {
        t <- makeState(LootData(items = List(gear("Шлем").copy(lvl = 1L)), silvers = List(30L)), bag = full,
                       eq = _.copy(helmet = old))
        (state, renderer, heroDao, invRepo) = t
        _      <- state.enter(testUser, renderer)
        result <- state.action(testUser, tapWith("TakeSwap", "i" -> "0", "n" -> "Шлем"), renderer)
        hero   <- heroDao.getHeroByUserId(userId)
        last   <- renderer.sentScreens.map(_.last)
        text   <- renderer.sentScreens.map(_.map(_.text).mkString(" | "))
      } yield assertTrue(result == StateType.Loot) &&
              assertTrue(text.contains("Сумка полна — снятой вещи некуда деться")) &&
              assertTrue(hero.exists(h => h.equipment.helmet.name == "Старый шлем" && h.silver == 130L)) &&
              assertTrue(invRepo.snapshot.size == 20) &&
              assertTrue(last.choices.map(_.id).contains("TakeSwap"))
    },

    test("два кольца надеты → «Надеть: кольцо» спрашивает, какое снять; выбор второго — меняется второе") {
      def ring(id: Long, name: String) = gear(name).copy(id = id, lvl = 1L, itemType = ItemType.Ring)
      val loot = LootData(items = List(ring(-1L, "Новое кольцо")), silvers = Nil)
      for {
        t <- makeState(loot, eq = _.copy(firstRing = ring(7L, "Кольцо раз"), secondRing = ring(8L, "Кольцо два")))
        (state, renderer, heroDao, invRepo) = t
        asked   <- state.action(testUser, tapWith("TakeSwap", "i" -> "0", "n" -> "Новое кольцо"), renderer)
        choice  <- renderer.sentScreens.map(_.last)
        result  <- state.action(testUser, tapWith("TakeSwap", "i" -> "0", "n" -> "Новое кольцо", "slot" -> "2"), renderer)
        hero    <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(asked == StateType.Loot) &&
              assertTrue(choice.choices.map(_.id) == List("TakeSwap", "TakeSwap", "SwapBack")) &&
              assertTrue(choice.choices.flatMap(_.data.get("slot")) == List("1", "2")) &&
              assertTrue(result == StateType.Dungeon) &&
              assertTrue(hero.exists(h => h.equipment.firstRing.id == 7L && h.equipment.secondRing.name == "Новое кольцо")) &&
              assertTrue(invRepo.snapshot.map(_.id) == List(8L))
    },

    test("«Надеть: …» со старой кнопки (имя не то) и «Назад» → только превью, без начисления серебра") {
      val old = gear("Старый шлем").copy(id = 9L, lvl = 1L)
      for {
        t <- makeState(LootData(items = List(gear("Шлем").copy(lvl = 1L)), silvers = List(30L)), eq = _.copy(helmet = old))
        (state, renderer, heroDao, invRepo) = t
        _     <- state.enter(testUser, renderer)
        stale <- state.action(testUser, tapWith("TakeSwap", "i" -> "0", "n" -> "Чужой шлем"), renderer)
        back  <- state.action(testUser, tap("SwapBack"), renderer)
        hero  <- heroDao.getHeroByUserId(userId)
        last  <- renderer.sentScreens.map(_.last)
      } yield assertTrue(stale == StateType.Loot && back == StateType.Loot) &&
              assertTrue(hero.exists(h => h.equipment.helmet.name == "Старый шлем" && h.silver == 130L)) &&
              assertTrue(invRepo.snapshot.isEmpty) &&
              assertTrue(last.choices.map(_.id) == List("Take", "TakeSwap", "Leave"))
    },

    test("старая запись добычи без полей группы читается как одиночная") {
      val json = io.circe.parser.parse("""{"items":[],"silvers":[3]}""").toOption.get
      val loot = json.as[LootData].toOption
      assertTrue(loot.exists(l => l.monsterName.isEmpty && l.queue.isEmpty && l.silvers == List(3L)))
    }
  )
}
