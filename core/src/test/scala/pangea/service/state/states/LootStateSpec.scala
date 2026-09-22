package pangea.service.state.states

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.GemGenerator
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

  private def makeState(loot: LootData, canAdd: Boolean = true, bag: List[Item] = Nil) =
    for {
      renderer <- TestRenderer.make
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(silver = 100L))
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

    test("старая запись добычи без полей группы читается как одиночная") {
      val json = io.circe.parser.parse("""{"items":[],"silvers":[3]}""").toOption.get
      val loot = json.as[LootData].toOption
      assertTrue(loot.exists(l => l.monsterName.isEmpty && l.queue.isEmpty && l.silvers == List(3L)))
    }
  )
}
