package pangea.service.state.states.events

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.hero.{Hero, Knowledge, LoreData}
import pangea.model.item.{Item, MaterialKind, QuestItemKind}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.stats.BaseStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.InventoryState
import pangea.service.state.states.events.FlowerMeadowState.MeadowScene
import pangea.service.state.states.gustavo.{GustavoHerbsState, GustavoState}
import pangea.service.state.states.hero.KnowledgeState
import pangea.service.state.states.merchant.MerchantState
import pangea.service.state.{HerbLore, UserAction}
import pangea.test._
import zio.test.{TestClock, TestRandom}
import zio.test._
import zio.{Duration, Task, ZIO}

/** Поляна цветов: травы по таймеру, знания о них, трактаты Густаво, сдача трав. */
object FlowerMeadowSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  /** Интеллект 40: догадка 10 %, чтение 20 %. */
  private def hero(int: Long = 40L, silver: Long = 0L): Hero =
    TestFixtures.hero(userId).copy(silver = silver, baseStats = BaseStats(agi = 10, vit = 10, str = 10, int = int))

  private def content = ZIO.attempt(SceneContent.load())

  private def meadow(h: Hero, lore: LoreData = LoreData.empty) =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      _     <- dao.writeLoreData(userId, lore.asJson)
      inv    = TestInventoryRepository.accepting
      sched <- TestScheduler.make
      r     <- TestRenderer.make
      c     <- content
    } yield (FlowerMeadowState(dao, inv, TestItemRepository.make, sched, c), dao, inv, sched, r)

  private def loreOf(dao: TestHeroDao): Task[LoreData] = HerbLore.readLore(dao, userId)
  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))
  private def sceneOf(dao: TestHeroDao) = dao.readSceneData(userId).map(_.flatMap(_.as[MeadowScene].toOption))

  private def herb(kind: MaterialKind, id: Long): Item =
    pangea.generator.item.MaterialGenerator.item(kind).copy(id = id)

  override def spec = suite("Поляна цветов")(

    test("в пуле событий: 2% (по одному у боя и у находки), билеты дальше по списку не сдвинулись") {
      val ev = StateType.events
      assertTrue(ev.count(_ == StateType.FlowerMeadow) == 2) &&
      assertTrue(ev.count(_ == StateType.Battle) == 38 && ev.count(_ == StateType.FoundItem) == 19) &&
      assertTrue(ev.size == 100) &&
      assertTrue(ev(67) == StateType.Spring && ev(38) == StateType.FlowerMeadow && ev(99) == StateType.ElementalLair)
    },

    test("вход: 2–6 цветов, таймер на 2–3 минуты, кнопки «Персонаж» и красная «Уйти»; уйти — через подтверждение") {
      for {
        t <- meadow(hero())
        (state, dao, _, sched, r) = t
        _      <- TestRandom.feedInts(2) *> TestRandom.feedLongs(150000L) // 2 цветка, 2,5 минуты
        _      <- state.enter(testUser, r)
        scene  <- sceneOf(dao)
        tasks  <- sched.scheduled
        screen <- r.sentScreens.map(_.last)
        ask    <- state.action(testUser, tap("LeaveMeadow"), r)
        confirm <- r.sentScreens.map(_.last)
        stay   <- state.action(testUser, tap("StayMeadow"), r)
        stayed <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("LeaveMeadow"), r)
        left   <- state.action(testUser, tap("ConfirmLeave"), r)
        after  <- dao.readSceneData(userId)
        cancelled <- sched.cancelled
      } yield assertTrue(scene.exists(s => s.left == 2 && s.nextAt == 150000L)) &&
              assertTrue(tasks.exists(t => t.kind == TaskKind.FlowerMeadow && t.fireAt == 150000L)) &&
              assertTrue(screen.text.contains("поляна шепчет")) &&
              assertTrue(screen.choices.map(_.id) == List("OpenCharacter", "LeaveMeadow")) &&
              assertTrue(screen.choices.last.color == pangea.engine.ChoiceColor.Negative) &&
              assertTrue(ask == StateType.FlowerMeadow && confirm.choices.map(_.id) == List("ConfirmLeave", "StayMeadow")) &&
              assertTrue(stay == StateType.FlowerMeadow && !stayed.text.contains("поляна шепчет")) && // описание не повторяется
              assertTrue(left == StateType.Dungeon && after.contains(io.circe.Json.Null)) &&
              assertTrue(cancelled.contains(userId -> TaskKind.FlowerMeadow))
    },

    test("без знаний любая трава — «Странный цветок»; догадка (интеллект ÷ 4) даёт знания 1 ранга") {
      for {
        t <- meadow(hero(int = 40L), LoreData.empty)
        (state, dao, inv, _, r) = t
        _     <- dao.writeSceneData(userId, MeadowScene(left = 3, nextAt = 0L).asJson)
        // волк не напал (50 > 3), ранг 1 (50), вид 0, догадка не удалась (50 > 10), задержка
        _     <- TestRandom.feedInts(50, 50, 0, 50) *> TestRandom.feedLongs(120000L)
        _     <- state.action(testUser, tap("FlowerFind"), r)
        lore1 <- loreOf(dao)
        // без волка, ранг 1, вид 2, догадка удалась (5 ≤ 10)
        _     <- TestRandom.feedInts(50, 50, 2, 5) *> TestRandom.feedLongs(120000L)
        _     <- state.action(testUser, tap("FlowerFind"), r)
        lore2 <- loreOf(dao)
        // теперь трава узнаётся; догадка больше не бросается
        _     <- TestRandom.feedInts(50, 50, 2) *> TestRandom.feedLongs(120000L)
        res   <- state.action(testUser, tap("FlowerFind"), r)
        all   <- texts(r)
        names  = inv.snapshot.map(_.name)
        h     <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(!lore1.knows(Knowledge.FlowersRank1)) &&
              assertTrue(h.fightStats.energy > hero().fightStats.energy && all.contains("⚡ +")) &&   // каждый цветок возвращает энергию
              assertTrue(!all.contains("поляна шепчет") && all.contains("бродите по поляне")) &&       // после цветка — короткий экран, не описание
              assertTrue(lore2.knows(Knowledge.FlowersRank1) && lore2.learnedAlone(Knowledge.FlowersRank1)) &&
              assertTrue(all.contains("стал лучше разбираться")) &&
              assertTrue(names.take(2) == List("Странный цветок", "Странный цветок")) &&
              assertTrue(names(2) == MaterialKind.herbsOfRank(1)(2).displayName) &&
              assertTrue(res == StateType.Dungeon) &&   // третий был последним
              assertTrue(all.contains("обобрана"))
    },

    test("редкая трава (2%) без знаний 2 ранга — тоже странная, а с ними — по имени") {
      for {
        t <- meadow(hero(), LoreData.empty.learn(Knowledge.FlowersRank1, alone = false))
        (state, dao, inv, _, r) = t
        _   <- dao.writeSceneData(userId, MeadowScene(left = 5, nextAt = 0L).asJson)
        _   <- TestRandom.feedInts(50, 2, 0) *> TestRandom.feedLongs(120000L)   // без волка; ранг 2, вид 0; знаний 1 ранга хватает → без броска догадки
        _   <- state.action(testUser, tap("FlowerFind"), r)
        _   <- dao.writeLoreData(userId, LoreData.empty.learn(Knowledge.FlowersRank1, alone = false).learn(Knowledge.FlowersRank2, alone = false).asJson)
        _   <- TestRandom.feedInts(50, 2, 0) *> TestRandom.feedLongs(120000L)
        _   <- state.action(testUser, tap("FlowerFind"), r)
        names = inv.snapshot.map(_.name)
      } yield assertTrue(names == List("Странный цветок", MaterialKind.herbsOfRank(2)(0).displayName))
    },

    test("возврат из меню персонажа: созревший цветок выдаётся сразу, несозревший — таймер заново") {
      for {
        t <- meadow(hero(), LoreData.empty.learn(Knowledge.FlowersRank1, alone = false))
        (state, dao, inv, sched, r) = t
        _     <- dao.writeSceneData(userId, MeadowScene(left = 2, nextAt = 60000L).asJson)
        _     <- state.enter(testUser, r)             // ещё рано: перепланировали на 60 000
        tasks <- sched.scheduled
        _     <- TestClock.adjust(Duration.fromMillis(61000L))
        _     <- TestRandom.feedInts(50, 50, 1) *> TestRandom.feedLongs(120000L)
        _     <- state.enter(testUser, r)             // пора: цветок сразу
        scene <- sceneOf(dao)
      } yield assertTrue(tasks.exists(_.fireAt == 60000L)) &&
              assertTrue(inv.snapshot.size == 1) &&
              assertTrue(scene.exists(_.left == 1))
    },

    test("Белый волк (3%): цветок сорван, бой начинается сразу, первая встреча — реплика и след в знаниях; добыча вернёт на поляну") {
      for {
        t <- meadow(hero().copy(lvl = 9L, dungeonLevel = 7), LoreData.empty)
        (state, dao, inv, sched, r) = t
        _      <- dao.writeSceneData(userId, MeadowScene(left = 3, nextAt = 0L).asJson)
        _      <- TestRandom.feedInts(3, 50, 1, 5)           // волк напал (3 ≤ 3), ранг 1, вид 1, догадка удалась (5 ≤ 10)
        res    <- state.action(testUser, tap("FlowerFind"), r)
        all    <- texts(r)
        battle <- dao.readActiveBattle(userId).map(_.flatMap(_.as[pangea.model.battle.SoloPveBattle].toOption))
        routing <- dao.readSceneData(userId).map(_.flatMap(_.as[pangea.service.state.states.LootState.LootData].toOption))
        lore   <- loreOf(dao)
        cancelled <- sched.cancelled
        // вторая встреча — без реплики о первой
        _      <- dao.writeSceneData(userId, MeadowScene(left = 1, nextAt = 0L).asJson)
        r2     <- TestRenderer.make
        _      <- TestRandom.feedInts(1, 50, 1)
        _      <- state.action(testUser, tap("FlowerFind"), r2)
        again  <- texts(r2)
        last   <- dao.readSceneData(userId).map(_.flatMap(_.as[pangea.service.state.states.LootState.LootData].toOption))
      } yield assertTrue(res == StateType.Battle) &&
              assertTrue(inv.snapshot.size == 2) &&                                   // цветок всё же сорван
              assertTrue(all.contains("услышал сзади шум") && all.contains("лабиринт его угодья")) &&
              assertTrue(battle.exists(b => b.boss.contains(pangea.model.monster.MiniBoss.WhiteWolf) && b.monsterLvl == 2L)) && // (9−1)/4 = 2
              assertTrue(routing.exists(_.returnState.contains(StateType.FlowerMeadow))) &&
              assertTrue(routing.flatMap(_.eventData).flatMap(_.as[MeadowScene].toOption).contains(MeadowScene(left = 2, nextAt = 0L))) &&
              assertTrue(lore.metWolf && lore.knows(Knowledge.FlowersRank1)) &&                 // догадка того же тика не потеряна
              assertTrue(cancelled.contains(userId -> TaskKind.FlowerMeadow)) &&
              assertTrue(again.contains("услышал сзади шум") && !again.contains("лабиринт его угодья")) &&
              assertTrue(last.exists(_.returnState.isEmpty))                          // цветов не осталось — после добычи в лабиринт
    },

    test("возврат на поляну после волка: таймер ставится заново, цветок сразу не выдаётся") {
      for {
        t <- meadow(hero())
        (state, dao, inv, sched, r) = t
        _     <- dao.writeSceneData(userId, MeadowScene(left = 2, nextAt = 0L).asJson)
        _     <- TestRandom.feedLongs(150000L)
        _     <- state.enter(testUser, r)
        scene <- sceneOf(dao)
        tasks <- sched.scheduled
        screen <- r.sentScreens.map(_.last)
      } yield assertTrue(inv.snapshot.isEmpty) &&
              assertTrue(scene.exists(s => s.left == 2 && s.nextAt == 150000L)) &&
              assertTrue(tasks.exists(_.fireAt == 150000L)) &&
              assertTrue(screen.text.contains("бродите по поляне"))
    },

    test("Густаво: травы сдаются все разом по цене, странные — по цене сена; Ришелье их не показывает") {
      val items = List(herb(MaterialKind.Nettle, 1L), herb(MaterialKind.Nettle, 2L), herb(MaterialKind.StrangeFlower, 3L), herb(MaterialKind.BubbleLily, 4L))
      for {
        c   <- content
        dao <- TestHeroDao.withHero(userId, hero(silver = 100L))
        inv  = TestInventoryRepository.withItems(items)
        r   <- TestRenderer.make
        gus  = GustavoState(dao, inv, c)
        _   <- gus.action(testUser, tap("Herbs"), r)
        list <- r.sentScreens.map(_.last)
        _   <- gus.action(testUser, tap("HerbsSell"), r)
        h   <- dao.getHeroByUserId(userId).map(_.get)
        _   <- TestRandom.feedLongs(1L, 2L, 3L, 4L, 5L, 6L)
        inv2 = TestInventoryRepository.withItems(items)
        _   <- MerchantState(dao, inv2, TestItemRepository.make, c).action(testUser, tap("Sell"), r)
        sell <- r.sentScreens.map(_.last)
      } yield assertTrue(list.text.contains("Крапива ×2 — 🪙 60") && list.text.contains("Странный цветок ×1 — 🪙 5")) &&
              assertTrue(list.choices.head.label == "Сдать всё за 165 🪙") &&
              assertTrue(h.silver == 100L + 165L && inv.snapshot.isEmpty) &&
              assertTrue(!sell.choices.exists(_.id.startsWith(MerchantState.SellItemPrefix)))
    },

    test("«Расскажи о травах»: первый урок за 15 000 → трактат в сумке; с книгой — «дочитай»; самоучке — сразу вторая часть") {
      for {
        c   <- content
        dao <- TestHeroDao.withHero(userId, hero(silver = 20000L))
        inv  = TestInventoryRepository.accepting
        r   <- TestRenderer.make
        st   = GustavoHerbsState(dao, inv, TestItemRepository.make, c)
        _   <- st.enter(testUser, r)
        offer <- r.sentScreens.map(_.last)
        _   <- st.action(testUser, tap("BuyTreatise1"), r)
        h   <- dao.getHeroByUserId(userId).map(_.get)
        again <- r.sentScreens.map(_.last)
        dao2 <- TestHeroDao.withHero(userId, hero(silver = 100L))
        _   <- dao2.writeLoreData(userId, LoreData.empty.learn(Knowledge.FlowersRank1, alone = true).asJson)
        r2  <- TestRenderer.make
        _   <- GustavoHerbsState(dao2, TestInventoryRepository.accepting, TestItemRepository.make, c).enter(testUser, r2)
        self <- r2.sentScreens.map(_.last)
      } yield assertTrue(offer.text.contains("Пятнадцать тысяч") && offer.choices.map(_.id) == List("BuyTreatise1", "Back")) &&
              assertTrue(h.silver == 5000L) &&
              assertTrue(inv.snapshot.exists(_.questItem.contains(QuestItemKind.FlowerTreatise1))) &&
              assertTrue(again.text.contains("дочитай")) &&
              assertTrue(self.text.contains("Сам разобрался") && self.choices.map(_.id) == List("BuyTreatise2", "Back"))
    },

    test("чтение трактата: бросок интеллект ÷ 2 (+2% за каждую неудачу); неудача — час; удача — знание по книге, книга уходит") {
      val book = QuestItemKind.item(QuestItemKind.FlowerTreatise1).copy(id = 9L)
      for {
        c   <- content
        dao <- TestHeroDao.withHero(userId, hero(int = 40L))
        _   <- dao.writeReturnState(userId, Some(StateType.GlobalMap))
        inv  = TestInventoryRepository.withItems(List(book))
        r   <- TestRenderer.make
        st   = InventoryState(dao, inv, TestItemRepository.make, c)
        _   <- st.action(testUser, tap(s"${InventoryState.ItemActionPrefix}9"), r)
        card <- r.sentScreens.map(_.last)
        _   <- TestRandom.feedInts(50)                       // 50 > 20 — не осилил
        _   <- st.action(testUser, tap("ReadTreatise"), r)
        lore1 <- loreOf(dao)
        h1  <- dao.getHeroByUserId(userId).map(_.get)
        _   <- st.action(testUser, tap(s"${InventoryState.ItemActionPrefix}9"), r)
        _   <- st.action(testUser, tap("ReadTreatise"), r)   // ещё рано
        early <- texts(r)
        _   <- TestClock.adjust(Duration.fromMillis(HerbLore.ReadingCooldownMs + 1000L))
        _   <- st.action(testUser, tap(s"${InventoryState.ItemActionPrefix}9"), r)
        _   <- TestRandom.feedInts(22)                       // 22 ≤ 20 + 2 за неудачу — осилил
        _   <- st.action(testUser, tap("ReadTreatise"), r)
        lore2 <- loreOf(dao)
        all <- texts(r)
      } yield assertTrue(card.choices.map(_.id) == List("ReadTreatise", "InventoryList")) &&
              assertTrue(!lore1.knows(Knowledge.FlowersRank1) && lore1.bookCooldowns.contains(QuestItemKind.FlowerTreatise1.entryName)) &&
              assertTrue(lore1.failuresOf(QuestItemKind.FlowerTreatise1.entryName) == 1) &&
              assertTrue(HerbLore.readingChance(h1, 0L, 0) == 20L && HerbLore.readingChance(h1, 0L, 1) == 22L) &&
              assertTrue(lore2.bookFailures.isEmpty) &&
              assertTrue(early.contains("Через ")) &&
              assertTrue(lore2.knows(Knowledge.FlowersRank1) && !lore2.learnedAlone(Knowledge.FlowersRank1)) &&
              assertTrue(inv.snapshot.isEmpty) &&
              assertTrue(all.contains("потратили время") && all.contains("Знания о цветах 1 ранга"))
    },

    test("знание, добытое догадкой, делает купленный трактат лишним: книга уходит из сумки, Густаво предлагает вторую часть") {
      val book = QuestItemKind.item(QuestItemKind.FlowerTreatise1).copy(id = 9L)
      for {
        c     <- content
        dao   <- TestHeroDao.withHero(userId, hero(int = 40L))
        _     <- dao.writeLoreData(userId, LoreData.empty.bookFailed(QuestItemKind.FlowerTreatise1.entryName, 999L).asJson)
        inv    = TestInventoryRepository.withItems(List(book))
        sched <- TestScheduler.make
        r     <- TestRenderer.make
        state  = FlowerMeadowState(dao, inv, TestItemRepository.make, sched, c)
        _     <- dao.writeSceneData(userId, MeadowScene(left = 3, nextAt = 0L).asJson)
        _     <- TestRandom.feedInts(50, 50, 2, 5) *> TestRandom.feedLongs(120000L) // без волка, ранг 1, вид 2, догадка удалась
        _     <- state.action(testUser, tap("FlowerFind"), r)
        all   <- texts(r)
        lore  <- loreOf(dao)
        r2    <- TestRenderer.make
        _     <- GustavoHerbsState(dao, inv, TestItemRepository.make, c).enter(testUser, r2)
        gus   <- r2.sentScreens.map(_.last)
      } yield assertTrue(lore.knows(Knowledge.FlowersRank1) && lore.learnedAlone(Knowledge.FlowersRank1)) &&
              assertTrue(!inv.snapshot.exists(_.isQuestItem)) &&                          // трактат ушёл — тихо
              assertTrue(!all.contains("больше не нужен")) &&
              assertTrue(lore.bookFailures.isEmpty && lore.bookCooldowns.isEmpty) &&       // следы чтения стёрты
              assertTrue(gus.text.contains("Сам разобрался") && gus.choices.map(_.id) == List("BuyTreatise2", "Back"))
    },

    test("Густаво: трактат в сумке о том, что герой уже знает, выбрасывается на месте, а не считается недочитанным") {
      val book = QuestItemKind.item(QuestItemKind.FlowerTreatise1).copy(id = 9L)
      for {
        c   <- content
        dao <- TestHeroDao.withHero(userId, hero())
        _   <- dao.writeLoreData(userId, LoreData.empty.learn(Knowledge.FlowersRank1, alone = true).asJson)
        inv  = TestInventoryRepository.withItems(List(book))
        r   <- TestRenderer.make
        _   <- GustavoHerbsState(dao, inv, TestItemRepository.make, c).enter(testUser, r)
        all <- texts(r)
        gus <- r.sentScreens.map(_.last)
      } yield assertTrue(inv.snapshot.isEmpty) &&
              assertTrue(!all.contains("больше не нужен")) &&
              assertTrue(!gus.text.contains("дочитай") && gus.choices.map(_.id) == List("BuyTreatise2", "Back"))
    },

    test("«Знания» в меню персонажа: пусто — так и сказано; с знанием — название и как получено") {
      for {
        c   <- content
        dao <- TestHeroDao.withHero(userId, hero())
        r   <- TestRenderer.make
        _   <- KnowledgeState(dao, c).enter(testUser, r)
        empty <- r.sentScreens.map(_.last)
        _   <- dao.writeLoreData(userId, LoreData.empty.learn(Knowledge.FlowersRank1, alone = true).asJson)
        _   <- KnowledgeState(dao, c).enter(testUser, r)
        one <- r.sentScreens.map(_.last)
      } yield assertTrue(empty.text.contains("только опыт") && empty.choices.map(_.id) == List("BackFromKnowledge")) &&
              assertTrue(one.text.contains("Знания о цветах 1 ранга") && one.text.contains("дошёл сам"))
    }
  )
}
