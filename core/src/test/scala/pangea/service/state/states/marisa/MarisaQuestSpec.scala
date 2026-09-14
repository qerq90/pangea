package pangea.service.state.states.marisa

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Achievement, Hero}
import pangea.model.inventory.Inventory
import pangea.model.item.{Item, ItemType, QuestItemKind, Rarity}
import pangea.model.monster.{Monster, Race, Rarity => MobRarity}
import pangea.model.quest.{NpcQuest, NpcQuestProgress, NpcQuests}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.marisa.MarisaHuntState.{Progress, Step}
import pangea.service.state.states.merchant.MerchantState
import pangea.service.state.states.tavern.InnkeeperState
import pangea.service.state.states.temple.TempleAzatState
import pangea.service.state.states.{DeathState, InventoryState, LootState, RestState}
import pangea.service.state.{MarisaQuest, NpcQuestLog, UserAction}
import pangea.test._
import zio.test.{TestClock, TestRandom}
import zio.test._
import zio.{Duration, Task, ZIO}

/** «Письмо Марисе»: сюжетные предметы, письмо с пятидесятого убитого, поиски
  * адресата, тайник Кельвина и коллектор — со всеми развязками. */
object MarisaQuestSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero: Hero = TestFixtures.hero(userId).copy(lvl = 5L, silver = 1000L, doubloons = 0L)

  private def letter(id: Long = 11L): Item = QuestItemKind.item(QuestItemKind.MarisaLetter).copy(id = id)
  private def map(id: Long = 12L): Item    = QuestItemKind.item(QuestItemKind.KelvinMap).copy(id = id)
  private def gear(id: Long): Item =
    Item(id, s"Предмет $id", 1L, Rarity.Gray, ItemType.Helmet, attack = 0, accuracy = 0, energy = 0, armor = 1, defence = 0, evasion = 0)

  private def content = ZIO.attempt(SceneContent.load())

  private def quests(dao: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(dao, userId)
  private def seed(dao: TestHeroDao, p: NpcQuestProgress): Task[Unit] =
    NpcQuestLog.modify(dao, userId)(_.updated(NpcQuest.Marisa, p)).unit
  private def heroOf(dao: TestHeroDao): Task[Hero] = dao.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  /** Инвентарь, открытый из города (или из лабиринта). */
  private def inventoryFrom(from: StateType, items: List[Item], h: Hero = hero) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      _   <- dao.writeReturnState(userId, Some(from))
      inv  = TestInventoryRepository.withItems(items)
      r   <- TestRenderer.make
      c   <- content
    } yield (InventoryState(dao, inv, TestItemRepository.make, c), dao, inv, r)

  private def weakMob(hp: Long = 10L): Monster =
    Monster(0L, 1L, Race.Orc, MobRarity.Common, FightStats(atk = 1, hp = hp, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))

  private def strong: Hero = hero.copy(fightStats = FightStats(atk = 100000L, hp = 500000L, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))

  override def spec = suite("Письмо Марисе")(

    suite("сюжетные предметы")(
      test("не занимают места в сумке и кладутся даже в полную") {
        val full = Inventory(1L, hero.id, 2L, Inventory.Items(List(gear(1L), gear(2L), letter())))
        val repo = TestInventoryRepository.full
        for {
          _   <- repo.addItem(hero.id, map())
          bad <- repo.addItem(hero.id, gear(3L)).either
        } yield assertTrue(full.occupied == 2L && full.freeSlots == 0L && !full.hasRoomFor(1L)) &&
                assertTrue(repo.snapshot.exists(_.questItem.contains(QuestItemKind.KelvinMap))) &&
                assertTrue(bad.isLeft)
      },
      test("не хлам, не продаются, не выпадают при смерти, не кладутся в бочку") {
        for {
          c   <- content
          dao <- TestHeroDao.withHero(userId, hero.copy(exp = 80L))
          inv  = TestInventoryRepository.withItems(List(letter(), gear(1L)))
          r   <- TestRenderer.make
          _   <- TestRandom.feedInts(1, 0, 0) // травма, потом «потерян» на каждый предмет
          _   <- DeathState(dao, inv, c).enter(testUser, r)
          merchant = MerchantState(dao, inv, TestItemRepository.make, c)
          _   <- TestRandom.feedLongs(1L, 2L, 3L, 4L, 5L, 6L)
          _   <- merchant.action(testUser, tap("Sell"), r)
          sell <- r.sentScreens.map(_.last)
        } yield assertTrue(inv.snapshot.map(_.id) == List(11L)) &&                 // шлем ушёл, письмо осталось
                assertTrue(!MerchantState.isJunk(letter(), MerchantState.JunkSaleSettings())) &&
                assertTrue(!sell.choices.exists(_.id.contains("11")))
      }
    ),

    suite("письмо")(
      test("пятидесятый убитый оставляет письмо: сообщение, предмет, задание на первом шаге; второй раз — нет") {
        val h = strong.copy(kills = 49L)
        for {
          dao <- TestHeroDao.withHero(userId, h)
          _   <- dao.writeActiveBattle(userId, SoloPveBattle.from(weakMob(), h).asJson)
          inv  = TestInventoryRepository.accepting
          r   <- TestRenderer.make
          c   <- content
          state = BattleState(dao, inv, TestItemRepository.make, c)
          _   <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
          res <- state.action(testUser, tap("Attack"), r)
          after <- heroOf(dao)
          q   <- quests(dao)
          all <- texts(r)
          // ещё одна победа — письма больше не будет
          _   <- dao.writeActiveBattle(userId, SoloPveBattle.from(weakMob(), after).asJson)
          _   <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
          _   <- state.action(testUser, tap("Attack"), r)
          later <- heroOf(dao)
        } yield assertTrue(res == StateType.Loot && after.kills == 50L && later.kills == 51L) &&
                assertTrue(all.contains("Мариса Кальдера") && all.contains("Старое письмо Марисе")) &&
                assertTrue(inv.snapshot.count(_.questItem.contains(QuestItemKind.MarisaLetter)) == 1) &&
                assertTrue(q.onStep(NpcQuest.Marisa, 1))
      },
      test("в инвентаре: сложность и подсказка про Трактирщика; вскрыть — текст Кельвина и карта; потом «перечитать»") {
        for {
          t <- inventoryFrom(StateType.GlobalMap, List(letter()))
          (state, dao, inv, r) = t
          _     <- seed(dao, NpcQuestProgress(step = 1))
          _     <- state.action(testUser, tap(s"${InventoryState.ItemActionPrefix}11"), r)
          card  <- r.sentScreens.map(_.last)
          _     <- state.action(testUser, tap("OpenLetter"), r)
          all   <- texts(r)
          q     <- quests(dao)
          _     <- state.action(testUser, tap(s"${InventoryState.ItemActionPrefix}11"), r)
          again <- r.sentScreens.map(_.last)
        } yield assertTrue(card.text.contains("🦴🦴🦴") && card.text.contains("Трактирщик")) &&
                assertTrue(card.choices.map(_.id) == List("OpenLetter", "InventoryList")) &&
                assertTrue(all.contains("Кельвин") && all.contains("Карта Кельвина")) &&
                assertTrue(inv.snapshot.exists(_.questItem.contains(QuestItemKind.KelvinMap))) &&
                assertTrue(q.of(NpcQuest.Marisa).bonus) &&
                assertTrue(again.choices.head.label == "Перечитать письмо")
      }
    ),

    suite("поиски адресата")(
      test("Трактирщик: кнопка при письме на руках, ответ, «продолжать поиски» — в таверну, второй шаг") {
        for {
          dao <- TestHeroDao.withHero(userId, hero)
          _   <- seed(dao, NpcQuestProgress(step = 1))
          r   <- TestRenderer.make
          c   <- content
          state = InnkeeperState(dao, TestInventoryRepository.withItems(List(letter())), c)
          _   <- state.enter(testUser, r)
          menu <- r.sentScreens.map(_.last)
          _   <- state.action(testUser, tap("AskMarisa"), r)
          ans <- r.sentScreens.map(_.last)
          res <- state.action(testUser, tap("ContinueSearch"), r)
          q   <- quests(dao)
          all <- texts(r)
        } yield assertTrue(menu.choices.exists(_.id == "AskMarisa")) &&
                assertTrue(ans.text.contains("благочестивой") && ans.choices.map(_.id) == List("ContinueSearch")) &&
                assertTrue(res == StateType.Tavern && all.contains("удачи тебе")) &&
                assertTrue(q.onStep(NpcQuest.Marisa, 2))
      },
      test("Жрец: кнопка на втором шаге, подсказка про Долорес, дальше — Портовый квартал и Мариса") {
        for {
          dao <- TestHeroDao.withHero(userId, hero)
          _   <- seed(dao, NpcQuestProgress(step = 2))
          r   <- TestRenderer.make
          c   <- content
          inv  = TestInventoryRepository.withItems(List(letter()))
          temple = TempleAzatState(dao, inv, TestItemRepository.make, c)
          _   <- temple.action(testUser, tap("Priest"), r)
          menu <- r.sentScreens.map(_.last)
          _   <- temple.action(testUser, tap("AskMarisa"), r)
          res <- temple.action(testUser, tap("SearchDolores"), r)
          search = MarisaSearchState(dao, inv, TestItemRepository.make, c)
          _   <- search.enter(testUser, r)
          _   <- search.action(testUser, tap("FoundLetter"), r)
          _   <- search.action(testUser, tap("AskMarisa"), r)
          _   <- search.action(testUser, tap("Agree"), r)
          end <- search.action(testUser, tap("Promise"), r)
          q   <- quests(dao)
          all <- texts(r)
        } yield assertTrue(menu.choices.exists(_.id == "AskMarisa")) &&
                assertTrue(res == StateType.MarisaSearch) &&
                assertTrue(all.contains("Долорес") && all.contains("покойного мужа")) &&
                assertTrue(end == StateType.HarborQuarter) &&
                assertTrue(q.onStep(NpcQuest.Marisa, 3) && q.of(NpcQuest.Marisa).bonus) &&
                assertTrue(inv.snapshot.exists(_.questItem.contains(QuestItemKind.KelvinMap))) &&
                assertTrue(all.contains("так было бы честнее"))
      }
    ),

    suite("карта Кельвина")(
      test("из лабиринта не работает; из города уходит за город одному, а если Мариса ждёт — спрашивает") {
        for {
          t <- inventoryFrom(StateType.Dungeon, List(map()))
          (state, _, _, r) = t
          stay <- state.action(testUser, tap(s"${InventoryState.ItemActionPrefix}12"), r) *> state.action(testUser, tap("UseKelvinMap"), r)
          all  <- texts(r)
          t2 <- inventoryFrom(StateType.GlobalMap, List(map()))
          (state2, dao2, _, r2) = t2
          _    <- seed(dao2, NpcQuestProgress(step = 1, bonus = true))
          gone <- state2.action(testUser, tap("UseKelvinMap"), r2)
          p2   <- dao2.readSceneData(userId).map(_.flatMap(_.as[Progress].toOption).get)
          t3 <- inventoryFrom(StateType.Tavern, List(map()))
          (state3, dao3, _, r3) = t3
          _    <- seed(dao3, NpcQuestProgress(step = 3, bonus = true))
          ask  <- state3.action(testUser, tap("UseKelvinMap"), r3)
          q    <- r3.sentScreens.map(_.last)
          went <- state3.action(testUser, tap("MapWithMarisa"), r3)
          p3   <- dao3.readSceneData(userId).map(_.flatMap(_.as[Progress].toOption).get)
        } yield assertTrue(stay == StateType.Inventory && all.contains("только из города")) &&
                assertTrue(gone == StateType.MarisaHunt && !p2.withMarisa && p2.step == Step.Road) &&
                assertTrue(ask == StateType.Inventory && q.choices.map(_.id) == List("MapWithMarisa", "MapAlone")) &&
                assertTrue(went == StateType.MarisaHunt && p3.withMarisa)
      }
    ),

    suite("тайник и коллектор")(
      test("дорога: таймер на 10 минут; по нему — тайник через экран добычи, возврат на коллектора") {
        for {
          dao <- TestHeroDao.withHero(userId, hero)
          _   <- dao.writeSceneData(userId, Progress(Step.Road, withMarisa = true).asJson)
          sched <- TestScheduler.make
          r   <- TestRenderer.make
          c   <- content
          state = MarisaHuntState(dao, TestInventoryRepository.accepting, sched, c)
          _   <- state.enter(testUser, r)
          tasks <- sched.scheduled
          thanks <- texts(r)
          early <- state.action(testUser, UserAction("", None), r)
          _   <- TestClock.adjust(Duration.fromMillis(MarisaHuntState.HuntDurationMs + 1L))
          _   <- TestRandom.feedLongs(7L)
          done <- state.action(testUser, UserAction("", None), r)
          loot <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption).get)
          gems  = loot.items.filter(_.itemType == ItemType.Gem)
          gearItem = loot.items.find(_.itemType != ItemType.Gem).get
        } yield assertTrue(tasks.exists(t => t.kind == TaskKind.MarisaHunt && t.fireAt == MarisaHuntState.HuntDurationMs)) &&
                assertTrue(thanks.contains("не обманули")) &&
                assertTrue(early == StateType.MarisaHunt) &&
                assertTrue(done == StateType.Loot) &&
                assertTrue(loot.silvers == List(17000L) && loot.doubloons == 120L) &&
                assertTrue(gems.map(_.name).toSet == Set("Поврежденный рубин", "Поврежденный изумруд", "Поврежденный топаз", "Поврежденный сапфир", "Поврежденный череп")) &&
                assertTrue(gearItem.rarity == Rarity.Purple && gearItem.lvl == 4L) &&
                assertTrue(loot.returnState.contains(StateType.MarisaHunt)) &&
                assertTrue(loot.eventData.flatMap(_.as[Progress].toOption).exists(p => p.step == Step.Collector && p.withMarisa))
      },
      test("коллектор: заплатить — долг уплачен, письмо и карта уходят, в город; с Марисой — «Спаситель Марисы»") {
        val rich = hero.copy(silver = 20000L, doubloons = 150L)
        for {
          dao <- TestHeroDao.withHero(userId, rich)
          _   <- seed(dao, NpcQuestProgress(step = 3, bonus = true))
          _   <- dao.writeSceneData(userId, Progress(Step.Collector, withMarisa = true).asJson)
          inv  = TestInventoryRepository.withItems(List(letter(), map(), gear(1L)))
          sched <- TestScheduler.make
          r   <- TestRenderer.make
          c   <- content
          state = MarisaHuntState(dao, inv, sched, c)
          _   <- state.enter(testUser, r)
          screen <- r.sentScreens.map(_.last)
          res <- state.action(testUser, tap("PayCollector"), r)
          h   <- heroOf(dao)
          q   <- quests(dao)
          all <- texts(r)
        } yield assertTrue(screen.text.contains("А вот и тайник, Мариса") && screen.text.contains("Отдай ему эти деньги")) &&
                assertTrue(screen.choices.map(_.id) == List("PayCollector", "FightCollector")) &&
                assertTrue(res == StateType.GlobalMap) &&
                assertTrue(h.silver == 20000L - 14644L && h.doubloons == 150L - 105L) &&
                assertTrue(h.hasAchievement(Achievement.MarisaSavior)) &&
                assertTrue(inv.snapshot.map(_.id) == List(1L)) &&
                assertTrue(q.isDone(NpcQuest.Marisa)) &&
                assertTrue(all.contains("долг Кельвина уплачен") && all.contains("Спаситель Марисы") && all.contains("завершён"))
      },
      test("бой: Коллектор — человек-вор второго уровня с +75 HP и +100 брони; победа — 500 серебра и 10 дублонов без опыта") {
        val h = strong.copy(silver = 20000L, doubloons = 150L)
        for {
          dao <- TestHeroDao.withHero(userId, h)
          _   <- dao.writeSceneData(userId, Progress(Step.Collector, withMarisa = false).asJson)
          sched <- TestScheduler.make
          r   <- TestRenderer.make
          c   <- content
          inv  = TestInventoryRepository.withItems(List(letter(), map()))
          state = MarisaHuntState(dao, inv, sched, c)
          _   <- TestRandom.feedLongs(50L)
          res <- state.action(testUser, tap("FightCollector"), r)
          battle <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
          base   = pangea.generator.monster.MonsterGenerator.generateOfRaceAndRarity(2, Race.Human, MobRarity.Uncommon)
          // герой бьёт насмерть
          _   <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
          won <- BattleState(dao, inv, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
          after <- heroOf(dao)
          loot <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption).get)
          all <- texts(r)
          // добыча вернула в поход: развязка без Марисы
          journal <- TestJournal.make
          lootState = LootState(dao, inv, TestItemRepository.make, journal, c)
          _   <- dao.writeSceneData(userId, loot.eventData.get)
          _   <- state.enter(testUser, r)
          q   <- quests(dao)
          end <- texts(r)
        } yield assertTrue(res == StateType.Battle) &&
                assertTrue(battle.monsterName == "Коллектор" && battle.story.contains("collector")) &&
                assertTrue(battle.monsterLvl == 2L && battle.monsterRace == Race.Human.entryName && battle.monsterRarity == MobRarity.Uncommon.entryName) &&
                assertTrue(battle.monsterStats.hp == base.fightStats.hp + 75L && battle.monsterStats.armor == base.fightStats.armor + 100L) &&
                assertTrue(won == StateType.Loot && after.exp == 0L) &&
                assertTrue(loot.items.isEmpty && loot.silvers == List(500L) && loot.doubloons == 10L) &&
                assertTrue(all.contains("Коллектор повержен!") && !all.contains("Получено 0")) &&
                assertTrue(end.contains("никому не повредит") && end.contains("завершён")) &&
                assertTrue(q.isDone(NpcQuest.Marisa) && inv.snapshot.isEmpty) &&
                assertTrue(lootState.targetStates.contains(StateType.MarisaHunt))
      },
      test("после боя с Марисой: отдать долг — «Спаситель»; оставить себе — «Мерзавец»") {
        def run(action: String) =
          for {
            dao <- TestHeroDao.withHero(userId, hero.copy(silver = 20000L, doubloons = 150L))
            _   <- dao.writeSceneData(userId, Progress(Step.AfterFight, withMarisa = true).asJson)
            sched <- TestScheduler.make
            r   <- TestRenderer.make
            c   <- content
            state = MarisaHuntState(dao, TestInventoryRepository.withItems(List(letter(), map())), sched, c)
            _   <- state.enter(testUser, r)
            ask <- r.sentScreens.map(_.last)
            res <- state.action(testUser, tap(action), r)
            h   <- heroOf(dao)
            all <- texts(r)
          } yield (ask, res, h, all)
        for {
          gave <- run("GiveMarisa")
          kept <- run("KeepSilver")
        } yield assertTrue(gave._1.text.contains("Мариса недовольна") && gave._1.choices.map(_.id) == List("GiveMarisa", "KeepSilver")) &&
                assertTrue(gave._2 == StateType.GlobalMap && gave._3.silver == 20000L - 14644L && gave._3.hasAchievement(Achievement.MarisaSavior)) &&
                assertTrue(kept._2 == StateType.GlobalMap && kept._3.silver == 20000L && kept._3.hasAchievement(Achievement.Scoundrel)) &&
                assertTrue(kept._4.contains("стыдно") && kept._4.contains("Мерзавец"))
      },
      test("смерть от коллектора: серебра не меньше 15 000, минус 105 дублонов, задание закрыто, проснуться — в городе") {
        val h = hero.copy(exp = 80L, silver = 20000L, doubloons = 150L)
        val battle = SoloPveBattle.from(weakMob(), h).copy(story = Some(MarisaQuest.CollectorStory))
        for {
          dao <- TestHeroDao.withHero(userId, h)
          _   <- dao.writeActiveBattle(userId, battle.asJson)
          _   <- dao.writeSceneData(userId, LootData(Nil, Nil, returnState = Some(StateType.MarisaHunt),
                   eventData = Some(Progress(Step.AfterFight, withMarisa = true).asJson)).asJson)
          inv  = TestInventoryRepository.withItems(List(letter(), map()))
          r   <- TestRenderer.make
          c   <- content
          _   <- TestRandom.feedInts(1)
          _   <- DeathState(dao, inv, c).enter(testUser, r)
          dead <- heroOf(dao)
          q   <- quests(dao)
          sched <- TestScheduler.make
          rest = RestState(dao, sched, c)
          _   <- rest.enter(testUser, r)
          scene <- dao.readSceneData(userId)
          _   <- TestClock.adjust(Duration.fromMillis(2L * 60L * 60L * 1000L))
          woke <- rest.action(testUser, UserAction("", None), r)
          all <- texts(r)
        } yield assertTrue(dead.silver == 5000L && dead.doubloons == 45L) &&     // 20 000 − max(10 000, 15 000)
                assertTrue(inv.snapshot.isEmpty && q.isDone(NpcQuest.Marisa)) &&
                assertTrue(scene.exists(_.hcursor.get[String]("wakeTo").toOption.contains("GlobalMap"))) &&
                assertTrue(woke == StateType.GlobalMap) &&
                assertTrue(all.contains("след, как и Марисы") && all.contains("завершён"))
      }
    ),

    suite("достижения")(
      test("«Спаситель Марисы»: серебро добычи на 10% больше; «Мерзавец»: удар на 5% сильнее") {
        val savior    = hero.withAchievement(Achievement.MarisaSavior)
        // урон: (сила 10 × 3 + атака 10) × 0.5 без оружия = 20; Мерзавец → 21
        val plain     = hero.copy(fightStats = hero.fightStats.copy(accuracy = 9999))
        val scoundrel = plain.withAchievement(Achievement.Scoundrel)
        def hit(hh: Hero) = for {
          c  <- content
          d  <- TestHeroDao.withHero(userId, hh)
          _  <- d.writeActiveBattle(userId, SoloPveBattle.from(weakMob(1000L), hh).asJson)
          rr <- TestRenderer.make
          _  <- TestRandom.feedInts(60, 1) *> TestRandom.feedLongs(100L, 100L)
          _  <- BattleState(d, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap("Attack"), rr)
          b  <- d.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        } yield 1000L - b.monsterCurrentHp
        for {
          c   <- content
          dao <- TestHeroDao.withHero(userId, savior)
          _   <- dao.writeSceneData(userId, LootData(Nil, List(100L)).asJson)
          r   <- TestRenderer.make
          journal <- TestJournal.make
          _   <- LootState(dao, TestInventoryRepository.accepting, TestItemRepository.make, journal, c).enter(testUser, r)
          h   <- heroOf(dao)
          a   <- hit(plain)
          b   <- hit(scoundrel)
        } yield assertTrue(h.silver == 1000L + 110L) &&
                assertTrue(a == 20L && b == 21L)
      }
    )
  )
}
