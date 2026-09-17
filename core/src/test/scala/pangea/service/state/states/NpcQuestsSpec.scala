package pangea.service.state.states

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.GemGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{FlaskEffect, GemKind, Item, ItemDetails, ItemType, Rarity, TrophyKind}
import pangea.model.monster.{Race, Rarity => MobRarity}
import pangea.model.quest.{NpcQuest, NpcQuestProgress, NpcQuests}
import pangea.model.state.StateType
import pangea.model.stats.{FightStats, StatBoost}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.gustavo.{BoostStat, GustavoBoostState, GustavoState}
import pangea.service.state.states.guild.{MasterHornState, TrophyExchangeState}
import pangea.service.state.states.merchant.MerchantState
import pangea.service.state.states.tavern.InnkeeperState
import pangea.service.state.states.temple.TempleAzatState
import pangea.service.state.{NpcQuestLog, UserAction}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test.TestRandom
import zio.test._
import zio.{Task, ZIO}

/** Сюжетные задания горожан: одна система на пятерых. У каждого — кнопка в
  * меню, завязка с «взять / не сейчас», свои шаги и награда сверх опыта. */
object NpcQuestsSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def tapWith(key: String, k: String, v: String): UserAction =
    UserAction("", Some(s"""{"action":"$key","$k":"$v"}"""))

  private def hero: Hero = TestFixtures.hero(userId).copy(silver = 1000L, dungeonLevel = 3)

  private def gear(id: Long, rarity: Rarity): Item =
    Item(id, s"Предмет $id", 1L, rarity, ItemType.Helmet,
      attack = 0, accuracy = 0, energy = 0, armor = 1, defence = 0, evasion = 0)

  private def trophy(id: Long, kind: TrophyKind, lvl: Long = 5L): Item =
    Item(id, s"Трофей $id", lvl, Rarity.Gray, ItemType.Trophy,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Trophy(Race.Human.entryName, kind))

  private def flask(charges: Int, maxCharges: Int): Item =
    Item.NoItem.copy(name = "Фляга", itemType = ItemType.Flask,
      details = ItemDetails.Flask(FlaskEffect.HealPercent(25), charges = charges, maxCharges = maxCharges))

  private def quests(dao: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(dao, userId)

  private def seed(dao: TestHeroDao, q: NpcQuest, p: NpcQuestProgress): Task[Unit] =
    NpcQuestLog.modify(dao, userId)(_.updated(q, p)).unit

  private def heroOf(dao: TestHeroDao): Task[Hero] = dao.getHeroByUserId(userId).map(_.get)

  private def texts(r: pangea.test.TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  private def content = ZIO.attempt(SceneContent.load())

  override def spec = suite("Сюжетные задания горожан")(

    suite("модель и хранение")(
      test("пустая запись — ничего не взято; взятое — на первом шаге; новое поле не роняет разбор") {
        val q = NpcQuests.empty.take(NpcQuest.Innkeeper, 5L)
        val old = io.circe.parser.parse("""{"quests":{"horn":{"step":2,"counter":120}}}""").toOption.get
        val decoded = old.as[NpcQuests].toOption.get
        assertTrue(!NpcQuests.empty.isTaken(NpcQuest.Innkeeper)) &&
        assertTrue(q.onStep(NpcQuest.Innkeeper, 1) && q.of(NpcQuest.Innkeeper).since == 5L) &&
        assertTrue(q.finish(NpcQuest.Innkeeper).isDone(NpcQuest.Innkeeper)) &&
        assertTrue(!q.finish(NpcQuest.Innkeeper).isTaken(NpcQuest.Innkeeper)) &&
        assertTrue(decoded.onStep(NpcQuest.Horn, 2) && decoded.of(NpcQuest.Horn).counter == 120L) &&
        assertTrue(decoded.recipes.isEmpty) &&
        assertTrue(q.asJson.as[NpcQuests].toOption.contains(q))
      },
      test("опыт за задание — пять боёв на текущем этаже") {
        assertTrue(NpcQuestLog.expReward(hero) == 15L) &&
        assertTrue(NpcQuestLog.expReward(hero.copy(dungeonLevel = 0)) == 5L)
      }
    ),

    suite("Трактирщик — «Плата за первую кружку»")(
      test("кнопка предлагает задание; принятие — первый шаг; без трофея — упрёк") {
        for {
          dao <- TestHeroDao.withHero(userId, hero)
          r   <- TestRenderer.make
          c   <- content
          state = InnkeeperState(dao, TestInventoryRepository.accepting, c)
          _   <- state.action(testUser, tap("InnQuest"), r)
          offer <- r.sentScreens.map(_.last)
          _   <- state.action(testUser, tap("InnQuestAccept"), r)
          q   <- quests(dao)
          _   <- state.action(testUser, tap("InnQuest"), r)
          all <- texts(r)
        } yield assertTrue(offer.text.contains("прикрой")) &&
                assertTrue(offer.choices.map(_.id) == List("InnQuestAccept", "InnQuestDecline")) &&
                assertTrue(q.onStep(NpcQuest.Innkeeper, 1)) &&
                assertTrue(all.contains("Пустые руки"))
      },
      test("с трофеями забирает самый дешёвый, платит серебром и опытом, открывает рассказ о Кинэте") {
        val items = List(trophy(1L, TrophyKind.Relic), trophy(2L, TrophyKind.Sack), trophy(3L, TrophyKind.Head))
        for {
          dao <- TestHeroDao.withHero(userId, hero)
          _   <- seed(dao, NpcQuest.Innkeeper, NpcQuestProgress(step = 1))
          inv  = TestInventoryRepository.withItems(items)
          r   <- TestRenderer.make
          c   <- content
          state = InnkeeperState(dao, inv, c)
          _   <- state.action(testUser, tap("InnQuest"), r)
          h   <- heroOf(dao)
          q   <- quests(dao)
          all <- texts(r)
          menu <- r.sentScreens.map(_.last)
        } yield assertTrue(inv.snapshot.map(_.id) == List(1L, 3L)) &&   // ушёл мешок
                assertTrue(h.silver == 1000L + InnkeeperState.QuestSilver) &&
                assertTrue(h.exp == 15L) &&
                assertTrue(q.isDone(NpcQuest.Innkeeper)) &&
                assertTrue(all.contains("портал домой")) &&
                assertTrue(menu.choices.exists(_.id == "KinetLore")) &&
                assertTrue(!menu.choices.exists(_.id == "InnQuest"))
      }
    ),

    suite("Ришелье — «Товар с того света»")(
      test("пока серых меньше трёх — считает; три через «Продать хлам» — доплата, обновление и рассказ") {
        val items = List(gear(1L, Rarity.Gray), gear(2L, Rarity.Gray), gear(3L, Rarity.Gray), gear(4L, Rarity.Green))
        for {
          dao <- TestHeroDao.withHero(userId, hero)
          _   <- seed(dao, NpcQuest.Richelieu, NpcQuestProgress(step = 1))
          inv  = TestInventoryRepository.withItems(items.take(2))
          r   <- TestRenderer.make
          c   <- content
          state = MerchantState(dao, inv, TestItemRepository.make, c)
          _   <- TestRandom.feedLongs(1L, 2L, 3L, 4L, 5L, 6L)
          _   <- state.action(testUser, tap("RichQuest"), r)
          two <- texts(r)
          full = TestInventoryRepository.withItems(items)
          state2 = MerchantState(dao, full, TestItemRepository.make, c)
          _   <- state2.action(testUser, tap("SellJunk"), r)
          h   <- heroOf(dao)
          q   <- quests(dao)
          all <- texts(r)
          data <- dao.readMerchantData(userId).map(_.flatMap(_.as[MerchantState.MerchantData].toOption).get)
          grayPrice  = ((1L + 5) * 1.2 * Rarity.Gray.factorR).toLong
          whitePrice = ((1L + 5) * 1.2 * Rarity.White.factorR).toLong
        } yield assertTrue(two.contains("2. Я сказал — три")) &&
                assertTrue(q.isDone(NpcQuest.Richelieu)) &&
                assertTrue(h.silver == 1000L + 3L * grayPrice + 3L * (whitePrice - grayPrice)) &&
                assertTrue(data.refreshedAt == 0L) &&
                assertTrue(all.contains("Снятое")) &&
                assertTrue(full.snapshot.map(_.id) == List(4L))
      },
      test("продажа двух серых задание не закрывает") {
        for {
          dao <- TestHeroDao.withHero(userId, hero)
          _   <- seed(dao, NpcQuest.Richelieu, NpcQuestProgress(step = 1))
          inv  = TestInventoryRepository.withItems(List(gear(1L, Rarity.Gray), gear(2L, Rarity.Gray)))
          r   <- TestRenderer.make
          c   <- content
          state = MerchantState(dao, inv, TestItemRepository.make, c)
          _   <- TestRandom.feedLongs(1L, 2L, 3L, 4L, 5L, 6L)
          _   <- state.action(testUser, tap("SellJunk"), r)
          q   <- quests(dao)
        } yield assertTrue(q.onStep(NpcQuest.Richelieu, 1))
      }
    ),

    suite("Густаво — «Подопытный»")(
      test("зелье закрывает первый шаг, победы под зельем копятся, третья ведёт к отчёту") {
        for {
          dao <- TestHeroDao.withHero(userId, hero)
          _   <- seed(dao, NpcQuest.Gustavo, NpcQuestProgress(step = 1))
          r   <- TestRenderer.make
          c   <- content
          boost = GustavoBoostState(dao, c)
          _   <- boost.action(testUser, tapWith("BoostBuy", "stat", "str"), r)
          q1  <- quests(dao)
          h   <- heroOf(dao)
          _   <- NpcQuestLog.onVictory(dao, userId, 2, GustavoState.potionActive(h, 0L))
          q2  <- quests(dao)
          _   <- NpcQuestLog.onVictory(dao, userId, 1, potionActive = false) // зелье выветрилось — не в счёт
          q3  <- quests(dao)
          _   <- NpcQuestLog.onVictory(dao, userId, 1, potionActive = true)
          q4  <- quests(dao)
        } yield assertTrue(q1.onStep(NpcQuest.Gustavo, 2)) &&
                assertTrue(q2.of(NpcQuest.Gustavo).counter == 2L) &&
                assertTrue(q3.of(NpcQuest.Gustavo).counter == 2L) &&
                assertTrue(q4.onStep(NpcQuest.Gustavo, 3))
      },
      test("отчёт: фляга до краёв, серебро, опыт; без фляги — серебро «на флягу»") {
        for {
          dao <- TestHeroDao.withHero(userId, hero.copy(equipment = TestFixtures.emptyEquipment.copy(flask = flask(0, 4))))
          _   <- seed(dao, NpcQuest.Gustavo, NpcQuestProgress(step = 3, counter = 3L))
          r   <- TestRenderer.make
          c   <- content
          _   <- GustavoState(dao, TestInventoryRepository.accepting, c).action(testUser, tap("GusQuest"), r)
          h   <- heroOf(dao)
          q   <- quests(dao)
          all <- texts(r)
          charges = h.equipment.flask.details match { case f: ItemDetails.Flask => f.charges; case _ => -1 }
          dao2 <- TestHeroDao.withHero(userId, hero)
          _   <- seed(dao2, NpcQuest.Gustavo, NpcQuestProgress(step = 3, counter = 3L))
          r2  <- TestRenderer.make
          _   <- GustavoState(dao2, TestInventoryRepository.accepting, c).action(testUser, tap("GusQuest"), r2)
          h2  <- heroOf(dao2)
        } yield assertTrue(charges == 4) &&
                assertTrue(h.silver == 1000L + GustavoState.QuestSilver) &&
                assertTrue(h.exp == 15L) &&
                assertTrue(q.isDone(NpcQuest.Gustavo)) &&
                assertTrue(all.contains("Ашалдарон")) &&
                assertTrue(h2.silver == 1000L + GustavoState.QuestSilver + GustavoState.QuestFlaskSilver)
      },
      test("зелье выветрилось раньше троих — Густаво наливает ещё одно даром") {
        for {
          dao <- TestHeroDao.withHero(userId, hero)
          _   <- seed(dao, NpcQuest.Gustavo, NpcQuestProgress(step = 2, counter = 1L))
          r   <- TestRenderer.make
          c   <- content
          _   <- GustavoState(dao, TestInventoryRepository.accepting, c).action(testUser, tap("GusQuest"), r)
          q   <- quests(dao)
          all <- texts(r)
          boost = GustavoBoostState(dao, c)
          _   <- boost.enter(testUser, r)
          menu <- r.sentScreens.map(_.last)
          _   <- boost.action(testUser, tapWith("BoostBuy", "stat", "int"), r)
          h   <- heroOf(dao)
          q2  <- quests(dao)
        } yield assertTrue(all.contains("Выветрилось")) &&
                assertTrue(q.onStep(NpcQuest.Gustavo, 1) && q.of(NpcQuest.Gustavo).bonus) &&
                assertTrue(menu.choices.count(_.label.contains("бесплатно")) == 4) &&
                assertTrue(h.silver == 1000L) &&                       // угощение — не за серебро
                assertTrue(q2.onStep(NpcQuest.Gustavo, 2) && !q2.of(NpcQuest.Gustavo).bonus)
      },
      test("под зельем считаются и павшие в групповом бою — через победу в BattleState") {
        val h = hero.copy(
          fightStats = FightStats(atk = 100000L, hp = 500000L, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0),
          statBoosts = pangea.model.stats.StatBoosts.none.add(StatBoost(BoostStat.Str.boostName, BoostStat.Str.buff, Long.MaxValue), 0L))
        val mob = pangea.model.monster.Monster(0L, 3L, Race.Orc, MobRarity.Common,
          FightStats(atk = 1, hp = 10, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))
        for {
          dao <- TestHeroDao.withHero(userId, h)
          _   <- seed(dao, NpcQuest.Gustavo, NpcQuestProgress(step = 2, counter = 1L))
          _   <- dao.writeActiveBattle(userId, SoloPveBattle.fromGroup(List(mob, mob), h, Nil).asJson)
          r   <- TestRenderer.make
          c   <- content
          _   <- TestRandom.feedInts(60, 99, 60) *> TestRandom.feedLongs(100L, 100L)
          _   <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tapWith("Attack", "target", "1"), r)
          res <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
          q   <- quests(dao)
        } yield assertTrue(res == StateType.Loot) &&
                assertTrue(q.onStep(NpcQuest.Gustavo, 3) && q.of(NpcQuest.Gustavo).counter == 3L)
      }
    ),

    suite("Жрец — «То, что не пропадает»")(
      test("без камня — реплика; с камнем — пыль того же вида, рецепт, серебро, камень остаётся") {
        val ruby = GemGenerator.item(GemKind.Ruby, 2).copy(id = 7L)
        for {
          dao <- TestHeroDao.withHero(userId, hero)
          _   <- seed(dao, NpcQuest.Priest, NpcQuestProgress(step = 1))
          r   <- TestRenderer.make
          c   <- content
          empty = TestInventoryRepository.accepting
          _   <- TempleAzatState(dao, empty, TestItemRepository.make, c).action(testUser, tap("PriestQuest"), r)
          none <- texts(r)
          inv  = TestInventoryRepository.withItems(List(ruby))
          _   <- TempleAzatState(dao, inv, TestItemRepository.make, c).action(testUser, tap("PriestQuest"), r)
          h   <- heroOf(dao)
          q   <- quests(dao)
          all <- texts(r)
        } yield assertTrue(none.contains("щедр на камни")) &&
                assertTrue(inv.snapshot.exists(_.id == 7L)) &&
                assertTrue(inv.snapshot.exists(_.name == "Рубиновая пыль")) &&
                assertTrue(q.isDone(NpcQuest.Priest)) &&
                assertTrue(q.recipes == List(NpcQuests.DustAssemblyRecipe)) &&
                assertTrue(h.silver == 1000L + TempleAzatState.QuestSilver) &&
                assertTrue(all.contains("Ничто не пропадает"))
      },
      test("в меню Жреца кнопка задания стоит перед «Назад» и исчезает после выполнения") {
        for {
          dao <- TestHeroDao.withHero(userId, hero)
          r   <- TestRenderer.make
          c   <- content
          state = TempleAzatState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c)
          _   <- state.action(testUser, tap("Priest"), r)
          before <- r.sentScreens.map(_.last.choices.map(_.id))
          _   <- seed(dao, NpcQuest.Priest, NpcQuestProgress(step = 1, done = true))
          _   <- state.action(testUser, tap("Priest"), r)
          after <- r.sentScreens.map(_.last.choices.map(_.id))
        } yield assertTrue(before == List("WhoIsAzat", "AskBlessing", "PriestQuest", "BackToTemple")) &&
                assertTrue(after == List("WhoIsAzat", "AskBlessing", "BackToTemple"))
      }
    ),

    suite("Мастер Горн — «Ржавая вилка»")(
      test("завязка смотрит на оружие: серое — хлам, зелёное — «хоть что-то», без оружия — свой текст") {
        val gray  = gear(1L, Rarity.Gray).copy(name = "Меч новобранца", itemType = ItemType.Weapon)
        val green = gray.copy(rarity = Rarity.Green, name = "Клинок")
        def intro(weapon: Option[Item]) =
          for {
            dao <- TestHeroDao.withHero(userId, hero.copy(equipment = TestFixtures.emptyEquipment.copy(weapon = weapon.getOrElse(Item.NoItem))))
            r   <- TestRenderer.make
            c   <- content
            _   <- MasterHornState(dao, TestInventoryRepository.accepting, c).action(testUser, tap("HornQuest"), r)
            t   <- r.sentScreens.map(_.last.text)
          } yield t
        for {
          a <- intro(Some(gray))
          b <- intro(Some(green))
          n <- intro(None)
        } yield assertTrue(a.contains("Меч новобранца") && a.contains("двери подпирают")) &&
                assertTrue(b.contains("Клинок") && b.contains("хоть что-то")) &&
                assertTrue(n.contains("Голыми руками"))
      },
      test("репутация с трофеев копится до ста, потом улучшение даром и рассказ") {
        val head = trophy(1L, TrophyKind.Head, lvl = 95L) // 5 + 95 × 1 = 100 репутации
        for {
          dao <- TestHeroDao.withHero(userId, hero.copy(guildReputation = 0L, silver = 0L))
          _   <- seed(dao, NpcQuest.Horn, NpcQuestProgress(step = 1))
          r   <- TestRenderer.make
          c   <- content
          horn = MasterHornState(dao, TestInventoryRepository.accepting, c)
          _   <- horn.action(testUser, tap("HornQuest"), r)
          zero <- texts(r)
          _   <- TrophyExchangeState(dao, TestInventoryRepository.withItems(List(head)), c).action(testUser, tap("SubmitTrophies"), r)
          q1  <- quests(dao)
          _   <- horn.action(testUser, tap("ImproveArmor"), r)
          confirm <- r.sentScreens.map(_.last.text)
          _   <- horn.action(testUser, tap("ConfirmImprove"), r)
          h   <- heroOf(dao)
          q2  <- quests(dao)
          all <- texts(r)
        } yield assertTrue(zero.contains("У тебя 0")) &&
                assertTrue(q1.onStep(NpcQuest.Horn, 2) && q1.of(NpcQuest.Horn).counter == 100L) &&
                assertTrue(confirm.contains("даром")) &&
                assertTrue(h.masterHornBoosts.armor == 3L) &&
                assertTrue(h.guildReputation == 100L && h.silver == 0L) &&  // ничего не списано
                assertTrue(h.exp == 15L) &&
                assertTrue(q2.isDone(NpcQuest.Horn)) &&
                assertTrue(all.contains("Репутация — это учёт")) &&
                assertTrue(all.contains("Иди. И оружие себе найди"))
      },
      test("после задания улучшение снова платное") {
        for {
          dao <- TestHeroDao.withHero(userId, hero.copy(guildReputation = 100L, silver = 100L))
          _   <- seed(dao, NpcQuest.Horn, NpcQuestProgress(step = 2, counter = 100L, done = true))
          r   <- TestRenderer.make
          c   <- content
          horn = MasterHornState(dao, TestInventoryRepository.accepting, c)
          _   <- horn.action(testUser, tap("ImproveArmor"), r)
          _   <- horn.action(testUser, tap("ConfirmImprove"), r)
          h   <- heroOf(dao)
        } yield assertTrue(h.guildReputation == 95L && h.silver == 95L)
      }
    )
  )
}
