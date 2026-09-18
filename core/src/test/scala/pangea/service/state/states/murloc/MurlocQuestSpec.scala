package pangea.service.state.states.murloc

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Achievement, Hero}
import pangea.model.item.{Item, ItemType, QuestItemKind, Rarity}
import pangea.model.monster.{Monster, Race, Rarity => MobRarity}
import pangea.model.quest.{NpcQuest, NpcQuestProgress, NpcQuests}
import pangea.model.squad.{Ally, AllyKind, Squad}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.dungeon.DungeonState
import pangea.service.state.states.murloc.MurlocVillageState.{Progress, Step}
import pangea.service.state.states.{DeathState, InventoryState}
import pangea.service.state.{MurlocQuest, NpcQuestLog, UserAction}
import pangea.test._
import zio.test.TestRandom
import zio.test._
import zio.{Task, ZIO}

/** «Деревня Мурлоков»: старейшина после сотого убитого, карта, налёт на деревню
  * (строй и очередь, Плюх уходит, награда как за обычный бой) и помощь
  * снаряжением (пачки по цвету, клинок за двадцатую вещь, клинок ждёт, если
  * сумка полна), достижения с плюсом к ловкости и силе. */
object MurlocQuestSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero: Hero = TestFixtures.hero(userId).copy(lvl = 7L)
  private def murloc: Hero = hero.copy(race = Race.Murloc)

  private def map(id: Long = 12L): Item = QuestItemKind.item(QuestItemKind.MurlocVillageMap).copy(id = id)
  private def gear(id: Long, rarity: Rarity = Rarity.Gray, lvl: Long = 1L, itemType: ItemType = ItemType.Helmet): Item =
    Item(id, s"Предмет $id", lvl, rarity, itemType, attack = 0, accuracy = 0, energy = 0, armor = 1, defence = 0, evasion = 0)
  private def gem(id: Long): Item = gear(id).copy(itemType = ItemType.Gem)

  private def content = ZIO.attempt(SceneContent.load())

  private def quests(dao: TestHeroDao): Task[NpcQuests] = NpcQuestLog.load(dao, userId)
  private def seed(dao: TestHeroDao, p: NpcQuestProgress): Task[Unit] =
    NpcQuestLog.modify(dao, userId)(_.updated(NpcQuest.Murloc, p)).unit
  private def heroOf(dao: TestHeroDao): Task[Hero] = dao.getHeroByUserId(userId).map(_.get)
  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  private def weakMob(hp: Long = 10L): Monster =
    Monster(0L, 1L, Race.Orc, MobRarity.Common, FightStats(atk = 1, hp = hp, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0))

  private def strong(h: Hero = hero): Hero =
    h.copy(fightStats = FightStats(atk = 100000L, hp = 500000L, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))

  /** Деревня с картой на руках (задание на втором шаге, `handed` сдано). */
  private def village(h: Hero, items: List[Item], handed: Long = 0L, inv: Option[TestInventoryRepository] = None) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      _   <- seed(dao, NpcQuestProgress(step = 2, counter = handed))
      _   <- dao.writeSceneData(userId, Progress(Step.Route).asJson)
      repo = inv.getOrElse(TestInventoryRepository.withItems(map() :: items))
      r   <- TestRenderer.make
      c   <- content
    } yield (MurlocVillageState(dao, repo, TestItemRepository.make, c), dao, repo, r)

  override def spec = suite("Деревня Мурлоков")(

    suite("старейшина")(
      test("сотый убитый: задание на первом шаге, добыча ведёт к старейшине; ветеран с сотней за спиной — тоже") {
        def kill(h: Hero) =
          for {
            dao <- TestHeroDao.withHero(userId, h)
            _   <- dao.writeActiveBattle(userId, SoloPveBattle.from(weakMob(), h).asJson)
            r   <- TestRenderer.make
            c   <- content
            _   <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
            res <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
            loot <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption).get)
            q   <- quests(dao)
          } yield (res, loot, q)
        for {
          fresh   <- kill(strong().copy(kills = 99L))
          veteran <- kill(strong().copy(kills = 150L))
          early   <- kill(strong().copy(kills = 10L))
        } yield assertTrue(fresh._1 == StateType.Loot && fresh._2.returnState.contains(StateType.MurlocElder) && MurlocQuest.elderPending(fresh._3)) &&
                assertTrue(veteran._2.returnState.contains(StateType.MurlocElder) && MurlocQuest.elderPending(veteran._3)) &&
                assertTrue(early._2.returnState.isEmpty && !early._3.isTaken(NpcQuest.Murloc))
      },
      test("добыча вела в другую сцену — старейшина выходит на ближайшем осмотре этажа") {
        for {
          dao   <- TestHeroDao.withHero(userId, hero)
          _     <- seed(dao, NpcQuestProgress(step = 1))
          sched <- TestScheduler.make
          r     <- TestRenderer.make
          c     <- content
          res   <- DungeonState(dao, TestInventoryRepository.accepting, sched, c).action(testUser, tap("FindEvent"), r)
        } yield assertTrue(res == StateType.MurlocElder)
      },
      test("разговор: штаны, дорога от ворот, вялое согласие — карта в сумке, задание на втором шаге, в лабиринт") {
        for {
          dao <- TestHeroDao.withHero(userId, hero)
          _   <- seed(dao, NpcQuestProgress(step = 1))
          inv  = TestInventoryRepository.accepting
          r   <- TestRenderer.make
          c   <- content
          state = MurlocElderState(dao, inv, TestItemRepository.make, c)
          _   <- state.enter(testUser, r)
          meet <- r.sentScreens.map(_.last)
          _   <- state.action(testUser, tap("ElderWho"), r)
          _   <- state.action(testUser, tap("ElderWhat"), r)
          _   <- state.action(testUser, tap("ElderWhere"), r)
          last <- r.sentScreens.map(_.last)
          res <- state.action(testUser, tap("ElderAgree"), r)
          all <- texts(r)
          q   <- quests(dao)
        } yield assertTrue(meet.text.contains("Штаны на нём причудливые") && meet.choices.map(_.id) == List("ElderWho")) &&
                assertTrue(last.choices.head.label == "Ну… может, занесу." && last.choices.head.label.length <= 40) &&
                assertTrue(all.contains("Мрачноглаз") && all.contains("ворот Кинэта") && all.contains("Обещать не стану")) &&
                assertTrue(all.contains("Добавлен квестовый предмет «Деревня Мурлоков»")) &&
                assertTrue(inv.snapshot.count(_.questItem.contains(QuestItemKind.MurlocVillageMap)) == 1) &&
                assertTrue(res == StateType.Dungeon && q.onStep(NpcQuest.Murloc, 2) && MurlocQuest.mapOnHands(q))
      },
      test("герой-мурлок: старейшина узнаёт своего, другие реплики и кнопки") {
        for {
          dao <- TestHeroDao.withHero(userId, murloc)
          _   <- seed(dao, NpcQuestProgress(step = 1))
          r   <- TestRenderer.make
          c   <- content
          state = MurlocElderState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c)
          _   <- state.enter(testUser, r)
          meet <- r.sentScreens.map(_.last)
          _   <- state.action(testUser, tap("ElderWho"), r)
          _   <- state.action(testUser, tap("ElderWhat"), r)
          _   <- state.action(testUser, tap("ElderWhere"), r)
          last <- r.sentScreens.map(_.last)
          _   <- state.action(testUser, tap("ElderAgree"), r)
          all <- texts(r)
        } yield assertTrue(meet.text.contains("Свой!") && meet.choices.head.label == "Кто ты?") &&
                assertTrue(last.choices.head.label == "Ну… занесу, наверное.") &&
                assertTrue(all.contains("Твоей крови") && all.contains("не как чужой, как свой") && all.contains("в лапу"))
      }
    ),

    suite("карта")(
      test("в инвентаре: сложность гроб и «Отправиться по карте»; из лабиринта нельзя, из города — развилка") {
        def inventoryFrom(from: StateType) =
          for {
            dao <- TestHeroDao.withHero(userId, hero)
            _   <- dao.writeReturnState(userId, Some(from))
            _   <- seed(dao, NpcQuestProgress(step = 2))
            r   <- TestRenderer.make
            c   <- content
          } yield (InventoryState(dao, TestInventoryRepository.withItems(List(map())), TestItemRepository.make, c), dao, r)
        for {
          t <- inventoryFrom(StateType.Dungeon)
          (state, _, r) = t
          _    <- state.action(testUser, tap(s"${InventoryState.ItemActionPrefix}12"), r)
          card <- r.sentScreens.map(_.last)
          stay <- state.action(testUser, tap("UseMurlocMap"), r)
          all  <- texts(r)
          t2 <- inventoryFrom(StateType.GlobalMap)
          (state2, dao2, r2) = t2
          gone <- state2.action(testUser, tap("UseMurlocMap"), r2)
          p2   <- dao2.readSceneData(userId).map(_.flatMap(_.as[Progress].toOption).get)
        } yield assertTrue(card.text.contains("Сложность: ⚰️") && card.text.contains("трёх камней")) &&
                assertTrue(card.choices.map(_.id) == List("UseMurlocMap", "InventoryList")) &&
                assertTrue(stay == StateType.Inventory && all.contains("только из города")) &&
                assertTrue(gone == StateType.MurlocVillage && p2.step == Step.Route)
      },
      test("развилка: напасть или помочь; «Напасть» предупреждает и даёт уйти") {
        for {
          t <- village(hero, Nil)
          (state, _, _, r) = t
          _     <- state.enter(testUser, r)
          route <- r.sentScreens.map(_.last)
          _     <- state.action(testUser, tap("Attack"), r)
          warn  <- r.sentScreens.map(_.last)
          left  <- state.action(testUser, tap("ToCity"), r)
        } yield assertTrue(route.text.contains("зачем вы туда идёте") && route.choices.map(_.id) == List("Attack", "Help", "ToInventory")) &&
                assertTrue(warn.text.contains("двадцати четырёх мурлоков") && warn.choices.map(_.id) == List("Raid", "ToCity")) &&
                assertTrue(left == StateType.GlobalMap)
      }
    ),

    suite("налёт")(
      test("строй из десяти 2–3 ранга, за ним двенадцать 3–4 и Старый Мрачноглаз; уровень героя; Плюх уходит на сутки") {
        val squad = Squad(heroPos = 1, allies = List(Ally(AllyKind.Murloc, 2, 100L, 100L, 0L, hiredUntil = Long.MaxValue),
                                                    Ally(AllyKind.Human, 3, 100L, 100L, 0L, hiredUntil = Long.MaxValue)))
        for {
          t <- village(hero.copy(squad = squad), Nil, handed = 3L)
          (state, dao, _, r) = t
          res    <- state.action(testUser, tap("Raid"), r)
          battle <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
          loot   <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption).get)
          after  <- heroOf(dao)
          all    <- texts(r)
          front   = battle.monstersInOrder
          queue   = battle.group.queue
        } yield assertTrue(res == StateType.Battle && battle.story.contains(MurlocQuest.RaidStory)) &&
                assertTrue(front.size == 10 && front.forall(m => m.race == Race.Murloc.entryName && m.lvl == 7L)) &&
                assertTrue(front.forall(m => Set(MobRarity.Uncommon, MobRarity.Rare).map(_.entryName).contains(m.rarity))) &&
                assertTrue(queue.size == 13 && queue.forall(m => m.race == Race.Murloc.entryName && m.lvl == 7L)) &&
                assertTrue(queue.init.forall(m => Set(MobRarity.Rare, MobRarity.Mythical).map(_.entryName).contains(m.rarity))) &&
                assertTrue(queue.last.rarity == MobRarity.Legendary.entryName && queue.last.name == "Старый Мрачноглаз") &&
                assertTrue(loot.returnState.contains(StateType.MurlocVillage) && loot.eventData.flatMap(_.as[Progress].toOption).exists(_.step == Step.AfterRaid)) &&
                // Плюх ушёл ещё до боя — по свитку, на сутки; Йорген остался
                assertTrue(battle.group.allies.map(_.kind) == List(AllyKind.Human)) &&
                assertTrue(!after.squad.has(AllyKind.Murloc) && after.squad.away.contains(AllyKind.Murloc.entryName)) &&
                assertTrue(all.contains("Я не собирался в таком участвовать") && all.contains("Их много. Очень много.")) &&
                assertTrue(all.contains("в кирасе, которую вы же ему и принесли"))
      },
      test("герой-мурлок нападает: тишина и голос старейшины") {
        for {
          t <- village(murloc, Nil)
          (state, _, _, r) = t
          _   <- state.action(testUser, tap("Raid"), r)
          all <- texts(r)
        } yield assertTrue(all.contains("С железом — на своих?") && !all.contains("Их много. Очень много."))
      },
      test("налёт награждается как обычный бой: опыт и счёт убитых, а не пустота сюжетного боя") {
        val h = strong().copy(kills = 5L, exp = 0L)
        for {
          dao <- TestHeroDao.withHero(userId, h)
          _   <- dao.writeActiveBattle(userId, SoloPveBattle.from(weakMob(), h, squad = false).copy(story = Some(MurlocQuest.RaidStory)).asJson)
          r   <- TestRenderer.make
          c   <- content
          _   <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L)
          res <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap("Attack"), r)
          after <- heroOf(dao)
        } yield assertTrue(res == StateType.Loot && after.exp > 0L && after.kills == 6L)
      },
      test("бегство из сюжетного боя ведёт в город, а не в лабиринт") {
        val h = strong()
        for {
          dao <- TestHeroDao.withHero(userId, h)
          _   <- dao.writeActiveBattle(userId, SoloPveBattle.from(weakMob(), h, squad = false).copy(story = Some(MurlocQuest.RaidStory)).asJson)
          r   <- TestRenderer.make
          c   <- content
          _   <- TestRandom.feedInts(100) *> TestRandom.feedLongs(100L)
          res <- BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c).action(testUser, tap("ConfirmFlee"), r)
        } yield assertTrue(res == StateType.GlobalMap)
      },
      test("смерть в налёте: проснуться в городе у трёх камней, карта остаётся") {
        for {
          dao <- TestHeroDao.withHero(userId, hero.copy(exp = 80L))
          _   <- dao.writeActiveBattle(userId, SoloPveBattle.from(weakMob(), hero, squad = false).copy(story = Some(MurlocQuest.RaidStory)).asJson)
          _   <- seed(dao, NpcQuestProgress(step = 2))
          inv  = TestInventoryRepository.withItems(List(map()))
          r   <- TestRenderer.make
          c   <- content
          _   <- TestRandom.feedInts(1, 0)
          _   <- DeathState(dao, inv, c).enter(testUser, r)
          scene <- dao.readSceneData(userId).map(_.get)
          q   <- quests(dao)
        } yield assertTrue(scene.hcursor.get[StateType]("wakeTo").toOption.contains(StateType.GlobalMap)) &&
                assertTrue(scene.hcursor.get[List[String]]("wakeLines").toOption.contains(List("murlocVillage.raid.death"))) &&
                assertTrue(inv.snapshot.exists(_.questItem.contains(QuestItemKind.MurlocVillageMap)) && q.onStep(NpcQuest.Murloc, 2))
      },
      test("после победы: деревня пуста, карта уходит, «Гроза Мурлоков» и +1 к силе, кнопка в город") {
        for {
          t <- village(hero, Nil)
          (state, dao, inv, r) = t
          _     <- dao.writeSceneData(userId, Progress(Step.AfterRaid).asJson)
          _     <- state.enter(testUser, r)
          last  <- r.sentScreens.map(_.last)
          all   <- texts(r)
          after <- heroOf(dao)
          q     <- quests(dao)
          res   <- state.action(testUser, tap("ToCity"), r)
        } yield assertTrue(all.contains("Тростник затих") && all.contains("Квест «Деревня Мурлоков» завершён")) &&
                assertTrue(all.contains("«Гроза Мурлоков». +1 к силе")) &&
                assertTrue(after.hasAchievement(Achievement.MurlocBane) && after.effectiveBaseStats(0L).str == hero.effectiveBaseStats(0L).str + 1L) &&
                assertTrue(!inv.snapshot.exists(_.questItem.contains(QuestItemKind.MurlocVillageMap)) && q.isDone(NpcQuest.Murloc)) &&
                assertTrue(last.choices.map(_.id) == List("ToCity") && res == StateType.GlobalMap)
      }
    ),

    suite("помощь")(
      test("деревня и экран сдачи: кнопки только по цветам, что есть в сумке; камни и надетое не в счёт") {
        val items = List(gear(1L), gear(2L, Rarity.White), gear(3L, Rarity.Green), gear(4L, Rarity.Violet), gear(5L, Rarity.Orange), gem(6L))
        for {
          t <- village(hero, items, handed = 2L)
          (state, _, _, r) = t
          _    <- state.action(testUser, tap("Help"), r)
          home <- r.sentScreens.map(_.last)
          _    <- state.action(testUser, tap("HandIn"), r)
          hand <- r.sentScreens.map(_.last)
        } yield assertTrue(home.text.contains("Сухой пришёл") && home.choices.map(_.id) == List("HandIn", "ToCity")) &&
                assertTrue(hand.text.contains("Сдано: 2 из 20")) &&
                assertTrue(hand.choices.map(_.label) == List("Сдать всё чёрное и белое (⚫⚪ 2)", "Сдать всё зелёное (🟢 1)",
                  "Сдать всё фиолетовое (🟣 1)", "Сдать всё (5)", "Назад"))
      },
      test("сдать пачку: вещи уходят, счётчик растёт, старейшина благодарит, экран сдачи снова") {
        val items = (1L to 5L).map(i => gear(i, Rarity.Gray, lvl = i)).toList :+ gear(6L, Rarity.Blue)
        for {
          t <- village(hero, items, handed = 10L)
          (state, dao, inv, r) = t
          _    <- state.action(testUser, tap("GiveGray"), r)
          all  <- texts(r)
          q    <- quests(dao)
          hand <- r.sentScreens.map(_.last)
        } yield assertTrue(all.contains("Осталось ещё 5 снаряжения")) &&
                assertTrue(q.of(NpcQuest.Murloc).counter == 15L) &&
                assertTrue(inv.snapshot.map(_.id).sorted == List(6L, 12L)) &&
                assertTrue(hand.text.contains("Сдано: 15 из 20") && hand.choices.map(_.id) == List("GiveBlue", "GiveAll", "Village"))
      },
      test("лишнего старейшина не берёт: до двадцати не хватало трёх — ушли три худших, остальное в сумке") {
        val items = (1L to 5L).map(i => gear(i, Rarity.Gray, lvl = 6L - i)).toList :+ gear(6L, Rarity.Blue)
        for {
          t <- village(hero, items, handed = 17L)
          (state, dao, inv, r) = t
          res <- state.action(testUser, tap("GiveGray"), r)
          q   <- quests(dao)
          left = inv.snapshot.filterNot(_.name == MurlocQuest.BladeName).map(_.id).sorted
        } yield assertTrue(res == StateType.GlobalMap && q.isDone(NpcQuest.Murloc)) &&
                // серые уровней 1–3 (id 5, 4, 3) ушли; 4-го и 5-го уровня (id 2, 1) и синий остались, карта — нет
                assertTrue(left == List(1L, 2L, 6L)) &&
                assertTrue(inv.snapshot.exists(_.name == MurlocQuest.BladeName))
      },
      test("двадцатая вещь: клинок старого мурлока (легендарный, уровня героя), карта уходит, «Любимец Мурлоков» и +1 к ловкости") {
        for {
          t <- village(hero, List(gear(1L)), handed = 19L)
          (state, dao, inv, r) = t
          res   <- state.action(testUser, tap("GiveAll"), r)
          all   <- texts(r)
          after <- heroOf(dao)
          q     <- quests(dao)
          blade  = inv.snapshot.find(_.name == MurlocQuest.BladeName)
        } yield assertTrue(res == StateType.GlobalMap) &&
                assertTrue(all.contains("Он твой, сухой") && all.contains("Вы получили «Клинок старого мурлока»") && all.contains("завершён")) &&
                assertTrue(blade.exists(b => b.rarity == Rarity.Orange && b.itemType == ItemType.Weapon && b.lvl == 7L)) &&
                assertTrue(!inv.snapshot.exists(_.questItem.contains(QuestItemKind.MurlocVillageMap)) && q.isDone(NpcQuest.Murloc)) &&
                assertTrue(all.contains("«Любимец Мурлоков». +1 к ловкости")) &&
                assertTrue(after.hasAchievement(Achievement.MurlocFavorite) && after.effectiveBaseStats(0L).agi == hero.effectiveBaseStats(0L).agi + 1L)
      },
      test("герой-мурлок: свои реплики в деревне, при сдаче и при награде") {
        for {
          t <- village(murloc, List(gear(1L), gear(2L)), handed = 18L)
          (state, _, _, r) = t
          _   <- state.action(testUser, tap("Help"), r)
          _   <- state.action(testUser, tap("GiveGray"), r)
          all <- texts(r)
        } yield assertTrue(all.contains("Наш пришёл") && all.contains("по праву крови") && !all.contains("Он твой, сухой"))
      },
      test("сумка полна: клинок ждёт в деревне, задание на третьем шаге; освободил место — «Забрать клинок»") {
        val full = new TestInventoryRepository(canAdd = false, items = List(map(), gear(1L)))
        for {
          t <- village(hero, Nil, handed = 19L, inv = Some(full))
          (state, dao, inv, r) = t
          res  <- state.action(testUser, tap("GiveAll"), r)
          all  <- texts(r)
          q    <- quests(dao)
          _    <- state.action(testUser, tap("Help"), r)
          wait <- r.sentScreens.map(_.last)
          // место появилось
          roomy = TestInventoryRepository.withItems(inv.snapshot)
          c    <- content
          state2 = MurlocVillageState(dao, roomy, TestItemRepository.make, c)
          res2 <- state2.action(testUser, tap("TakeBlade"), r)
          q2   <- quests(dao)
        } yield assertTrue(res == StateType.GlobalMap && all.contains("в карман не сунешь")) &&
                assertTrue(q.onStep(NpcQuest.Murloc, 3) && MurlocQuest.rewardWaits(q) && inv.snapshot.exists(_.questItem.contains(QuestItemKind.MurlocVillageMap))) &&
                assertTrue(wait.text.contains("с клинком в лапах") && wait.choices.map(_.id) == List("TakeBlade", "ToCity")) &&
                assertTrue(res2 == StateType.GlobalMap && roomy.snapshot.exists(_.name == MurlocQuest.BladeName) && q2.isDone(NpcQuest.Murloc)) &&
                assertTrue(!roomy.snapshot.exists(_.questItem.contains(QuestItemKind.MurlocVillageMap)))
      }
    )
  )
}
