package pangea.service.state.states.tavern

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.item.{Item, ItemDetails, ItemType, Rarity, TrophyKind}
import pangea.model.monster.Race
import pangea.model.hero.LoreData
import pangea.model.quest.QuestData
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._

object InnkeeperStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  // Трофей нужной расы; по умолчанию вид = Голова (coef = 1.0).
  private def trophy(id: Long, race: Race, lvl: Long, kind: TrophyKind = TrophyKind.Head): Item =
    Item(id, s"${kind.displayName} (${race.toString})", lvl, Rarity.Gray, ItemType.Trophy,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Trophy(race.entryName, kind))

  private def makeState(items: List[Item], active: Option[Race]) = makeStateWith(items, active, LoreData.empty, 0L)

  /** То же, но с заданными знаниями о мире и серебром — для рассказа об элементалях. */
  private def makeStateWith(items: List[Item], active: Option[Race], lore: LoreData, silver: Long) =
    for {
      heroDao  <- TestHeroDao.withHero(userId,
                    TestFixtures.hero(userId, state = StateType.Innkeeper).copy(silver = silver))
      _        <- heroDao.writeLoreData(userId, lore.asJson)
      _        <- ZIO.foreachDiscard(active.toList)(r =>
                    heroDao.writeQuestData(userId, QuestData(3, Some(r.entryName), Long.MaxValue, Some(r.entryName)).asJson))
      invRepo   = TestInventoryRepository.withItems(items)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (InnkeeperState(heroDao, invRepo, content), heroDao, invRepo, renderer)

  override def spec = suite("InnkeeperState")(

    test("enter → кнопки «Сдать квестовые предметы» и «Вернуться»") {
      for {
        t <- makeState(Nil, None)
        (state, _, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.choices.map(_.id).toSet == Set("TurnInQuest", "OpenCharacter", "BackFromInnkeeper"))
    },

    test("TurnInQuest без активного задания → сообщение об отсутствии задания") {
      for {
        t <- makeState(List(trophy(1L, Race.Orc, 3L)), active = None)
        (state, _, invRepo, renderer) = t
        _       <- state.action(testUser, tap("TurnInQuest"), renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.exists(_.text.contains("нет активного задания"))) &&
              assertTrue(invRepo.snapshot.size == 1)
    },

    test("TurnInQuest с подходящим трофеем → забирает предмет, начисляет опыт, закрывает задание") {
      // lvl 3, Голова (coef=1.0) → exp = ceil(5 + 3*1.0) = 8
      for {
        t <- makeState(List(trophy(1L, Race.Orc, 3L)), active = Some(Race.Orc))
        (state, heroDao, invRepo, renderer) = t
        _       <- state.action(testUser, tap("TurnInQuest"), renderer)
        hero    <- heroDao.getHeroByUserId(userId)
        quests  <- heroDao.readQuestData(userId).map(_.flatMap(_.as[QuestData].toOption))
        screens <- renderer.sentScreens
      } yield assertTrue(invRepo.snapshot.isEmpty) &&
              assertTrue(hero.exists(_.exp == 8L)) &&
              assertTrue(quests.exists(_.active.isEmpty)) &&
              assertTrue(screens.exists(_.text.contains("Задание выполнено")))
    },

    test("TurnInQuest забирает трофей с наибольшим коэффициентом, а не первый по порядку") {
      // В инвентаре Орки: Мешок (0.5), Реликвия (4.0), Голова (1.0) — уходит Реликвия,
      // хотя лежит не первой. Эльф не подходит по расе и остаётся.
      for {
        t <- makeState(
               List(trophy(1L, Race.Elf, 9L, TrophyKind.Relic),
                    trophy(2L, Race.Orc, 5L, TrophyKind.Sack),
                    trophy(3L, Race.Orc, 5L, TrophyKind.Relic),
                    trophy(4L, Race.Orc, 5L, TrophyKind.Head)),
               active = Some(Race.Orc))
        (state, heroDao, invRepo, renderer) = t
        _    <- state.action(testUser, tap("TurnInQuest"), renderer)
        hero <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(invRepo.snapshot.map(_.id) == List(1L, 2L, 4L)) && // ушла id=3 (Реликвия)
              assertTrue(hero.exists(_.exp == 25L))                         // ceil(5 + 5*4.0)
    },

    test("при равных коэффициентах уходит трофей старшего уровня") {
      for {
        t <- makeState(
               List(trophy(1L, Race.Orc, 2L, TrophyKind.Talisman),
                    trophy(2L, Race.Orc, 11L, TrophyKind.Talisman),
                    trophy(3L, Race.Orc, 7L, TrophyKind.Talisman)),
               active = Some(Race.Orc))
        (state, heroDao, invRepo, renderer) = t
        _    <- state.action(testUser, tap("TurnInQuest"), renderer)
        hero <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(invRepo.snapshot.map(_.id) == List(1L, 3L)) && // ушла id=2 (11 ур.)
              assertTrue(hero.exists(_.exp == 27L))                     // ceil(5 + 11*2.0)
    },

    test("bestTrophyFor: порядок видов Реликвия > Талисман > Голова > Мешок, чужая раса не берётся") {
      val orcs = List(
        trophy(1L, Race.Orc, 5L, TrophyKind.Sack),
        trophy(2L, Race.Orc, 5L, TrophyKind.Head),
        trophy(3L, Race.Orc, 5L, TrophyKind.Talisman),
        trophy(4L, Race.Orc, 5L, TrophyKind.Relic))
      val race = Race.Orc.entryName
      // Убираем лучший вид по одному — каждый раз всплывает следующий по коэффициенту.
      assertTrue(InnkeeperState.bestTrophyFor(orcs, race).map(_.id).contains(4L)) &&
      assertTrue(InnkeeperState.bestTrophyFor(orcs.filterNot(_.id == 4L), race).map(_.id).contains(3L)) &&
      assertTrue(InnkeeperState.bestTrophyFor(orcs.filterNot(i => i.id == 4L || i.id == 3L), race).map(_.id).contains(2L)) &&
      assertTrue(InnkeeperState.bestTrophyFor(List(orcs.head), race).map(_.id).contains(1L)) &&
      assertTrue(InnkeeperState.bestTrophyFor(orcs, Race.Elf.entryName).isEmpty) &&
      assertTrue(InnkeeperState.bestTrophyFor(Nil, race).isEmpty)
    },

    test("TurnInQuest без подходящего трофея → сообщение, инвентарь не тронут") {
      for {
        t <- makeState(List(trophy(1L, Race.Elf, 3L)), active = Some(Race.Orc))
        (state, _, invRepo, renderer) = t
        _       <- state.action(testUser, tap("TurnInQuest"), renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(invRepo.snapshot.size == 1) &&
              assertTrue(screens.exists(_.text.contains("нет подходящего трофея")))
    },

    test("BackFromInnkeeper → переход в Tavern") {
      for {
        t <- makeState(Nil, None)
        (state, _, _, renderer) = t
        result <- state.action(testUser, tap("BackFromInnkeeper"), renderer)
      } yield assertTrue(result == StateType.Tavern)
    },
    // ── Рассказ об элементалях ────────────────────────────────────────────────
    test("кнопки про элементалей нет, пока герой их не встречал") {
      for {
        t <- makeStateWith(Nil, None, LoreData.empty, 5000L)
        (state, _, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(!screens.last.choices.map(_.id).contains("ElementalLore"))
    },

    test("встретил элементаля → кнопка появилась и держится, пока не заплатил") {
      for {
        t <- makeStateWith(Nil, None, LoreData(metElemental = true), 5000L)
        (state, dao, _, renderer) = t
        _        <- state.enter(testUser, renderer)
        before   <- renderer.sentScreens.map(_.last.choices.map(_.id))
        _        <- state.action(testUser, tap("ElementalLore"), renderer)
        offer    <- renderer.sentScreens.map(_.last)
        _        <- state.action(testUser, tap("PayElementalLore"), renderer)
        lore     <- dao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption).get)
        silver   <- dao.getHeroByUserId(userId).map(_.get.silver)
        told     <- renderer.sentScreens.map(_.last)
        _        <- state.enter(testUser, renderer)
        after    <- renderer.sentScreens.map(_.last.choices.map(_.id))
      } yield assertTrue(before.contains("ElementalLore")) &&
              assertTrue(offer.text.contains("2000 серебра")) &&
              assertTrue(lore.elementalLore) &&
              assertTrue(silver == 3000L) &&
              assertTrue(told.text.contains("Сноходцы")) &&
              assertTrue(told.choices.map(_.label).contains("Надеюсь это стоило моего серебра.")) &&
              // после оплаты кнопка исчезает
              assertTrue(!after.contains("ElementalLore"))
    },

    test("легенду об элементалях продают строго один раз — старая запись знаний её не сбрасывает") {
      // До Гнилого Джо в lore_data лежали только два поля. Производный декодер
      // требовал все четыре и ронял разбор целиком, знания подменялись пустыми —
      // и трактирщик предлагал уже купленную легенду по второму кругу.
      val oldRecord = Json.obj(
        "metElemental"  -> Json.True,
        "elementalLore" -> Json.True
      )
      for {
        t <- makeStateWith(Nil, None, LoreData.empty, 5000L)
        (state, dao, _, renderer) = t
        _      <- dao.writeLoreData(userId, oldRecord)
        _      <- state.enter(testUser, renderer)
        ids    <- renderer.sentScreens.map(_.last.choices.map(_.id))
        parsed  = oldRecord.as[LoreData].toOption.get
      } yield assertTrue(parsed == LoreData(metElemental = true, elementalLore = true)) &&
              assertTrue(!ids.contains("ElementalLore"))
    },

    test("встреча с любым элементалем открывает один и тот же рассказ") {
      // Вид элементаля в знаниях не хранится: и огненный, и каменный ставят один
      // флаг metElemental, поэтому легенда одна на двоих и покупается однажды.
      for {
        t <- makeStateWith(Nil, None, LoreData(metElemental = true), 5000L)
        (state, dao, _, renderer) = t
        _     <- state.action(testUser, tap("ElementalLore"), renderer)
        _     <- state.action(testUser, tap("PayElementalLore"), renderer)
        // «встретил ещё одного» — флаг встречи уже стоит, знание не сбрасывается
        lore  <- dao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption).get)
        _     <- dao.writeLoreData(userId, lore.copy(metElemental = true).asJson)
        _     <- state.enter(testUser, renderer)
        ids   <- renderer.sentScreens.map(_.last.choices.map(_.id))
        silver <- dao.getHeroByUserId(userId).map(_.get.silver)
      } yield assertTrue(lore.elementalLore) &&
              assertTrue(!ids.contains("ElementalLore")) &&
              // серебро списано ровно один раз
              assertTrue(silver == 3000L)
    },

    // ── Рассказ о Гнилом Джо ──────────────────────────────────────────────────
    test("кнопки про Джо нет, пока герой его не встречал") {
      for {
        t <- makeStateWith(Nil, None, LoreData.empty, 5000L)
        (state, _, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(!screens.last.choices.map(_.id).contains("JoeLore"))
    },

    test("встретил Джо → кнопка появилась, рассказ стоит 1000 серебра") {
      for {
        t <- makeStateWith(Nil, None, LoreData(metJoe = true), 5000L)
        (state, dao, _, renderer) = t
        _      <- state.enter(testUser, renderer)
        before <- renderer.sentScreens.map(_.last.choices.map(_.id))
        _      <- state.action(testUser, tap("JoeLore"), renderer)
        offer  <- renderer.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("PayJoeLore"), renderer)
        lore   <- dao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption).get)
        silver <- dao.getHeroByUserId(userId).map(_.get.silver)
        told   <- renderer.sentScreens.map(_.last)
        _      <- state.enter(testUser, renderer)
        after  <- renderer.sentScreens.map(_.last.choices.map(_.id))
      } yield assertTrue(before.contains("JoeLore")) &&
              assertTrue(offer.text.contains("1000 серебра")) &&
              assertTrue(lore.joeLore) &&
              assertTrue(silver == 4000L) &&
              assertTrue(told.text.contains("Джо Галтон")) &&
              assertTrue(told.text.contains("яд и кровь против него бесполезны")) &&
              assertTrue(told.choices.map(_.label).contains("Надеюсь это стоило моего серебра.")) &&
              // после оплаты кнопка исчезает
              assertTrue(!after.contains("JoeLore"))
    },

    test("на рассказ о Джо не хватает серебра → деньги не списаны") {
      for {
        t <- makeStateWith(Nil, None, LoreData(metJoe = true), 100L)
        (state, dao, _, renderer) = t
        _      <- state.action(testUser, tap("PayJoeLore"), renderer)
        lore   <- dao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption).get)
        silver <- dao.getHeroByUserId(userId).map(_.get.silver)
      } yield assertTrue(!lore.joeLore) && assertTrue(silver == 100L)
    },

    test("не хватает серебра → рассказа нет и деньги не списаны") {
      for {
        t <- makeStateWith(Nil, None, LoreData(metElemental = true), 100L)
        (state, dao, _, renderer) = t
        _       <- state.action(testUser, tap("PayElementalLore"), renderer)
        lore    <- dao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption).get)
        silver  <- dao.getHeroByUserId(userId).map(_.get.silver)
        screens <- renderer.sentScreens
      } yield assertTrue(!lore.elementalLore) && assertTrue(silver == 100L) &&
              assertTrue(screens.map(_.text).mkString.contains("Столько серебра у тебя нет"))
    },

    test("повторная оплата невозможна — второй раз серебро не спишется") {
      for {
        t <- makeStateWith(Nil, None, LoreData(metElemental = true, elementalLore = true), 5000L)
        (state, dao, _, renderer) = t
        _      <- state.action(testUser, tap("PayElementalLore"), renderer)
        silver <- dao.getHeroByUserId(userId).map(_.get.silver)
      } yield assertTrue(silver == 5000L)
    }

  )
}
