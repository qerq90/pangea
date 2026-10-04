package pangea.service.state.states.tavern

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.item.Item
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

    test("enter → задание Трактирщика и «Вернуться»; сдача трофеев ушла на доску") {
      for {
        t <- makeState(Nil, None)
        (state, _, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.last.choices.map(_.id).toSet ==
                Set("InnQuest", "OpenCharacter", "BackFromInnkeeper", "GoToCity"))
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

    // ── Рассказ о Белом волке ─────────────────────────────────────────────────
    test("встретил Белого волка → кнопка появилась, рассказ стоит 1500 серебра и упоминает шкуру") {
      for {
        t <- makeStateWith(Nil, None, LoreData(metWolf = true), 5000L)
        (state, dao, _, renderer) = t
        _      <- state.enter(testUser, renderer)
        before <- renderer.sentScreens.map(_.last.choices.map(_.id))
        _      <- state.action(testUser, tap("WolfLore"), renderer)
        offer  <- renderer.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("PayWolfLore"), renderer)
        lore   <- dao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption).get)
        silver <- dao.getHeroByUserId(userId).map(_.get.silver)
        told   <- renderer.sentScreens.map(_.last)
        _      <- state.enter(testUser, renderer)
        after  <- renderer.sentScreens.map(_.last.choices.map(_.id))
      } yield assertTrue(before.contains("WolfLore")) &&
              assertTrue(offer.text.contains("1500 серебра")) &&
              assertTrue(lore.wolfLore) && assertTrue(silver == 3500L) &&
              assertTrue(told.text.contains("Белые волки") && told.text.contains("растворяется")) &&
              assertTrue(!after.contains("WolfLore"))
    },

    test("кнопки про волка нет, пока герой его не встречал") {
      for {
        t <- makeStateWith(Nil, None, LoreData.empty, 5000L)
        (state, _, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(!screens.last.choices.map(_.id).contains("WolfLore"))
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
