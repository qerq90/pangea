package pangea.service.state.states.events.treasure

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.item.{ItemDetails, ItemType, MapZone}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
import zio.test._
import zio.{ZIO, durationInt}

/** Поход за сокровищем: 7 часов пути. В пути — «Персонаж» и «Уйти»: повернул
  * назад — сокровища нет, карта той же зоны снова в сумке (если влезла). Дошёл
  * — клад, и карта уже не возвращается. */
object TreasureHuntStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val zone = MapZone.values.head

  private def make(inv: TestInventoryRepository = TestInventoryRepository.accepting) =
    for {
      dao <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, StateType.TreasureHunt))
      _   <- dao.writeSceneData(userId, TreasureHuntProgress(0L, zone).asJson)
      sch <- TestScheduler.make
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (TreasureHuntState(dao, sch, c, inv, TestItemRepository.make), dao, inv, sch, r)

  private def maps(inv: TestInventoryRepository) =
    inv.snapshot.filter(i => i.itemType == ItemType.TreasureMap && i.details == ItemDetails.TreasureMap(zone))

  override def spec = suite("Поход за сокровищем")(

    test("поход — 7 часов: в пути видно, сколько осталось, и две кнопки — «Персонаж» и «Уйти»") {
      for {
        t <- make()
        (state, _, _, _, r) = t
        _      <- state.enter(testUser, r)
        start  <- r.sentScreens.map(_.last)
        _      <- TestClock.adjust(90.minutes)
        _      <- state.action(testUser, tap("Anything"), r)
        later  <- r.sentScreens.map(_.last)
      } yield assertTrue(TreasureHuntState.HuntDurationMs == 7L * 3600000L) &&
              assertTrue(start.text.contains("7 ч 0 мин") && later.text.contains("5 ч 30 мин")) &&
              assertTrue(start.choices.map(_.id) == List("OpenCharacter", "HuntLeave")) &&
              assertTrue(later.choices.map(_.id) == List("OpenCharacter", "HuntLeave"))
    },

    test("«Персонаж» в пути — меню персонажа, а назад оно вернёт в поход") {
      for {
        t <- make()
        (state, dao, _, _, r) = t
        out  <- state.action(testUser, tap("OpenCharacter"), r)
        back <- dao.readReturnState(userId)
      } yield assertTrue(out == StateType.HeroStats && back.contains(StateType.TreasureHunt))
    },

    test("«Уйти» — с подтверждением; повернул назад — город, сокровища нет, карта снова в сумке, таймер снят") {
      for {
        t <- make()
        (state, dao, inv, sch, r) = t
        _      <- TestClock.adjust(2.hours)
        stay   <- state.action(testUser, tap("HuntLeave"), r)
        ask    <- r.sentScreens.map(_.last)
        out    <- state.action(testUser, tap("HuntLeaveYes"), r)
        said   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        cancel <- sch.cancelled
        scene  <- dao.readSceneData(userId)
      } yield assertTrue(stay == StateType.TreasureHunt && ask.text.contains("5 ч 0 мин")) &&
              assertTrue(ask.choices.map(_.id) == List("HuntLeaveYes", "HuntLeaveNo")) &&
              assertTrue(out == StateType.GlobalMap && said.contains("Карта при вас")) &&
              assertTrue(maps(inv).size == 1 && maps(inv).forall(_.id > 0L)) &&
              assertTrue(cancel.contains((userId, TaskKind.TreasureHunt)) && scene.contains(io.circe.Json.Null))
    },

    test("«Идти дальше» — снова в пути; повернул с полной сумкой — карту приходится оставить") {
      for {
        t <- make()
        (state, _, _, _, r) = t
        _     <- state.action(testUser, tap("HuntLeave"), r)
        on    <- state.action(testUser, tap("HuntLeaveNo"), r)
        road  <- r.sentScreens.map(_.last)
        t2 <- make(TestInventoryRepository.full)
        (state2, _, full, _, r2) = t2
        out   <- state2.action(testUser, tap("HuntLeaveYes"), r2)
        said  <- r2.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(on == StateType.TreasureHunt && road.text.contains("Вы ещё в пути")) &&
              assertTrue(out == StateType.GlobalMap && maps(full).isEmpty && said.contains("карту пришлось оставить"))
    },

    test("время вышло, пока герой был в меню, — он на месте: «Копать» выкапывает клад") {
      for {
        t <- make()
        (state, _, _, _, r) = t
        _      <- TestClock.adjust(8.hours)
        _      <- state.enter(testUser, r)
        here   <- r.sentScreens.map(_.last)
        out    <- state.action(testUser, tap("HuntDone"), r)
      } yield assertTrue(here.text.contains("Вы на месте") && here.choices.map(_.id) == List("HuntDone")) &&
              assertTrue(out == StateType.Loot)
    },

    test("дошёл до клада — «Уйти» уже не поворачивает: сокровище, а карта не возвращается") {
      for {
        t <- make()
        (state, _, inv, _, r) = t
        _    <- TestClock.adjust(7.hours)
        out  <- state.action(testUser, tap("HuntLeave"), r)
        said <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(out == StateType.Loot && said.contains("выкопали спрятанное сокровище")) &&
              assertTrue(maps(inv).isEmpty)
    }
  )
}
