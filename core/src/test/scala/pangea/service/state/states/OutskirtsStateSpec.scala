package pangea.service.state.states

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.TreasureMapGenerator
import pangea.model.item.{Item, MapZone}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.OutskirtsState.OutskirtsScene
import pangea.service.state.{ItemMenu, UserAction}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer, TestScheduler}
import zio.ZIO
import zio.test._

/** Выбор карты клада перед походом. Карт в сумке бывает сколько угодно, а на
 *  клавиатуру ВК влезает 10 рядов — поэтому список постраничный, как инвентарь. */
object OutskirtsStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def pickMap(itemId: Long): UserAction = tap(s"${OutskirtsState.MapPickPrefix}$itemId")

  /** `n` целых карт с id 1..n; зоны по кругу, чтобы имена различались. */
  private def maps(n: Int): List[Item] =
    (1 to n).toList.map { i =>
      TreasureMapGenerator.full(MapZone.values(i % MapZone.values.size)).copy(id = i.toLong)
    }

  private def makeState(items: List[Item]) =
    for {
      dao       <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
      invRepo    = TestInventoryRepository.withItems(items)
      scheduler <- TestScheduler.make
      renderer  <- TestRenderer.make
      content   <- ZIO.attempt(SceneContent.load())
    } yield (OutskirtsState(dao, invRepo, scheduler, content), dao, invRepo, scheduler, renderer)

  private def targetIds(ids: List[String]) = ids.filter(_.startsWith(OutskirtsState.MapPickPrefix))

  override def spec = suite("OutskirtsState")(

    test("без карт — только сообщение и «Назад»") {
      for {
        t <- makeState(Nil)
        (state, _, _, _, renderer) = t
        _      <- state.action(testUser, tap("DepartTreasure"), renderer)
        screen <- renderer.sentScreens.map(_.last)
      } yield assertTrue(screen.text.contains("нет ни одной карты")) &&
              assertTrue(screen.choices.map(_.id) == List("BackToOutskirts"))
    },

    test("когда карта одна, стрелок нет — только «Назад»") {
      for {
        t <- makeState(maps(1))
        (state, _, _, _, renderer) = t
        _      <- state.action(testUser, tap("DepartTreasure"), renderer)
        screen <- renderer.sentScreens.map(_.last)
      } yield assertTrue(screen.choices.map(_.id) == List("HuntPick_1", "BackToOutskirts")) &&
              assertTrue(!screen.text.contains("(1/"))
    },

    test("20 карт не лезут на клавиатуру ВК — режем на страницы по 8") {
      for {
        t <- makeState(maps(20))
        (state, _, _, _, renderer) = t
        _      <- state.action(testUser, tap("DepartTreasure"), renderer)
        screen <- renderer.sentScreens.map(_.last)
        rows    = screen.choices.flatMap(_.row).distinct.size
      } yield assertTrue(targetIds(screen.choices.map(_.id)).size == ItemMenu.DefaultPageSize) &&
              assertTrue(rows <= 10) && // жёсткий лимит клавиатуры ВК
              assertTrue(screen.text.contains("(1/3)")) &&
              // с первой страницы листать можно только вперёд
              assertTrue(screen.choices.map(_.id).contains("HuntNext")) &&
              assertTrue(!screen.choices.map(_.id).contains("HuntPrev"))
    },

    test("«След.» листает до конца, «Пред.» возвращает обратно") {
      for {
        t <- makeState(maps(20))
        (state, _, _, _, renderer) = t
        _      <- state.action(testUser, tap("DepartTreasure"), renderer)
        first  <- renderer.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("HuntNext"), renderer)
        second <- renderer.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("HuntNext"), renderer)
        third  <- renderer.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("HuntPrev"), renderer)
        back   <- renderer.sentScreens.map(_.last)
      } yield assertTrue(third.text.contains("(3/3)")) &&
              assertTrue(targetIds(third.choices.map(_.id)).size == 20 - 2 * ItemMenu.DefaultPageSize) &&
              // на последней странице вперёд листать некуда
              assertTrue(!third.choices.map(_.id).contains("HuntNext")) &&
              assertTrue(third.choices.map(_.id).contains("HuntPrev")) &&
              // страницы не пересекаются, а «Пред.» честно возвращает вторую
              assertTrue(targetIds(first.choices.map(_.id))
                           .intersect(targetIds(second.choices.map(_.id))).isEmpty) &&
              assertTrue(targetIds(back.choices.map(_.id)) == targetIds(second.choices.map(_.id)))
    },

    test("«След.» с последней страницы никуда не уезжает") {
      for {
        t <- makeState(maps(9)) // ровно две страницы
        (state, dao, _, _, renderer) = t
        _      <- state.action(testUser, tap("DepartTreasure"), renderer)
        _      <- state.action(testUser, tap("HuntNext"), renderer)
        _      <- state.action(testUser, tap("HuntNext"), renderer)
        screen <- renderer.sentScreens.map(_.last)
        scene  <- dao.readSceneData(userId).map(_.flatMap(_.as[OutskirtsScene].toOption))
      } yield assertTrue(screen.text.contains("(2/2)")) &&
              assertTrue(scene.flatMap(_.page).contains(1))
    },

    test("карта со второй страницы уходит в поход, страница выбор не ломает") {
      for {
        t <- makeState(maps(20))
        (state, dao, invRepo, scheduler, renderer) = t
        _       <- state.action(testUser, tap("DepartTreasure"), renderer)
        _       <- state.action(testUser, tap("HuntNext"), renderer)
        confirm <- state.action(testUser, pickMap(9L), renderer)
        screen  <- renderer.sentScreens.map(_.last)
        scene   <- dao.readSceneData(userId).map(_.flatMap(_.as[OutskirtsScene].toOption))
        result  <- state.action(testUser, tap("ConfirmDepart"), renderer)
        tasks   <- scheduler.scheduled
      } yield assertTrue(confirm == StateType.Outskirts) &&
              assertTrue(screen.choices.map(_.id).toSet == Set("ConfirmDepart", "CancelDepart")) &&
              // выбранная карта запомнена вместе со страницей, с которой её взяли
              assertTrue(scene.flatMap(_.mapId).contains(9L)) &&
              assertTrue(scene.flatMap(_.page).contains(1)) &&
              assertTrue(result == StateType.TreasureHunt) &&
              assertTrue(!invRepo.snapshot.exists(_.id == 9L)) &&
              assertTrue(tasks.exists(_.kind == TaskKind.TreasureHunt))
    },

    test("«Уйти» с подтверждения возвращает на ту же страницу") {
      for {
        t <- makeState(maps(20))
        (state, _, _, _, renderer) = t
        _      <- state.action(testUser, tap("DepartTreasure"), renderer)
        _      <- state.action(testUser, tap("HuntNext"), renderer)
        _      <- state.action(testUser, pickMap(9L), renderer)
        _      <- state.action(testUser, tap("CancelDepart"), renderer)
        screen <- renderer.sentScreens.map(_.last)
      } yield assertTrue(screen.text.contains("(2/3)")) &&
              assertTrue(targetIds(screen.choices.map(_.id)).contains("HuntPick_9"))
    },

    test("исчезнувшая карта не отправляет в поход, а возвращает к списку") {
      for {
        t <- makeState(maps(3))
        (state, dao, _, scheduler, renderer) = t
        // Сцена ссылается на карту, которой в сумке нет.
        _      <- dao.writeSceneData(userId, OutskirtsScene(mapId = Some(99L)).asJson)
        result <- state.action(testUser, tap("ConfirmDepart"), renderer)
        screen <- renderer.sentScreens.map(_.last)
        tasks  <- scheduler.scheduled
      } yield assertTrue(result == StateType.Outskirts) &&
              assertTrue(screen.text.contains("Какое сокровище ищем")) &&
              assertTrue(tasks.isEmpty)
    }
  )
}
