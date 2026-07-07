package pangea.service.state.states.tavern

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.item.{Item, ItemDetails, ItemType, Rarity, TrophyKind}
import pangea.model.monster.Race
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
      attack = 0, accuracy = 0, concentration = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Trophy(race.entryName, kind))

  private def makeState(items: List[Item], active: Option[Race]) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, state = StateType.Innkeeper))
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

    test("TurnInQuest забирает ПЕРВЫЙ подходящий трофей по порядку инвентаря") {
      for {
        t <- makeState(
               List(trophy(1L, Race.Elf, 1L), trophy(2L, Race.Orc, 5L), trophy(3L, Race.Orc, 9L)),
               active = Some(Race.Orc))
        (state, _, invRepo, renderer) = t
        _ <- state.action(testUser, tap("TurnInQuest"), renderer)
      } yield assertTrue(invRepo.snapshot.map(_.id) == List(1L, 3L)) // забрал id=2 (первый Орк)
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
    }
  )
}
