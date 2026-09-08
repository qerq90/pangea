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
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
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
    }
  )
}
