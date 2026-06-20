package pangea.service.state.states

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.LootState.LootData
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestJournal, TestRenderer}
import zio.ZIO
import zio.test._

object LootStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def gear(name: String) =
    Item(-1L, name, 5L, Rarity.Green, ItemType.Helmet,
         attack = 0, accuracy = 0, concentration = 0, armor = 7, defence = 1, evasion = 0)

  private def trophy =
    Item(-1L, "«Голова» (Человек) ур-5", 5L, Rarity.Gray, ItemType.Trophy,
         attack = 0, accuracy = 0, concentration = 0, armor = 0, defence = 0, evasion = 0,
         race = Some("Human"))

  private def makeState(loot: LootData, canAdd: Boolean = true) =
    for {
      renderer <- TestRenderer.make
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(gold = 100L))
      _        <- heroDao.writeSceneData(userId, loot.asJson)
      journal  <- TestJournal.make
      content  <- ZIO.attempt(SceneContent.load())
      invRepo   = if (canAdd) TestInventoryRepository.accepting else TestInventoryRepository.full
      itemRepo  = TestItemRepository.make
      state     = LootState(heroDao, invRepo, itemRepo, journal, content)
    } yield (state, renderer, heroDao, invRepo)

  override def spec = suite("LootState")(

    test("enter → предметы кладутся в инвентарь с реальным id, золото начисляется") {
      for {
        t <- makeState(LootData(items = List(gear("Шлем"), trophy), golds = List(30L, 12L)))
        (state, renderer, heroDao, invRepo) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
        hero    <- heroDao.getHeroByUserId(userId)
        items    = invRepo.snapshot
      } yield assertTrue(items.size == 2) &&
              assertTrue(items.forall(_.id > 0L)) &&
              assertTrue(hero.exists(_.gold == 100L + 42L)) &&
              assertTrue(screens.exists(_.text.contains("Осматривая добычу"))) &&
              assertTrue(screens.exists(_.choices.map(_.id).contains("Continue")))
    },

    test("переполненный инвентарь → предмет теряется, сообщение «Инвентарь переполнен»") {
      for {
        t <- makeState(LootData(items = List(gear("Шлем")), golds = Nil), canAdd = false)
        (state, renderer, _, invRepo) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(invRepo.snapshot.isEmpty) &&
              assertTrue(screens.exists(_.text.contains("Инвентарь переполнен")))
    },

    test("пустой лут → сообщение «ничего ценного», без изменений золота") {
      for {
        t <- makeState(LootData(Nil, Nil))
        (state, renderer, heroDao, _) = t
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
        hero    <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(screens.exists(_.text.contains("ничего ценного"))) &&
              assertTrue(hero.exists(_.gold == 100L))
    },

    test("Continue → переход в Dungeon") {
      for {
        t <- makeState(LootData(Nil, Nil))
        (state, renderer, _, _) = t
        result <- state.action(testUser, tap("Continue"), renderer)
      } yield assertTrue(result == StateType.Dungeon)
    },

    test("неизвестный ввод → тоже уходит в Dungeon (лут уже забран)") {
      for {
        t <- makeState(LootData(Nil, Nil))
        (state, renderer, _, _) = t
        result <- state.action(testUser, UserAction("что угодно", None), renderer)
      } yield assertTrue(result == StateType.Dungeon)
    }
  )
}
