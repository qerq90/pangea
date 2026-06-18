package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._

object DeathStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val anyAction: UserAction = UserAction("", Some("""{"action":"Respawn"}"""))

  private val testItem = Item(42L, "Меч судьбы", 1L, Rarity.Blue, ItemType.Weapon,
                              attack = 10, accuracy = 0, concentration = 0,
                              armor = 0, defence = 0, evasion = 0)

  private def makeState(hero: pangea.model.hero.Hero, items: List[Item] = Nil) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      invRepo   = TestInventoryRepository.withItems(items)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (DeathState(heroDao, invRepo, content), heroDao, invRepo, renderer)

  private val richHero = TestFixtures.hero(userId).copy(exp = 1000L, gold = 500L)

  override def spec = suite("DeathState")(

    test("enter → показывает экран смерти") {
      for {
        triple              <- makeState(TestFixtures.hero(userId))
        (state, _, _, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.choices.map(_.id).contains("Respawn"))
    },

    test("action → теряет 10% опыта и 50% золота, переходит в Rest") {
      for {
        triple                    <- makeState(richHero)
        (state, heroDao, _, renderer) = triple
        result                    <- state.action(testUser, anyAction, renderer)
        updated                   <- heroDao.getHeroByUserId(userId)
        screens                   <- renderer.sentScreens
        sceneData                 <- heroDao.readSceneData(userId)
      } yield assertTrue(result == StateType.Rest) &&
              assertTrue(updated.exists(_.exp == 900L)) &&
              assertTrue(updated.exists(_.gold == 250L)) &&
              assertTrue(screens.exists(_.text.contains("опыта"))) &&
              assertTrue(screens.exists(_.text.contains("золота"))) &&
              assertTrue(sceneData.flatMap(_.hcursor.get[Long]("restDurationMs").toOption).isDefined)
    },

    test("action → получает случайную именованную травму из списка") {
      for {
        triple                    <- makeState(richHero)
        (state, heroDao, _, renderer) = triple
        _                         <- state.action(testUser, anyAction, renderer)
        updated                   <- heroDao.getHeroByUserId(userId)
        screens                   <- renderer.sentScreens
      } yield assertTrue(updated.exists(_.traumaName.isDefined)) &&
              assertTrue(updated.exists(h => pangea.model.trauma.Trauma.byName(h.traumaName.get).isDefined)) &&
              assertTrue(updated.exists(_.traumaUntil.isDefined)) &&
              assertTrue(screens.exists(_.text.contains("травму")))
    },

    test("action без предметов → очищает бой, не падает") {
      for {
        triple                        <- makeState(richHero, items = Nil)
        (state, heroDao, _, renderer)  = triple
        result                        <- state.action(testUser, anyAction, renderer)
        battle                        <- heroDao.readActiveBattle(userId)
      } yield assertTrue(result == StateType.Rest) &&
              assertTrue(battle.isEmpty)
    },

    test("action с предметами в инвентаре → каждый предмет имеет 20% шанс дропа (не крашится)") {
      val manyItems = (1 to 20).map(i =>
        Item(i.toLong, s"Предмет $i", 1L, Rarity.Gray, ItemType.Weapon,
             attack = 1, accuracy = 0, concentration = 0, armor = 0, defence = 0, evasion = 0)
      ).toList
      for {
        triple                        <- makeState(richHero, items = manyItems)
        (state, _, invRepo, renderer)  = triple
        result                        <- state.action(testUser, anyAction, renderer)
      } yield assertTrue(result == StateType.Rest) &&
              assertTrue(invRepo.snapshot.length <= 20)
    },

    test("action с одним предметом → предмет может быть потерян или остаться") {
      for {
        triple                        <- makeState(richHero, items = List(testItem))
        (state, _, invRepo, renderer)  = triple
        result                        <- state.action(testUser, anyAction, renderer)
      } yield assertTrue(result == StateType.Rest) &&
              assertTrue(invRepo.snapshot.length == 0 || invRepo.snapshot.length == 1)
    }
  )
}
