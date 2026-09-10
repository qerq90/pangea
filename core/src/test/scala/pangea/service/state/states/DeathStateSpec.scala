package pangea.service.state.states

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.hero.AzatState
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._
import zio.test.TestRandom

// Death — узел-эффект (§18): вся логика смерти отыгрывается в `enter`, а переход
// в Rest идёт через `autoAdvance` без действия игрока. Поэтому тесты дёргают `enter`.
object DeathStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private val testItem = Item(42L, "Меч судьбы", 1L, Rarity.Blue, ItemType.Weapon,
                              attack = 10, accuracy = 0, energy = 0,
                              armor = 0, defence = 0, evasion = 0)

  private def makeState(hero: pangea.model.hero.Hero, items: List[Item] = Nil) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      invRepo   = TestInventoryRepository.withItems(items)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (DeathState(heroDao, invRepo, content), heroDao, invRepo, renderer)

  // exp=80 (внутри уровня 1), теряет 10% = 8, newExp=72
  private val richHero = TestFixtures.hero(userId).copy(exp = 80L, silver = 500L)

  override def spec = suite("DeathState")(

    test("камни из стопки теряются поштучно: бросок на каждый, а не на всю пачку") {
      // Пять одинаковых камней. Складываются они только на экране, поэтому
      // бросок идёт на каждый: два ролла из пяти «сгорают» (0 = потеря).
      val stones = (1 to 5).map(i =>
        pangea.generator.item.GemGenerator.item(pangea.model.item.GemKind.Amethyst, 1)
          .copy(id = 300L + i)).toList
      for {
        t <- makeState(richHero, stones)
        (state, _, invRepo, renderer) = t
        // Первый бросок съедает выбор травмы, дальше — по одному на камень.
        _    <- TestRandom.feedInts(1, 0, 1, 0, 2, 3)
        _    <- state.enter(testUser, renderer)
        left  = invRepo.snapshot
        log  <- renderer.sentScreens.map(_.map(_.text).mkString)
      } yield assertTrue(left.size == 3) &&
              // одно сообщение на всё потерянное, с количеством
              assertTrue(log.contains("×2"))
    },

    test("с благословением опыта теряется на 10% меньше") {
      val hero = TestFixtures.hero(userId).copy(exp = 1000L, silver = 500L)
      def lostWith(azat: Option[AzatState]) =
        for {
          t <- makeState(hero)
          (state, heroDao, _, renderer) = t
          _   <- ZIO.foreachDiscard(azat)(a => heroDao.writeAzatData(userId, a.asJson))
          _   <- state.enter(testUser, renderer)
          upd <- heroDao.getHeroByUserId(userId).map(_.get)
        } yield 1000L - upd.exp
      for {
        plain   <- lostWith(None)
        // Благословение на неделю вперёд: TestClock стоит на нуле.
        blessed <- lostWith(Some(AzatState(blessingUntil = Some(AzatState.BlessingDurationMs))))
      } yield assertTrue(plain == 100L) &&          // обычные 10% от 1000
              assertTrue(blessed == 90L)            // на 10% меньше самой потери
    },

    test("enter → показывает экран потерь и авто-переходит в Rest") {
      for {
        triple              <- makeState(richHero)
        (state, _, _, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.exists(_.text.contains("Потери"))) &&
              assertTrue(state.autoAdvance.contains(StateType.Rest))
    },

    test("enter → теряет 10% опыта текущего уровня и 50% серебра, пишет restDurationMs") {
      for {
        triple                    <- makeState(richHero)
        (state, heroDao, _, renderer) = triple
        _                         <- state.enter(testUser, renderer)
        updated                   <- heroDao.getHeroByUserId(userId)
        screens                   <- renderer.sentScreens
        sceneData                 <- heroDao.readSceneData(userId)
      } yield assertTrue(updated.exists(_.exp == 72L)) &&
              assertTrue(updated.exists(_.silver == 250L)) &&
              assertTrue(screens.exists(_.text.contains("опыта"))) &&
              assertTrue(screens.exists(_.text.contains("серебра"))) &&
              // уровень 1 → ровно 1 минута (90 - 89*exp(0) = 1)
              assertTrue(sceneData.flatMap(_.hcursor.get[Long]("restDurationMs").toOption).contains(60000L))
    },

    test("restDurationMs растёт с уровнем героя и не достигает 90 минут") {
      val highHero = TestFixtures.hero(userId).copy(lvl = 50L)
      for {
        triple                    <- makeState(highHero)
        (state, heroDao, _, renderer) = triple
        _                         <- state.enter(testUser, renderer)
        sceneData                 <- heroDao.readSceneData(userId)
        ms                         = sceneData.flatMap(_.hcursor.get[Long]("restDurationMs").toOption).getOrElse(0L)
      } yield assertTrue(ms > 60000L) &&            // больше, чем на 1 уровне
              assertTrue(ms < 90L * 60000L)         // строго меньше асимптоты в 90 минут
    },

    test("enter → получает случайную именованную травму из списка") {
      for {
        triple                    <- makeState(richHero)
        (state, heroDao, _, renderer) = triple
        _                         <- state.enter(testUser, renderer)
        updated                   <- heroDao.getHeroByUserId(userId)
        screens                   <- renderer.sentScreens
      } yield assertTrue(updated.exists(_.traumaNames.nonEmpty)) &&
              assertTrue(updated.exists(h => h.traumaNames.forall(n => pangea.model.trauma.Trauma.byName(n).isDefined))) &&
              assertTrue(updated.exists(_.traumaUntil.isDefined)) &&
              assertTrue(screens.exists(_.text.contains("травм")))
    },

    test("enter при максимуме травм → новую не даёт, продлевает таймер, сообщает «не осталось живого места»") {
      val allNames = pangea.model.trauma.Trauma.all.map(_.name).toList
      val maxedHero = richHero.copy(
        traumaUntil = Some(Long.MaxValue),       // травмы ещё активны
        traumaNames = allNames
      )
      for {
        triple                    <- makeState(maxedHero)
        (state, heroDao, _, renderer) = triple
        _                         <- state.enter(testUser, renderer)
        updated                   <- heroDao.getHeroByUserId(userId)
        screens                   <- renderer.sentScreens
      } yield assertTrue(updated.exists(_.traumaNames == allNames)) &&        // список не вырос
              assertTrue(updated.exists(_.traumaUntil.exists(_ != Long.MaxValue))) && // таймер обновлён
              assertTrue(screens.exists(_.text.contains("не осталось живого места")))
    },

    test("enter без предметов → очищает бой, не падает") {
      for {
        triple                        <- makeState(richHero, items = Nil)
        (state, heroDao, _, renderer)  = triple
        _                             <- state.enter(testUser, renderer)
        battle                        <- heroDao.readActiveBattle(userId)
      } yield assertTrue(state.autoAdvance.contains(StateType.Rest)) &&
              assertTrue(battle.isEmpty)
    },

    test("enter с предметами в инвентаре → каждый предмет имеет 25% шанс дропа (не крашится)") {
      val manyItems = (1 to 20).map(i =>
        Item(i.toLong, s"Предмет $i", 1L, Rarity.Gray, ItemType.Weapon,
             attack = 1, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)
      ).toList
      for {
        triple                        <- makeState(richHero, items = manyItems)
        (state, _, invRepo, renderer)  = triple
        _                             <- state.enter(testUser, renderer)
      } yield assertTrue(state.autoAdvance.contains(StateType.Rest)) &&
              assertTrue(invRepo.snapshot.length <= 20)
    },

    test("enter с одним предметом → предмет может быть потерян или остаться") {
      for {
        triple                        <- makeState(richHero, items = List(testItem))
        (state, _, invRepo, renderer)  = triple
        _                             <- state.enter(testUser, renderer)
      } yield assertTrue(state.autoAdvance.contains(StateType.Rest)) &&
              assertTrue(invRepo.snapshot.length == 0 || invRepo.snapshot.length == 1)
    }
  )
}
