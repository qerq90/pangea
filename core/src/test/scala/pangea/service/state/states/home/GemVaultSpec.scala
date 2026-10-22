package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.{GemGenerator, MaterialGenerator}
import pangea.model.artifact.ArtifactKind
import pangea.model.home.{HomeData, HomeRates}
import pangea.model.item.{Gem, GemKind, Item, MaterialKind}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestArtifactRepository, TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
import zio.test._
import zio.{Task, ZIO}

/** Хранилище камней на острове: камни из сумки и Ларца (до 50), виды и
  * качества, по одному — в сумку; «Улучшить все» — тройки в камень выше, по
  * энергии дома за сплавление. */
object GemVaultSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def gem(id: Long, kind: GemKind, grade: Int): Item = GemGenerator.item(kind, grade).copy(id = id)

  private val island = HomeData(tier = 3, metJunona = true, charges = 100)

  private def make(home: HomeData, items: List[Item] = Nil, casket: List[Item] = Nil) =
    for {
      dao <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, StateType.GemVault))
      _   <- dao.writeHomeData(userId, home.asJson)
      inv  = TestInventoryRepository.withItems(items)
      art  = TestArtifactRepository.of(casket = TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1, items = casket))
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (GemVaultState(dao, inv, TestItemRepository.make, c, Some(art)), dao, inv, art, r)

  private def homeOf(dao: TestHeroDao): Task[HomeData] =
    dao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
  private def last(r: TestRenderer)  = r.sentScreens.map(_.last)
  private def texts(r: TestRenderer) = r.sentScreens.map(_.map(_.text).mkString("\n"))

  override def spec = suite("Хранилище камней")(

    test("хранилище — только на острове: в доме ниже — назад в дом; на острове в доме есть его кнопка") {
      for {
        t <- make(HomeData(tier = 2, metJunona = true))
        (state, _, _, _, r) = t
        out   <- state.action(testUser, tap("GvMenu"), r)
        // дом на острове: кнопка хранилища
        dao   <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, StateType.Home))
        _     <- dao.writeHomeData(userId, island.asJson)
        sch   <- TestScheduler.make
        c     <- ZIO.attempt(SceneContent.load())
        r2    <- TestRenderer.make
        _     <- HomeState(dao, sch, c).enter(testUser, r2)
        house <- last(r2)
      } yield assertTrue(out == StateType.Home) && assertTrue(house.choices.exists(_.id == "HomeGems"))
    },

    test("«Сложить камни»: из сумки и из Ларца, только камни и не больше пятидесяти") {
      val nearlyFull = island.copy(gems = (1L to 48L).map(i => gem(1000L + i, GemKind.Topaz, 1)).toList)
      for {
        t <- make(nearlyFull, items = List(gem(201L, GemKind.Ruby, 2), MaterialGenerator.item(MaterialKind.Sage).copy(id = 202L)),
                  casket = List(gem(301L, GemKind.Amethyst, 1), gem(302L, GemKind.Emerald, 1)))
        (state, dao, inv, art, r) = t
        _    <- state.action(testUser, tap("GvPutAll"), r)
        home <- homeOf(dao)
        said <- texts(r)
      } yield assertTrue(home.gems.size == HomeRates.GemVault) &&
              assertTrue(inv.snapshot.map(_.id) == List(202L)) &&
              assertTrue(art.snapshot.casket.items.data.map(_.id) == List(302L)) &&
              assertTrue(said.contains("В хранилище легло камней: 2.") && said.contains("Не поместилось: 1"))
    },

    test("вид камня: качества и их число; нажал на качество — один камень в сумку") {
      val home = island.copy(gems = List(gem(11L, GemKind.Ruby, 1), gem(12L, GemKind.Ruby, 1), gem(13L, GemKind.Ruby, 3), gem(14L, GemKind.Topaz, 2)))
      for {
        t <- make(home)
        (state, dao, inv, _, r) = t
        _     <- state.enter(testUser, r)
        menu  <- last(r)
        _     <- state.action(testUser, tap("GvKind", "k" -> GemKind.Ruby.entryName, "p" -> "0"), r)
        kind  <- last(r)
        _     <- state.action(testUser, tap("GvTake", "k" -> GemKind.Ruby.entryName, "g" -> "1", "p" -> "0"), r)
        after <- homeOf(dao)
      } yield assertTrue(menu.choices.filter(_.id == "GvKind").map(_.label) == List("Рубин — 3", "Топаз — 1")) &&
              assertTrue(kind.choices.filter(_.id == "GvTake").map(_.label) == List(
                s"${Gem.gradeName(GemKind.Ruby, 1)} — 2", s"${Gem.gradeName(GemKind.Ruby, 3)} — 1")) &&
              assertTrue(inv.snapshot.map(_.id) == List(12L) && after.gems.map(_.id) == List(11L, 13L, 14L))
    },

    test("«Улучшить все»: тройки — в камень качеством выше, энергия дома за каждое сплавление") {
      val home = island.copy(gems = (1L to 7L).map(i => gem(i, GemKind.Ruby, 1)).toList ++ List(gem(8L, GemKind.Topaz, 2)))
      for {
        t <- make(home)
        (state, dao, _, _, r) = t
        _     <- state.action(testUser, tap("GvUpgrade"), r)
        after <- homeOf(dao)
        said  <- texts(r)
        gems   = after.gems.flatMap(_.gem)
      } yield assertTrue(after.charges == 98) &&
              // семь рубинов: две тройки ушли в два рубина выше, один остался; топаз не тронут
              assertTrue(gems.count(_ == Gem(GemKind.Ruby, 2)) == 2 && gems.count(_ == Gem(GemKind.Ruby, 1)) == 1) &&
              assertTrue(gems.count(_ == Gem(GemKind.Topaz, 2)) == 1 && after.gems.forall(_.id > 0L)) &&
              assertTrue(said.contains("улучшений — 2"))
    },

    test("без энергии дома не улучшает; нечего улучшать — так и говорит") {
      for {
        t <- make(island.copy(charges = 0, gems = (1L to 3L).map(i => gem(i, GemKind.Ruby, 1)).toList))
        (state, dao, _, _, r) = t
        _     <- state.action(testUser, tap("GvUpgrade"), r)
        dry   <- homeOf(dao)
        said  <- texts(r)
        t2 <- make(island.copy(gems = List(gem(1L, GemKind.Ruby, 1), gem(2L, GemKind.Ruby, 2))))
        (state2, dao2, _, _, r2) = t2
        _     <- state2.action(testUser, tap("GvUpgrade"), r2)
        still <- homeOf(dao2)
        said2 <- texts(r2)
      } yield assertTrue(dry.gems.size == 3 && said.contains("Энергии дома нет")) &&
              assertTrue(still.charges == 100 && still.gems.size == 2 && said2.contains("Улучшать нечего"))
    }
  )
}
