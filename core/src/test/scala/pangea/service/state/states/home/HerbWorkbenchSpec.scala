package pangea.service.state.states.home

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.artifact.ArtifactKind
import pangea.model.home.{HomeData, HomeRates}
import pangea.model.item.{BrewKind, Item, MaterialKind}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestArtifactRepository, TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.test._
import zio.{Task, ZIO}

/** Верстак травника: травы на полки (из сумки и Живой сумки, до 50), котелок
  * варит, как магия Живой сумки, и тратит энергию дома, сваренное — на полку
  * отваров (до 25). */
object HerbWorkbenchSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String, data: (String, String)*): UserAction =
    UserAction("", Some((("action" -> key) +: data).map { case (k, v) => s""""$k":"$v"""" }.mkString("{", ",", "}")))

  private def herb(id: Long, kind: MaterialKind): Item = MaterialGenerator.item(kind).copy(id = id)
  private def bottle(id: Long, kind: BrewKind): Item  = BrewKind.item(kind).copy(id = id)
  /** Ромашка, календула, крапива — костоправный отвар, две бутылки. */
  private def boneSet(from: Long): List[Item] =
    List(herb(from, MaterialKind.Chamomile), herb(from + 1, MaterialKind.Calendula), herb(from + 2, MaterialKind.Nettle))

  private val townhouse = HomeData(tier = 2, metJunona = true, charges = 100)

  private def make(home: HomeData, items: List[Item] = Nil, living: List[Item] = Nil) =
    for {
      dao <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, StateType.HerbWorkbench))
      _   <- dao.writeHomeData(userId, home.asJson)
      inv  = TestInventoryRepository.withItems(items)
      art  = TestArtifactRepository.of(bag = TestArtifactRepository.artifact(ArtifactKind.LivingBag, tier = 1, items = living))
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (HerbWorkbenchState(dao, inv, TestItemRepository.make, c, Some(art)), dao, inv, art, r)

  private def homeOf(dao: TestHeroDao): Task[HomeData] =
    dao.readHomeData(userId).map(_.flatMap(_.as[HomeData].toOption).getOrElse(HomeData.empty))
  private def last(r: TestRenderer)  = r.sentScreens.map(_.last)
  private def texts(r: TestRenderer) = r.sentScreens.map(_.map(_.text).mkString("\n"))

  override def spec = suite("Верстак травника")(

    test("верстак — со второго дома: в домике его нет, и герой возвращается в дом") {
      for {
        t <- make(HomeData(tier = 1, metJunona = true))
        (state, _, _, _, r) = t
        out <- state.action(testUser, tap("WbMenu"), r)
      } yield assertTrue(out == StateType.Home)
    },

    test("«Сложить травы»: из сумки и из Живой сумки, только травы и не больше, чем держат полки") {
      val almostFull = townhouse.copy(herbs = (1L to 48L).map(i => herb(1000L + i, MaterialKind.Sage)).toList)
      for {
        t <- make(almostFull, items = List(herb(201L, MaterialKind.Nettle), bottle(202L, BrewKind.BoneSetter)),
                  living = List(herb(301L, MaterialKind.Chamomile), herb(302L, MaterialKind.Calendula)))
        (state, dao, inv, art, r) = t
        _    <- state.action(testUser, tap("WbPutHerbs"), r)
        home <- homeOf(dao)
        said <- texts(r)
      } yield assertTrue(home.herbs.size == HomeRates.HerbShelves) &&
              // из сумки ушла крапива, склянка осталась; из Живой сумки — ромашка, календула не влезла
              assertTrue(inv.snapshot.map(_.id) == List(202L)) &&
              assertTrue(art.snapshot.bag.items.data.map(_.id) == List(302L)) &&
              assertTrue(said.contains("На полки легло трав: 2.") && said.contains("Не поместилось: 1"))
    },

    test("трава с полки — в котелок (до девяти) или в сумку") {
      val home = townhouse.copy(herbs = boneSet(10L))
      for {
        t <- make(home)
        (state, dao, inv, _, r) = t
        _      <- state.action(testUser, tap("WbHerb", "id" -> "10", "p" -> "0"), r)
        screen <- last(r)
        _      <- state.action(testUser, tap("WbHerbPot", "id" -> "10", "p" -> "0"), r)
        _      <- state.action(testUser, tap("WbHerbBag", "id" -> "11", "p" -> "0"), r)
        after  <- homeOf(dao)
        // котелок полон — не кладётся
        t2 <- make(townhouse.copy(herbs = boneSet(20L), cauldron = (1L to 9L).map(i => herb(900L + i, MaterialKind.Sage)).toList))
        (state2, dao2, _, _, r2) = t2
        _      <- state2.action(testUser, tap("WbHerbPot", "id" -> "20", "p" -> "0"), r2)
        full   <- homeOf(dao2)
        said2  <- texts(r2)
      } yield assertTrue(screen.text.contains("на полках 1") && screen.choices.map(_.id) == List("WbHerbPot", "WbHerbBag", "WbMenu")) &&
              assertTrue(after.cauldron.map(_.id) == List(10L) && after.herbs.map(_.id) == List(12L)) &&
              assertTrue(inv.snapshot.map(_.id) == List(11L)) &&
              assertTrue(full.cauldron.size == 9 && full.herbs.size == 3 && said2.contains("В котелке нет места"))
    },

    test("котелок варит: тратит энергию дома, сваренное встаёт на полку отваров; «Зельевар» считает сваренное") {
      for {
        t <- make(townhouse.copy(cauldron = boneSet(30L)))
        (state, dao, _, _, r) = t
        _    <- state.action(testUser, tap("WbCook"), r)
        home <- homeOf(dao)
        said <- texts(r)
        lore <- pangea.service.state.HerbLore.readLore(dao, userId)
      } yield assertTrue(home.cauldron.isEmpty && home.charges == 99) &&
              assertTrue(home.brews.flatMap(_.brew) == List(BrewKind.BoneSetter, BrewKind.BoneSetter)) &&
              assertTrue(home.brews.forall(_.id > 0L)) &&
              assertTrue(said.contains("Варок: 1, бутылок: 2")) &&
              assertTrue(lore.brewed.contains(BrewKind.BoneSetter.entryName))
    },

    test("без энергии дома котелок не варит; не сходится рецепт — так и говорит") {
      for {
        t <- make(townhouse.copy(charges = 0, cauldron = boneSet(40L)))
        (state, dao, _, _, r) = t
        _     <- state.action(testUser, tap("WbCook"), r)
        dry   <- homeOf(dao)
        said  <- texts(r)
        t2 <- make(townhouse.copy(cauldron = List(herb(50L, MaterialKind.Sage))))
        (state2, dao2, _, _, r2) = t2
        _     <- state2.action(testUser, tap("WbCook"), r2)
        still <- homeOf(dao2)
        said2 <- texts(r2)
      } yield assertTrue(dry.cauldron.size == 3 && said.contains("Энергии дома нет")) &&
              assertTrue(still.charges == 100 && still.cauldron.size == 1 && said2.contains("Ничего не сварилось"))
    },

    test("«Вынуть всё»: травы — на полки, отвары — на полку отваров") {
      for {
        t <- make(townhouse.copy(cauldron = List(herb(60L, MaterialKind.Sage), bottle(61L, BrewKind.Schnapps))))
        (state, dao, _, _, r) = t
        _    <- state.action(testUser, tap("WbEmptyPot"), r)
        home <- homeOf(dao)
      } yield assertTrue(home.cauldron.isEmpty && home.herbs.map(_.id) == List(60L) && home.brews.map(_.id) == List(61L))
    },

    test("полка отваров: сложить из сумки и Живой сумки (до 25), бутылку — в сумку или в котелок основой") {
      for {
        t <- make(townhouse, items = List(bottle(70L, BrewKind.Schnapps), herb(71L, MaterialKind.Sage)),
                  living = List(bottle(72L, BrewKind.BoneSetter)))
        (state, dao, inv, art, r) = t
        _      <- state.action(testUser, tap("WbPutBrews"), r)
        put    <- homeOf(dao)
        _      <- state.action(testUser, tap("WbBrew", "id" -> "70", "p" -> "0"), r)
        screen <- last(r)
        _      <- state.action(testUser, tap("WbBrewPot", "id" -> "70", "p" -> "0"), r)
        _      <- state.action(testUser, tap("WbBrewBag", "id" -> "72", "p" -> "0"), r)
        after  <- homeOf(dao)
      } yield assertTrue(put.brews.map(_.id).sorted == List(70L, 72L)) &&
              assertTrue(art.snapshot.bag.items.data.isEmpty) &&
              assertTrue(screen.choices.map(_.id) == List("WbBrewBag", "WbBrewPot", "WbBrews")) &&
              assertTrue(after.cauldron.map(_.id) == List(70L) && after.brews.isEmpty) &&
              assertTrue(inv.snapshot.map(_.id).sorted == List(71L, 72L))
    }
  )
}
