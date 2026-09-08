package pangea.service.state.states

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.GemGenerator
import pangea.model.item.{Gem, GemKind, Item, ItemType, Rarity}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Вставка камней в гнёзда. Главное правило: огонь и холод в одном оружии не
 *  уживаются и гасят друг друга, а одинаковые стихии спокойно стакаются. */
object SocketingStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  /** Нажатие на предмет-цель для вставки. */
  private def pickTarget(itemId: Long): UserAction =
    UserAction("", Some(s"""{"action":"SocketTarget_$itemId"}"""))

  private def weapon(sockets: List[Option[Gem]]) =
    Item(50L, "Меч", 1L, Rarity.Blue, ItemType.Weapon,
      attack = 10, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      sockets = sockets)

  /** Герой с этим оружием и камнем `gemKind` в сумке; сцена уже указывает на камень. */
  private def makeState(w: Item, gemKind: GemKind) = {
    val gemItem = GemGenerator.item(gemKind, 1).copy(id = 77L)
    for {
      dao      <- TestHeroDao.withHero(userId,
                    TestFixtures.hero(userId).copy(
                      equipment = TestFixtures.emptyEquipment.copy(weapon = w)))
      _        <- dao.writeSceneData(userId, SocketingState.Scene(gemItem.id).asJson)
      invRepo   = TestInventoryRepository.withItems(List(gemItem))
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (SocketingState(dao, invRepo, content), dao, invRepo, renderer)
  }

  private def weaponOf(dao: TestHeroDao) =
    dao.getHeroByUserId(userId).map(_.get.equipment.weapon)

  override def spec = suite("SocketingState")(

    test("камень встаёт в свободное гнездо и тратится из сумки") {
      for {
        t <- makeState(weapon(List(None)), GemKind.Ruby)
        (state, dao, invRepo, renderer) = t
        _ <- state.action(testUser, pickTarget(50L), renderer)
        w <- weaponOf(dao)
      } yield assertTrue(w.socketedGems.map(_.kind) == List(GemKind.Ruby)) &&
              assertTrue(invRepo.snapshot.isEmpty)
    },

    test("рубин в оружие с сапфиром — оба в пыль, гнездо освобождается") {
      for {
        t <- makeState(weapon(List(Some(Gem(GemKind.Sapphire, 1)), None)), GemKind.Ruby)
        (state, dao, invRepo, renderer) = t
        _       <- state.action(testUser, pickTarget(50L), renderer)
        w       <- weaponOf(dao)
        screens <- renderer.sentScreens
      } yield assertTrue(w.socketedGems.isEmpty) &&      // сапфира не стало
              assertTrue(invRepo.snapshot.isEmpty) &&    // рубин тоже израсходован
              assertTrue(screens.map(_.text).mkString.nonEmpty)
    },

    test("сапфир в оружие с рубином — симметрично") {
      for {
        t <- makeState(weapon(List(Some(Gem(GemKind.Ruby, 1)), None)), GemKind.Sapphire)
        (state, dao, _, renderer) = t
        _ <- state.action(testUser, pickTarget(50L), renderer)
        w <- weaponOf(dao)
      } yield assertTrue(w.socketedGems.isEmpty)
    },

    test("два рубина уживаются и стакаются — одинаковая стихия не аннигилирует") {
      for {
        t <- makeState(weapon(List(Some(Gem(GemKind.Ruby, 1)), None)), GemKind.Ruby)
        (state, dao, _, renderer) = t
        _ <- state.action(testUser, pickTarget(50L), renderer)
        w <- weaponOf(dao)
      } yield assertTrue(w.socketedGems.map(_.kind) == List(GemKind.Ruby, GemKind.Ruby))
    },

    test("нестихийный камень рядом со стихийным не гасит его") {
      for {
        t <- makeState(weapon(List(Some(Gem(GemKind.Ruby, 1)), None)), GemKind.Amethyst)
        (state, dao, _, renderer) = t
        _ <- state.action(testUser, pickTarget(50L), renderer)
        w <- weaponOf(dao)
      } yield assertTrue(w.socketedGems.map(_.kind) == List(GemKind.Ruby, GemKind.Amethyst))
    },

    test("молния и воздух друг другу не противоположны и спокойно соседствуют") {
      for {
        t <- makeState(weapon(List(Some(Gem(GemKind.Topaz, 1)), None)), GemKind.Diamond)
        (state, dao, _, renderer) = t
        _ <- state.action(testUser, pickTarget(50L), renderer)
        w <- weaponOf(dao)
      } yield assertTrue(w.socketedGems.size == 2)
    }
  )
}
