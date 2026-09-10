package pangea.service.state.states

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.GemGenerator
import pangea.model.hero.Equipment
import pangea.model.item.{Gem, GemKind, Item, ItemType, MaterialKind, Rarity}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.{ItemMenu, UiScene, UserAction}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Вставка камней в гнёзда. Главное правило: огонь и холод в одном оружии не
 *  уживаются и гасят друг друга, а одинаковые стихии спокойно стакаются. Плюс
 *  постраничный выбор цели: гнёзда бывают во всех 14 надетых предметах, а на
 *  клавиатуру ВК влезает меньше. */
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
  private def makeState(w: Item, gemKind: GemKind) =
    withEquipment(TestFixtures.emptyEquipment.copy(weapon = w), gemKind)

  /** Аннигиляция камнем заданного грейда. */
  private def makeStateWithGrade(w: Item, gemKind: GemKind, grade: Int) = {
    val gemItem = GemGenerator.item(gemKind, grade).copy(id = 77L)
    for {
      dao      <- TestHeroDao.withHero(userId,
                    TestFixtures.hero(userId).copy(
                      equipment = TestFixtures.emptyEquipment.copy(weapon = w)))
      _        <- dao.writeSceneData(userId, Json.obj(UiScene.Socketing -> SocketingState.Scene(gemItem.id).asJson))
      invRepo   = TestInventoryRepository.withItems(List(gemItem))
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (SocketingState(dao, invRepo, content), dao, invRepo, renderer)
  }

  /** Сумка забита: вместимость ровно равна числу лежащих в ней предметов. */
  private def fullBagState(w: Item, gemKind: GemKind) = {
    val gemItem = GemGenerator.item(gemKind, 1).copy(id = 77L)
    val filler  = (1 to 19).map(i => GemGenerator.item(GemKind.Amethyst, 1).copy(id = 100L + i)).toList
    for {
      dao      <- TestHeroDao.withHero(userId,
                    TestFixtures.hero(userId).copy(
                      equipment = TestFixtures.emptyEquipment.copy(weapon = w)))
      _        <- dao.writeSceneData(userId, Json.obj(UiScene.Socketing -> SocketingState.Scene(gemItem.id).asJson))
      invRepo   = TestInventoryRepository.withItems(gemItem :: filler)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (SocketingState(dao, invRepo, content), dao, invRepo, renderer)
  }

  /** То же, но со всей экипировкой сразу — для проверки страниц. */
  private def withEquipment(eq: Equipment, gemKind: GemKind) = {
    val gemItem = GemGenerator.item(gemKind, 1).copy(id = 77L)
    for {
      dao      <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(equipment = eq))
      _        <- dao.writeSceneData(userId, Json.obj(UiScene.Socketing -> SocketingState.Scene(gemItem.id).asJson))
      invRepo   = TestInventoryRepository.withItems(List(gemItem))
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (SocketingState(dao, invRepo, content), dao, invRepo, renderer)
  }

  /** Предмет нужного типа с одним свободным гнездом. */
  private def socketable(id: Long, itemType: ItemType) =
    Item(id, "Предмет", 1L, Rarity.Blue, itemType,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      sockets = List(None))

  /** Все 14 слотов заняты предметами с гнездом — максимум возможных целей. */
  private val fullEquipment: Equipment = {
    val types = List(
      ItemType.Helmet, ItemType.ShoulderPads, ItemType.ChestPlate, ItemType.Bracelets,
      ItemType.Gloves, ItemType.Pants, ItemType.Boots, ItemType.Amulet,
      ItemType.Ring, ItemType.Ring, ItemType.Belt, ItemType.Flask,
      ItemType.Weapon, ItemType.AdditionalWeapon)
    val i = types.zipWithIndex.map { case (t, n) => socketable(100L + n, t) }
    Equipment(i(0), i(1), i(2), i(3), i(4), i(5), i(6), i(7), i(8), i(9), i(10), i(11), i(12), i(13))
  }

  /** Нажатие навигационной кнопки. */
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

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
        left     = invRepo.snapshot
      } yield assertTrue(w.socketedGems.isEmpty) &&           // сапфира не стало
              assertTrue(!left.exists(_.gem.isDefined)) &&    // рубин тоже израсходован
              assertTrue(screens.map(_.text).mkString.nonEmpty)
    },

    test("от вспышки остаётся ровно две горсти пыли — по одной за каждый камень") {
      for {
        t <- makeState(weapon(List(Some(Gem(GemKind.Sapphire, 1)), None)), GemKind.Ruby)
        (state, _, invRepo, renderer) = t
        _       <- state.action(testUser, pickTarget(50L), renderer)
        dusts    = invRepo.snapshot.flatMap(_.material)
        screens <- renderer.sentScreens
      } yield assertTrue(dusts.size == 2) &&
              assertTrue(dusts.toSet == Set[MaterialKind](MaterialKind.RubyDust, MaterialKind.SapphireDust)) &&
              assertTrue(screens.last.text.contains("Рубиновая пыль")) &&
              assertTrue(screens.last.text.contains("Сапфировая пыль"))
    },

    test("грейд на вспышку не влияет: идеальный камень даёт ту же одну горсть") {
      for {
        t <- makeStateWithGrade(weapon(List(Some(Gem(GemKind.Sapphire, 5)), None)), GemKind.Ruby, grade = 5)
        (state, _, invRepo, renderer) = t
        _     <- state.action(testUser, pickTarget(50L), renderer)
        dusts  = invRepo.snapshot.flatMap(_.material)
      } yield assertTrue(dusts.size == 2) &&
              assertTrue(dusts.count(_ == MaterialKind.RubyDust) == 1) &&
              assertTrue(dusts.count(_ == MaterialKind.SapphireDust) == 1)
    },

    test("места в сумке нет — камни всё равно гибнут, но пыль не достаётся") {
      // Сумка забита под завязку: вставляемый камень освободит одну ячейку,
      // а под две горсти нужно две.
      for {
        t <- fullBagState(weapon(List(Some(Gem(GemKind.Sapphire, 1)), None)), GemKind.Ruby)
        (state, dao, invRepo, renderer) = t
        _       <- state.action(testUser, pickTarget(50L), renderer)
        w       <- weaponOf(dao)
        screens <- renderer.sentScreens
      } yield assertTrue(w.socketedGems.isEmpty) &&
              assertTrue(invRepo.snapshot.flatMap(_.material).isEmpty) &&
              assertTrue(screens.last.text.contains("не нашлось места"))
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

    // ── Страницы ─────────────────────────────────────────────────────────────

    test("14 целей не лезут на клавиатуру ВК — режем на страницы по 8") {
      for {
        t <- withEquipment(fullEquipment, GemKind.Ruby)
        (state, _, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        screen  <- renderer.sentScreens.map(_.last)
        targets  = screen.choices.filter(_.id.startsWith(SocketingState.TargetPrefix))
        rows     = screen.choices.flatMap(_.row).distinct.size
      } yield assertTrue(targets.size == ItemMenu.DefaultPageSize) &&
              assertTrue(rows <= 10) && // жёсткий лимит клавиатуры ВК
              assertTrue(screen.text.contains("(1/2)")) &&
              // с первой страницы листать можно только вперёд
              assertTrue(screen.choices.map(_.id).contains("SocketNext")) &&
              assertTrue(!screen.choices.map(_.id).contains("SocketPrev"))
    },

    test("«След.» показывает остаток, «Пред.» возвращает обратно") {
      def targetIds(ids: List[String]) = ids.filter(_.startsWith(SocketingState.TargetPrefix))
      for {
        t <- withEquipment(fullEquipment, GemKind.Ruby)
        (state, _, _, renderer) = t
        _      <- state.enter(testUser, renderer)
        _      <- state.action(testUser, tap("SocketNext"), renderer)
        second <- renderer.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("SocketPrev"), renderer)
        first  <- renderer.sentScreens.map(_.last)
      } yield assertTrue(targetIds(second.choices.map(_.id)).size == 14 - ItemMenu.DefaultPageSize) &&
              assertTrue(second.text.contains("(2/2)")) &&
              // на последней странице вперёд листать некуда
              assertTrue(!second.choices.map(_.id).contains("SocketNext")) &&
              assertTrue(second.choices.map(_.id).contains("SocketPrev")) &&
              // страницы не пересекаются, а «Пред.» честно возвращает первую
              assertTrue(targetIds(first.choices.map(_.id))
                           .intersect(targetIds(second.choices.map(_.id))).isEmpty) &&
              assertTrue(first.text.contains("(1/2)"))
    },

    test("камень вставляется в предмет со второй страницы") {
      for {
        t <- withEquipment(fullEquipment, GemKind.Ruby)
        (state, dao, invRepo, renderer) = t
        _      <- state.enter(testUser, renderer)
        _      <- state.action(testUser, tap("SocketNext"), renderer)
        // 113 — доп. оружие, последняя цель списка
        result <- state.action(testUser, pickTarget(113L), renderer)
        hero   <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(result == StateType.Inventory) &&
              assertTrue(hero.equipment.additionalWeapon.socketedGems.map(_.kind) == List(GemKind.Ruby)) &&
              assertTrue(invRepo.snapshot.isEmpty)
    },

    test("когда цель одна, стрелок нет — только «Назад»") {
      for {
        t <- makeState(weapon(List(None)), GemKind.Ruby)
        (state, _, _, renderer) = t
        _      <- state.enter(testUser, renderer)
        screen <- renderer.sentScreens.map(_.last)
      } yield assertTrue(screen.choices.map(_.id) == List("SocketTarget_50", "BackFromSocketing")) &&
              assertTrue(!screen.text.contains("/"))
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
