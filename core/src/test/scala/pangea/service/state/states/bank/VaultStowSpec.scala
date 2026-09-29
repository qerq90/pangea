package pangea.service.state.states.bank

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.{GemGenerator, MaterialGenerator, TreasureMapGenerator}
import pangea.model.artifact.ArtifactKind
import pangea.model.bank.{StowGroup, StowSettings}
import pangea.model.hero.Hero
import pangea.model.item.{BrewKind, GemKind, Item, ItemDetails, ItemType, MapZone, MaterialKind, QuestItemKind, Rarity, TrophyKind}
import pangea.model.rune.{RuneStone, RuneStoneSize}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.sender.vk.VkRenderer
import pangea.service.state.UserAction
import pangea.test._
import zio.ZIO
import zio.test._

/** «Положить всё» в хранилище Торгового дома: что уходит в ячейку, что
  * остаётся при герое и как это настраивается. */
object VaultStowSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def content = ZIO.attempt(SceneContent.load())

  private def hero(silver: Long = 0L): Hero = TestFixtures.hero(userId).copy(silver = silver)

  // ── Предметы всех родов ───────────────────────────────────────────────────
  private def herb(id: Long)     = MaterialGenerator.item(MaterialKind.Chamomile).copy(id = id)
  private def dust(id: Long)     = MaterialGenerator.item(MaterialKind.RubyDust).copy(id = id)
  private def material(id: Long) = MaterialGenerator.item(MaterialKind.Mithril).copy(id = id)
  private def gem(id: Long)      = GemGenerator.item(GemKind.Ruby, 1).copy(id = id)
  private def brew(id: Long)     = BrewKind.item(BrewKind.Schnapps).copy(id = id)
  private def smallRune(id: Long) = RuneStone.item(RuneStone.all.head, RuneStoneSize.Small).copy(id = id)
  private def bigRune(id: Long)   = RuneStone.item(RuneStone.all.head, RuneStoneSize.Big).copy(id = id)
  private def quest(id: Long)     = QuestItemKind.item(QuestItemKind.MarisaLetter).copy(id = id)
  private def map(id: Long)       = TreasureMapGenerator.full(MapZone.Kinet).copy(id = id)
  private def halfMap(id: Long)   = TreasureMapGenerator.create(10L, half = true).copy(id = id)

  private def gear(id: Long): Item =
    Item(id, "🔵 Шлем", 10L, Rarity.Blue, ItemType.Helmet,
      attack = 0, accuracy = 0, energy = 0, armor = 5, defence = 0, evasion = 0,
      details = ItemDetails.Plain)

  private def trophy(id: Long): Item =
    Item(id, "Клык", 10L, Rarity.Gray, ItemType.Trophy,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Trophy(pangea.model.monster.Race.Orc.entryName, TrophyKind.Fang))

  private def vaultState(
    items:     List[Item] = Nil,
    cells:     Int        = 1,
    h:         Hero       = hero(),
    artifacts: Option[TestArtifactRepository] = None,
    settings:  Option[StowSettings] = None
  ) =
    for {
      dao  <- TestHeroDao.withHero(userId, h)
      _    <- ZIO.foreachDiscard(settings)(s => dao.writeVaultStow(userId, s.asJson))
      inv   = TestInventoryRepository.withItems(items)
      bank  = TestBankRepository.withCells(cells)
      r    <- TestRenderer.make
      c    <- content
    } yield (BankVaultState(dao, inv, bank, c, artifacts), dao, inv, bank, r)

  /** Настройка, где включено только перечисленное. */
  private def only(groups: StowGroup*): StowSettings =
    StowGroup.values.foldLeft(StowSettings.default) { (s, g) =>
      if (groups.contains(g) == s.on(g)) s else s.toggle(g)
    }

  override def spec = suite("Положить всё")(

    test("каждая вещь знает свою полку, сюжетная — ничью") {
      val s = StowSettings.default
      assertTrue(s.groupOf(herb(1)).contains(StowGroup.Herbs)) &&
      assertTrue(s.groupOf(gem(2)).contains(StowGroup.Gems)) &&
      assertTrue(s.groupOf(gear(3)).contains(StowGroup.Gear)) &&
      assertTrue(s.groupOf(trophy(4)).contains(StowGroup.Trophies)) &&
      assertTrue(s.groupOf(brew(5)).contains(StowGroup.Brews)) &&
      // пыль — тоже материал, но полка у неё своя, и руны так же
      assertTrue(s.groupOf(dust(6)).contains(StowGroup.Dust)) &&
      assertTrue(s.groupOf(material(7)).contains(StowGroup.Materials)) &&
      assertTrue(s.groupOf(smallRune(8)).contains(StowGroup.SmallRunes)) &&
      assertTrue(s.groupOf(bigRune(9)).contains(StowGroup.BigRunes)) &&
      // карта и половинка — под одним переключателем
      assertTrue(s.groupOf(map(10)).contains(StowGroup.Maps)) &&
      assertTrue(s.groupOf(halfMap(11)).contains(StowGroup.Maps)) &&
      assertTrue(s.groupOf(quest(12)).isEmpty)
    },

    test("настройка переживает запись в jsonb, а пустая запись — это настройка по умолчанию") {
      val flipped = StowSettings.default.toggle(StowGroup.Gear).toggle(StowGroup.Silver)
      val back    = flipped.asJson.as[StowSettings]
      val fresh   = Json.obj().as[StowSettings]
      assertTrue(back.contains(flipped)) &&
      assertTrue(!flipped.gear && flipped.silver) &&
      assertTrue(fresh.contains(StowSettings.default)) &&
      // по умолчанию сумка уходит целиком, а кошелёк и чужие хранилища — нет
      assertTrue(StowGroup.itemGroups.forall(StowSettings.default.on)) &&
      assertTrue(!StowSettings.default.silver && StowGroup.storageGroups.forall(g => !StowSettings.default.on(g)))
    },

    test("экран настройки: все переключатели по трое в ряд, и клавиатура ВК это держит") {
      for {
        t <- vaultState()
        (state, _, _, _, r) = t
        _      <- state.action(testUser, tap("VaultStowSettings"), r)
        screen <- r.sentScreens.map(_.last)
        rows    = screen.choices.flatMap(_.row).distinct
        byRow   = screen.choices.groupBy(_.row).values.map(_.size)
      } yield assertTrue(screen.choices.count(_.id.startsWith("VaultStow_")) == StowGroup.values.size) &&
              assertTrue(screen.choices.size == StowGroup.values.size + 1) &&
              assertTrue(rows.size <= VkRenderer.MaxRows && byRow.max <= VkRenderer.MaxButtonsPerRow) &&
              // четырнадцать переключателей по трое — пять рядов, шестой под «Назад»
              assertTrue(byRow.max == 3 && rows.size == 6 && screen.choices.last.id == "VaultMenu")
    },

    test("после щелчка — строчка о том, что поменялось, а не всё объяснение заново") {
      for {
        t <- vaultState()
        (state, _, _, _, r) = t
        _      <- state.action(testUser, tap("VaultStowSettings"), r)
        opened <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("VaultStow_gems"), r)
        off    <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("VaultStow_gems"), r)
        on     <- r.sentScreens.map(_.last)
      } yield assertTrue(opened.text.contains("Отметьте, что уходит в ячейку")) &&
              assertTrue(off.text == "❌ Теперь не перекладывается: 💎 Камни") &&
              assertTrue(on.text == "✅ Теперь перекладывается: 💎 Камни") &&
              // кнопки остаются: щёлкать дальше можно там же
              assertTrue(on.choices.size == opened.choices.size)
    },

    test("переключатель гаснет и загорается, и это запоминается") {
      for {
        t <- vaultState()
        (state, dao, _, _, r) = t
        _     <- state.action(testUser, tap("VaultStow_gear"), r)
        off   <- dao.readVaultStow(userId).map(_.flatMap(_.as[StowSettings].toOption).get)
        shown <- r.sentScreens.map(_.last.choices.find(_.id == "VaultStow_gear").get)
        _     <- state.action(testUser, tap("VaultStow_gear"), r)
        on    <- dao.readVaultStow(userId).map(_.flatMap(_.as[StowSettings].toOption).get)
      } yield assertTrue(!off.gear && on.gear) &&
              assertTrue(shown.label.contains("❌") && shown.color == pangea.engine.ChoiceColor.Negative)
    },

    test("кладёт отмеченное, а невключённое и сюжетное оставляет при герое") {
      for {
        t <- vaultState(
               items = List(herb(1), gear(2), quest(3), trophy(4)),
               settings = Some(only(StowGroup.Herbs, StowGroup.Trophies)))
        (state, _, inv, bank, r) = t
        _    <- state.action(testUser, tap("VaultStowAll"), r)
        said <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(bank.itemsSnapshot.map(_.id).sorted == List(1L, 4L)) &&
              assertTrue(inv.snapshot.map(_.id).sorted == List(2L, 3L)) &&
              assertTrue(said.contains("Положено вещей: 2") && said.contains("травы 1") && said.contains("трофеи 1"))
    },

    test("серебро уходит только с включённым переключателем — и не больше, чем влезает") {
      def run(withSilver: Boolean) =
        for {
          t <- vaultState(h = hero(silver = 250_000L),
                 settings = Some(if (withSilver) only(StowGroup.Silver) else only()))
          (state, dao, _, bank, r) = t
          _    <- state.action(testUser, tap("VaultStowAll"), r)
          left <- dao.getHeroByUserId(userId).map(_.get.silver)
        } yield (bank.silverSnapshot, left)
      for {
        off <- run(withSilver = false)
        on  <- run(withSilver = true)
      } yield assertTrue(off == (0L, 250_000L)) &&
              // одна ячейка держит сто тысяч, остальное остаётся на руках
              assertTrue(on == (100_000L, 150_000L))
    },

    test("с включёнными хранилищами выгребает ларец и живую сумку, шкаф не трогает") {
      val repo = TestArtifactRepository.of(
        casket = TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 1, items = List(gem(11), gem(12))),
        bag    = TestArtifactRepository.artifact(ArtifactKind.LivingBag, tier = 1, items = List(herb(13))),
        wardrobe = TestArtifactRepository.artifact(ArtifactKind.Wardrobe, tier = 1, items = List(gear(14))))
      for {
        t <- vaultState(artifacts = Some(repo),
               settings = Some(only(StowGroup.Casket, StowGroup.LivingBag)))
        (state, _, _, bank, r) = t
        _    <- state.action(testUser, tap("VaultStowAll"), r)
        said <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        left  = repo.snapshot
      } yield assertTrue(bank.itemsSnapshot.map(_.id).sorted == List(11L, 12L, 13L)) &&
              assertTrue(left.of(ArtifactKind.Casket).items.data.isEmpty) &&
              assertTrue(left.of(ArtifactKind.LivingBag).items.data.isEmpty) &&
              assertTrue(left.of(ArtifactKind.Wardrobe).items.data.map(_.id) == List(14L)) &&
              assertTrue(said.contains("из ларца 2") && said.contains("из живой сумки 1"))
    },

    test("забитая ячейка останавливает укладку и честно об этом говорит") {
      val full = (1 to 100).toList.map(i => gear(1000L + i))
      for {
        t <- vaultState(items = List(gear(1), gear(2)), cells = 1)
        (state, _, inv, bank, r) = t
        _    <- ZIO.foreachDiscard(full)(i => bank.deposit(TestFixtures.hero(userId).id, i))
        _    <- state.action(testUser, tap("VaultStowAll"), r)
        said <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(said.contains("Ячейка забита")) &&
              assertTrue(inv.snapshot.map(_.id).sorted == List(1L, 2L))
    },

    test("класть нечего — так и говорим, в ячейку ничего не падает") {
      for {
        t <- vaultState(items = List(quest(1)), settings = Some(only()))
        (state, _, _, bank, r) = t
        out  <- state.action(testUser, tap("VaultStowAll"), r)
        said <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(out == StateType.BankVault && bank.itemsSnapshot.isEmpty) &&
              assertTrue(said.contains("Класть нечего"))
    },

    test("без ячейки класть некуда: Рахадим сперва продаёт место") {
      for {
        t <- vaultState(items = List(herb(1)), cells = 0)
        (state, _, inv, bank, r) = t
        _    <- state.action(testUser, tap("VaultStowAll"), r)
        said <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(bank.itemsSnapshot.isEmpty && inv.snapshot.map(_.id) == List(1L)) &&
              assertTrue(said.contains("Ячеек у Вас пока нет"))
    },

    test("в меню хранилища кнопки стоят, и рядов не больше, чем держит ВК") {
      for {
        t <- vaultState()
        (state, _, _, _, r) = t
        _      <- state.enter(testUser, r)
        screen <- r.sentScreens.map(_.last)
        rows    = screen.choices.flatMap(_.row).distinct
      } yield assertTrue(screen.choices.map(_.id).containsSlice(List("VaultStowAll", "VaultStowSettings"))) &&
              assertTrue(rows.size == 4 && rows.size <= VkRenderer.MaxRows)
    }
  )
}
