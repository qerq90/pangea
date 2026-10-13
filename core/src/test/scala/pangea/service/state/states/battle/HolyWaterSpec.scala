package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.cave.{CaveRoom, CaveScene, RoomKind}
import pangea.model.hero.{Equipment, Hero, LoreData}
import pangea.model.item.{BeltSlot, Item, ItemDetails, ItemSet, ItemType, MaterialKind, Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.events.cave.MonsterCaveState
import pangea.service.state.states.temple.TempleAzatState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
import zio.{Task, ZIO}
import zio.test._

/** Святая вода: Жрец её продаёт (но не тому, в ком тьма пустила корни), на
  * поясе она лежит склянкой, а разлитая — до конца боя жжёт мёртвых. С
  * миазмами тьмы, чьими угодно, они гасят друг друга. */
object HolyWaterSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def useSlot(idx: Int): UserAction =
    UserAction("", Some(s"""{"action":"BeltUse","slot":"$idx"}"""))

  private def content = ZIO.attempt(SceneContent.load())

  private def holy(id: Long): Item = MaterialGenerator.item(MaterialKind.HolyWater).copy(id = id)

  /** Пояс с ячейками: синий держит три. */
  private def belt(slots: BeltSlot*): Item =
    Item(9L, "Пояс", 1L, ItemRarity.Blue, ItemType.Belt,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Belt(slots.toList))

  /** Набор «Некромант» из `n` вещей (не больше десяти — пояс и оружие свободны). */
  private def necroGear(n: Int): Equipment = {
    val slots = List(ItemType.Helmet, ItemType.ShoulderPads, ItemType.ChestPlate, ItemType.Bracelets,
      ItemType.Gloves, ItemType.Pants, ItemType.Boots, ItemType.Amulet, ItemType.Ring, ItemType.Ring)
    slots.take(n).zipWithIndex.foldLeft(TestFixtures.emptyEquipment) { case (eq, (t, i)) =>
      val it = Item(800L + i, "Предмет", 1L, ItemRarity.Blue, t,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
        set = Some(ItemSet.Necromancer))
      i match {
        case 0 => eq.copy(helmet = it)
        case 1 => eq.copy(shoulderPads = it)
        case 2 => eq.copy(chestPlate = it)
        case 3 => eq.copy(bracelets = it)
        case 4 => eq.copy(gloves = it)
        case 5 => eq.copy(pants = it)
        case 6 => eq.copy(boots = it)
        case 7 => eq.copy(amulet = it)
        case 8 => eq.copy(firstRing = it)
        case _ => eq.copy(secondRing = it)
      }
    }
  }

  /** Герой, которого не убьёт ни один ответ: бьёт еле-еле, чтобы было видно,
    * сколько снимает вода, а сколько удар. */
  private def hero(pieces: Int = 0, beltItem: Item = belt(BeltSlot.HolyWater())): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = 10L,
      fightStats = FightStats(atk = 1, hp = 5000, armor = 0, defence = 0,
                              evasion = 9999, accuracy = 9999, energy = 60),
      baseStats  = TestFixtures.hero(userId).baseStats.copy(int = 20, str = 1),
      equipment  = necroGear(pieces).copy(belt = beltItem))

  /** Противник на сто тысяч HP: живой или поднятый. */
  private def foe(undead: Boolean, hp: Long = 100000L): SoloPveBattle = SoloPveBattle(
    monsterLvl          = 10L,
    monsterRace         = Race.Orc.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 1, hp = hp, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0),
    monsterCurrentHp    = hp,
    monsterCurrentArmor = 0L,
    monsterUndead       = undead)

  private def makeState(h: Hero, b: SoloPveBattle,
                        inv: TestInventoryRepository = TestInventoryRepository.accepting) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      c        <- content
    } yield (BattleState(dao, inv, TestItemRepository.make, c), dao, renderer)

  private def battleOf(dao: TestHeroDao): Task[SoloPveBattle] =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  private def slotsOf(h: Hero): List[BeltSlot] = h.equipment.belt.details match {
    case b: ItemDetails.Belt => b.slots
    case _                   => Nil
  }

  /** Покупка у Жреца: что он показал, сколько серебра осталось, что сказал и что
    * легло в сумку. */
  private def buy(silver: Long, inv: TestInventoryRepository, pieces: Int = 0) =
    for {
      dao   <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(silver = silver, equipment = necroGear(pieces)))
      r     <- TestRenderer.make
      c     <- content
      state  = TempleAzatState(dao, inv, TestItemRepository.make, c)
      _     <- state.action(testUser, tap("HolyWater"), r)
      offer <- r.sentScreens.map(_.last)
      _     <- state.action(testUser, tap("BuyHolyWater"), r)
      left  <- dao.getHeroByUserId(userId).map(_.get.silver)
      said  <- texts(r)
    } yield (offer, left, said, inv.snapshot)

  override def spec = suite("Святая вода")(

    // ── Предмет ──────────────────────────────────────────────────────────────

    test("склянка ложится на пояс, видна в карточке пояса и читается обратно") {
      val card = belt(BeltSlot.HolyWater(), BeltSlot.HolyWater(spent = true)).statsLines
      assertTrue(BeltSlot.of(holy(1L)).contains(BeltSlot.HolyWater())) &&
      assertTrue((BeltSlot.HolyWater(): BeltSlot).asJson.as[BeltSlot] == Right(BeltSlot.HolyWater())) &&
      assertTrue((BeltSlot.HolyWater(spent = true): BeltSlot).asJson.as[BeltSlot] ==
        Right(BeltSlot.HolyWater(spent = true))) &&
      assertTrue(card.contains("• Святая вода") && card.contains("• Святая вода — пусто")) &&
      // Ришелье святыней не торгует; в описании — что она делает, а не как ей пользоваться
      assertTrue(MaterialKind.HolyWater.worthless && MaterialKind.HolyWater.description.contains("купели")) &&
      assertTrue(!MaterialKind.HolyWater.description.contains("пояс"))
    },

    // ── Жрец ─────────────────────────────────────────────────────────────────

    test("о некромантах жрец рассказывает, только когда к нему отправил трактирщик") {
      def menu(lore: LoreData) =
        for {
          dao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
          _    <- dao.writeLoreData(userId, lore.asJson)
          r    <- TestRenderer.make
          c    <- content
          state = TempleAzatState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c)
          _    <- state.action(testUser, tap("Priest"), r)
          ids  <- r.sentScreens.map(_.last.choices.map(_.id))
          _    <- state.action(testUser, tap("PriestNecroLore"), r)
          told <- r.sentScreens.map(_.last.text)
        } yield (ids, told)
      for {
        before <- menu(LoreData(metNecro = true))
        after  <- menu(LoreData(metNecro = true, necroSent = true))
      } yield assertTrue(!before._1.contains("PriestNecroLore") && before._1.contains("HolyWater")) &&
              assertTrue(after._1.contains("PriestNecroLore")) &&
              assertTrue(after._2.contains("Ищи его самого") && after._2.contains("святую воду")) &&
              // про алтарь жрец молчит
              assertTrue(!after._2.contains("алтарь"))
    },

    test("склянка у жреца — тысяча серебра; без денег и с полной сумкой ничего не списано") {
      for {
        ok   <- buy(1500L, TestInventoryRepository.accepting)
        poor <- buy(100L, TestInventoryRepository.accepting)
        full <- buy(1500L, TestInventoryRepository.full)
      } yield assertTrue(ok._1.text.contains("1000 серебра") && ok._1.choices.exists(_.id == "BuyHolyWater")) &&
              assertTrue(ok._2 == 500L && ok._4.exists(_.material.contains(MaterialKind.HolyWater))) &&
              assertTrue(ok._3.contains("Да хранит тебя Азат")) &&
              assertTrue(poor._2 == 100L && poor._4.isEmpty && poor._3.contains("нужно 1000 серебра")) &&
              assertTrue(full._2 == 1500L)
    },

    test("с «Некромантом» от шести вещей жрец воды не продаёт, с пятью — ещё продаёт") {
      for {
        six  <- buy(5000L, TestInventoryRepository.accepting, pieces = 6)
        five <- buy(5000L, TestInventoryRepository.accepting, pieces = 5)
      } yield assertTrue(six._2 == 5000L && six._4.isEmpty && six._3.contains("Тьма уже пустила в тебе корни")) &&
              assertTrue(five._2 == 4000L && five._4.size == 1) &&
              assertTrue(TempleAzatState.HolyWaterBannedFrom == 6 && TempleAzatState.HolyWaterPrice == 1000L)
    },

    // ── Бой ──────────────────────────────────────────────────────────────────

    test("склянка из пояса разливает воду до конца боя; вторая на то же поле не тратится") {
      val h = hero(beltItem = belt(BeltSlot.HolyWater(), BeltSlot.HolyWater()))
      for {
        t <- makeState(h, foe(undead = false))
        (state, dao, r) = t
        _     <- state.action(testUser, useSlot(0), r)
        b1    <- battleOf(dao)
        h1    <- dao.getHeroByUserId(userId).map(_.get)
        said  <- texts(r)
        // новый раунд: за пояс снова можно браться, но вода уже на поле
        _     <- dao.writeActiveBattle(userId, b1.copy(consumableUsedThisRound = false).asJson)
        _     <- state.action(testUser, useSlot(1), r)
        h2    <- dao.getHeroByUserId(userId).map(_.get)
        again <- texts(r)
      } yield assertTrue(b1.holyWater && said.contains("окропляете поле святой водой")) &&
              assertTrue(slotsOf(h1) == List(BeltSlot.HolyWater(spent = true), BeltSlot.HolyWater())) &&
              assertTrue(again.contains("уже на поле") && slotsOf(h2) == slotsOf(h1))
    },

    test("в конце раунда вода снимает с мёртвых по 5% максимума, а живых не трогает") {
      def round(undead: Boolean) =
        for {
          t <- makeState(hero(), foe(undead).copy(holyWater = true))
          (state, dao, r) = t
          _     <- state.action(testUser, tap("Attack"), r)
          after <- battleOf(dao)
          said  <- texts(r)
        } yield (100000L - after.monsterCurrentHp, said)
      for {
        dead   <- round(undead = true)
        living <- round(undead = false)
      } yield assertTrue(dead._1 >= 5000L && dead._1 < 6000L) &&
              assertTrue(dead._2.contains("Святая вода жжёт мёртвых: каждый теряет по 5% здоровья")) &&
              // живому досталось только от удара, и вода о себе промолчала
              assertTrue(living._1 < 1000L && !living._2.contains("Святая вода жжёт"))
    },

    test("миазмы героя гасят воду; свернул их — вода жжёт мёртвых и самого героя-нежить") {
      for {
        t <- makeState(hero(pieces = 10), foe(undead = true).copy(holyWater = true))
        (state, dao, r) = t
        _     <- state.action(testUser, tap("Attack"), r)
        b1    <- battleOf(dao)
        said1 <- texts(r)
        _     <- state.action(testUser, tap("Miasma"), r)
        seen  <- r.sentScreens.map(_.size)
        _     <- state.action(testUser, tap("Attack"), r)
        b2    <- battleOf(dao)
        said2 <- r.sentScreens.map(_.drop(seen).map(_.text).mkString("\n"))
      } yield assertTrue(said1.contains("Святая вода и миазмы тьмы гасят друг друга")) &&
              // ни тьмы, ни воды: мобу досталось только от удара
              assertTrue(!said1.contains("Миазмы держат поле боя") && 100000L - b1.monsterCurrentHp < 1000L) &&
              assertTrue(said2.contains("Святая вода жжёт мёртвых") && said2.contains("Святая вода жжёт и вас")) &&
              assertTrue(b1.monsterCurrentHp - b2.monsterCurrentHp >= 5000L)
    },

    test("миазмы Некроманта гасят воду, пока ему есть чем их держать; выдохся — вода жжёт и его") {
      def necro(energy: Long) = {
        val b = foe(undead = false)
        b.copy(monsterRace = Race.Undead.entryName, bossKind = Some("Necromancer"),
          monsterStats = b.monsterStats.copy(energy = energy), monsterCurrentEnergy = energy, holyWater = true)
      }
      def round(energy: Long) =
        for {
          t <- makeState(hero(), necro(energy))
          (state, dao, r) = t
          _     <- state.action(testUser, tap("Attack"), r)
          after <- battleOf(dao)
          said  <- texts(r)
        } yield (100000L - after.monsterCurrentHp, said)
      for {
        held  <- round(1000000L)
        empty <- round(0L)
      } yield assertTrue(held._2.contains("Святая вода и миазмы тьмы гасят друг друга")) &&
              assertTrue(!held._2.contains("Миазмы Некроманта держат поле боя") && !held._2.contains("Святая вода жжёт")) &&
              assertTrue(held._1 < 1000L) &&
              // выдохся: миазмы осели, и вода жжёт самого Некроманта
              assertTrue(empty._2.contains("Некромант выдохся") && empty._2.contains("Святая вода жжёт мёртвых")) &&
              assertTrue(empty._1 >= 5000L)
    },

    test("после боя пустая ячейка сама берёт склянку святой воды из сумки") {
      val h   = hero(beltItem = belt(BeltSlot.HolyWater(spent = true)))
      val inv = TestInventoryRepository.withItems(List(holy(42L)))
      for {
        t <- makeState(h, foe(undead = false, hp = 1L), inv)
        (state, dao, r) = t
        // моб с одной единицей HP: бьём, пока бой не кончится
        _     <- {
                   def attack(left: Int): Task[StateType] =
                     state.action(testUser, tap("Attack"), r).flatMap { st =>
                       if (st == StateType.Battle && left > 1) attack(left - 1) else ZIO.succeed(st)
                     }
                   attack(5)
                 }
        after <- dao.getHeroByUserId(userId).map(_.get)
        said  <- texts(r)
      } yield assertTrue(slotsOf(after) == List(BeltSlot.HolyWater())) &&
              assertTrue(inv.snapshot.isEmpty && said.contains("На пояс легло из сумки"))
    },

    // ── Встреча в логове ─────────────────────────────────────────────────────

    test("встреча в логове записывается: Некромант и Крысиный король — каждый своей отметкой") {
      def lair(boss: Option[String], sewer: Boolean) = CaveScene(
        race   = if (sewer) Race.Animal.entryName else Race.Orc.entryName,
        rooms  = List(CaveRoom(0, 0, 0, RoomKind.Empty, done = true), CaveRoom(0, 1, 1, RoomKind.Lair)),
        inside = true, sewer = sewer, questLvl = if (sewer) 5L else 0L, floor = if (sewer) 2 else 1, boss = boss)
      def meet(scene: CaveScene) =
        for {
          dao   <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, dungeonLevel = 10))
          sched <- TestScheduler.make
          r     <- TestRenderer.make
          c     <- content
          state  = MonsterCaveState(dao, TestInventoryRepository.accepting, TestItemRepository.make, sched, c, None)
          _     <- dao.writeSceneData(userId, scene.asJson)
          out   <- state.action(testUser, tap("CaveForward"), r)
          lore  <- dao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))
        } yield (out, lore)
      for {
        necro <- meet(lair(Some("Necromancer"), sewer = false))
        king  <- meet(lair(None, sewer = true))
      } yield assertTrue(necro._1 == StateType.Battle && necro._2.metNecro && !necro._2.metRatKing) &&
              assertTrue(king._1 == StateType.Battle && king._2.metRatKing && !king._2.metNecro)
    }
  )
}
