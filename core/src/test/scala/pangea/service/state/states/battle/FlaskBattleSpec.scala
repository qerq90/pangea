package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.FlaskGenerator
import pangea.model.battle.{Bleed, Burn, Element, Poison, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{FlaskKind, FlaskRates, Item, ItemDetails, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Разновидности фляг: что делает каждая при использовании в бою. */
object FlaskBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def flask(kind: FlaskKind, rarity: ItemRarity = ItemRarity.Blue): Item =
    FlaskGenerator.item(kind, rarity).copy(id = 7L)

  private def weapon: Item =
    Item(50L, "Меч", 1L, ItemRarity.Blue, ItemType.Weapon,
      attack = 100, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  /** Кираса ради потолка брони: без неё кузнецу нечего чинить. */
  private def chest: Item =
    Item(51L, "Кираса", 1L, ItemRarity.Blue, ItemType.ChestPlate,
      attack = 0, accuracy = 0, energy = 0, armor = 1000, defence = 0, evasion = 0)

  /** Герой с флягой: бьёт без промаха, живучий; часть HP, брони и энергии не хватает. */
  private def hero(kind: FlaskKind, hp: Long = 50000L, armor: Long = 0L, energy: Long = 0L): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = 10L,
      fightStats = FightStats(atk = 100, hp = hp, armor = armor, defence = 0,
                              evasion = 0, accuracy = 9999, energy = energy),
      baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000, int = 100, agi = 100),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = weapon, chestPlate = chest, flask = flask(kind)))

  private def monster(hp: Long = 100000L, armor: Long = 0L): Monster =
    Monster(0L, 10L, Race.Orc, Rarity.Common,
      // Без энергии: умений моб не кастует, и броски хода предсказуемы.
      FightStats(atk = 20, hp = hp, armor = armor, defence = 0, evasion = 0, accuracy = 1, energy = 0))

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  private def use(h: Hero, b: SoloPveBattle, actions: String*) =
    for {
      t <- makeState(h, b)
      (state, dao, r) = t
      outs    <- ZIO.foreach(actions.toList)(a => state.action(testUser, tap(a), r))
      updated <- dao.getHeroByUserId(userId).map(_.get)
      after   <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      screens <- r.sentScreens
    } yield (updated, after, screens.map(_.text).mkString("\n"), outs.last, screens)

  private def charges(h: Hero): Int = h.equipment.flask.details match {
    case f: ItemDetails.Flask => f.charges
    case _                    => -1
  }

  override def spec = suite("Фляги")(

    test("заряды по редкости, цена у Ришелье по редкости, уровень не показывается") {
      val orange = FlaskGenerator.item(FlaskKind.Smith, ItemRarity.Orange)
      assertTrue(FlaskKind.chargesFor(ItemRarity.Gray) == 3 && FlaskKind.chargesFor(ItemRarity.Orange) == 10) &&
      assertTrue(FlaskKind.priceFor(ItemRarity.Gray) == 100L && FlaskKind.priceFor(ItemRarity.Violet) == 600L &&
                 FlaskKind.priceFor(ItemRarity.Orange) == 1000L) &&
      assertTrue(FlaskKind.rarityWeights.map(_._2).sum == 100) &&
      assertTrue(orange.displayTitle == "🟠 Фляга кузнеца" && !orange.displayTitle.contains("Ур.")) &&
      assertTrue(orange.statsLines.exists(_.contains("+25% макс. брони")) && orange.statsLines.exists(_.contains("Заряды: 10/10"))) &&
      assertTrue(FlaskKind.families.size == 8 && FlaskKind.elemental.size == 4)
    },

    test("кнопка боя подписана видом фляги") {
      val h = hero(FlaskKind.Venom)
      for {
        t <- makeState(h, SoloPveBattle.from(monster(), h))
        (state, _, r) = t
        _      <- state.enter(testUser, r)
        screen <- r.sentScreens.map(_.last)
      } yield assertTrue(screen.choices.exists(_.label == "🧪 Отрава (6)"))
    },

    test("фляга кузнеца чинит броню на 25% потолка, не выше потолка") {
      val h = hero(FlaskKind.Smith, armor = 10L)
      for {
        r <- use(h, SoloPveBattle.from(monster(), h), "UseFlask")
        (u, _, log, _, _) = r
        maxArmor = h.effectiveMaxArmor(0L)
      } yield assertTrue(u.fightStats.armor == (10L + maxArmor * 25L / 100L).min(maxArmor)) &&
              assertTrue(log.contains("Фляга кузнеца: восстановлено")) &&
              assertTrue(charges(u) == 5)
    },

    test("фляга бодрости возвращает 30% потолка энергии") {
      val h = hero(FlaskKind.Vigor, energy = 0L)
      for {
        r <- use(h, SoloPveBattle.from(monster(), h), "UseFlask")
        (u, _, log, _, _) = r
        maxEn = h.maxEnergy(0L)
      } yield assertTrue(u.fightStats.energy == (maxEn * 30L / 100L).min(maxEn)) &&
              assertTrue(log.contains("Фляга бодрости"))
    },

    test("стихийные фляги: прок гарантирован без броска — огонь жжёт, холод сковывает, молния выжигает энергию, ветер помогает") {
      def splash(kind: FlaskKind) = {
        val h = hero(kind)
        use(h, SoloPveBattle.from(monster(), h), "UseFlask").map(r => (r._2.get, r._3))
      }
      for {
        fire  <- splash(FlaskKind.Fire)
        cold  <- splash(FlaskKind.Cold)
        bolt  <- splash(FlaskKind.Lightning)
        air   <- splash(FlaskKind.Air)
      } yield assertTrue(fire._1.effects.monsterBurn.contains(Burn(Burn.Initial)) && fire._2.contains("объят ярким пламенем")) &&
              assertTrue(cold._1.effects.monsterColdDefenceCut == Element.Cold.DefenceReductionCut && cold._2.contains("сковал защиту")) &&
              assertTrue(bolt._1.monsterCurrentEnergy == 0L && bolt._2.contains("выжгла")) &&
              assertTrue(air._1.effects.airBoostTurns == Element.Air.ProcTurns && air._2.contains("Шквальный ветер")) &&
              // урона фляга не наносит, моб цел
              assertTrue(fire._1.monsterCurrentHp == 100000L) &&
              assertTrue(fire._2.contains("Вы плеснули во врага"))
    },

    test("огненная фляга об огненного элементаля — пламя гаснет, горения нет") {
      val h    = hero(FlaskKind.Fire)
      val lvl  = 1L
      val boss = Monster(0L, lvl, Race.Elemental, Rarity.Legendary, MiniBoss.FireElemental.stats(lvl))
      for {
        r <- use(h, SoloPveBattle.from(boss, h).copy(bossKind = Some(MiniBoss.FireElemental.entryName)), "UseFlask")
        (_, after, log, _, _) = r
      } yield assertTrue(after.get.effects.monsterBurn.isEmpty) &&
              assertTrue(log.contains("Пламя гаснет") && !log.contains("объят ярким пламенем"))
    },

    test("фляга очищения снимает горение, яд, кровь и дебафы с героя") {
      val h    = hero(FlaskKind.Cleansing)
      val base = SoloPveBattle.from(monster(), h)
      val hurt = base.copy(effects = base.effects.copy(
        heroBurn = Some(Burn(10)), heroPoison = Some(Poison(10)), heroBleed = Some(Bleed(4)),
        heroStunnedTurns = 2, heroGroundedTurns = 2, heroColdDefenceCut = 20))
      for {
        r <- use(h, hurt, "UseFlask")
        (_, after, log, _, _) = r
        e = after.get.effects
      } yield assertTrue(e.heroBurn.isEmpty && e.heroPoison.isEmpty && e.heroBleed.isEmpty) &&
              assertTrue(e.heroStunnedTurns == 0 && e.heroGroundedTurns == 0 && e.heroColdDefenceCut == 0) &&
              assertTrue(log.contains("Фляга очищения"))
    },

    test("дымная фляга: побег без ответного удара, бой закрыт, из группы не окружают") {
      val h = hero(FlaskKind.Smoke)
      for {
        r <- use(h, SoloPveBattle.fromGroup(List(monster(), monster(), monster()), h, Nil), "UseFlask")
        (u, after, log, out, _) = r
      } yield assertTrue(out == StateType.Dungeon) &&
              assertTrue(after.isEmpty) &&
              assertTrue(u.fightStats.hp == 50000L) && // ни удара в спину, ни удара сбоку
              assertTrue(log.contains("в дыму вы уходите") && log.contains("успешно сбежали")) &&
              assertTrue(!log.contains("окружает")) &&
              assertTrue(charges(u) == 5)
    },

    test("вампирская фляга: три удара по HP лечат на 30% урона, четвёртый — нет; в броню удар не считается") {
      val h  = hero(FlaskKind.Vampiric, hp = 10000L)
      val b  = SoloPveBattle.from(monster(), h)
      // фляга, затем четыре атаки: герой попадает (60), моб мажет (1) — герой не теряет HP
      // Раунд: герой попадает (60), моб мажет (1), подкрепление не идёт (99); герой HP не теряет.
      val seed = TestRandom.feedInts(List.fill(4)(List(60, 1, 99)).flatten: _*) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)
      for {
        t <- makeState(h, b)
        (state, dao, r) = t
        _   <- seed
        _   <- state.action(testUser, tap("UseFlask"), r)
        h1  <- dao.getHeroByUserId(userId).map(_.get)
        _   <- state.action(testUser, tap("Attack"), r)
        h2  <- dao.getHeroByUserId(userId).map(_.get)
        b2  <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        _   <- state.action(testUser, tap("Attack"), r)
        _   <- state.action(testUser, tap("Attack"), r)
        h4  <- dao.getHeroByUserId(userId).map(_.get)
        b4  <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        _   <- state.action(testUser, tap("Attack"), r)
        h5  <- dao.getHeroByUserId(userId).map(_.get)
        log <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        dealt = 100000L - b2.monsterCurrentHp
      } yield assertTrue(h1.fightStats.hp == 10000L && log.contains("Вампирская фляга")) &&
              assertTrue(dealt > 0L && h2.fightStats.hp == 10000L + dealt * 30L / 100L) &&
              assertTrue(b2.effects.heroVampiricHits == 2 && b4.effects.heroVampiricHits == 0) &&
              assertTrue(h4.fightStats.hp == 10000L + 3L * (dealt * 30L / 100L)) &&
              assertTrue(h5.fightStats.hp == h4.fightStats.hp) && // четвёртый удар уже не лечит
              assertTrue(log.contains("Вампиризм: восстановлено"))
    },

    test("фляга отравы: пока оружие смазано, удары по HP травят и пускают кровь; сошла — не пускают") {
      val h = hero(FlaskKind.Venom)
      val b = SoloPveBattle.from(monster(), h)
      // Яд и кровь считаются от макс.HP и за пять раундов добьют любого моба,
      // поэтому механика — на трёх ударах, а срок — на отраве с последним раундом.
      val seed = TestRandom.feedInts(List.fill(3)(List(60, 1, 99)).flatten: _*) *> TestRandom.feedLongs(List.fill(3)(100L): _*)
      val lastRound = b.copy(effects = b.effects.copy(heroVenomTurns = 1))
      for {
        t <- makeState(h, b)
        (state, dao, r) = t
        _   <- seed
        _   <- state.action(testUser, tap("UseFlask"), r)
        b0  <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        _   <- state.action(testUser, tap("Attack"), r)
        b1  <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        _   <- state.action(testUser, tap("Attack"), r)
        _   <- state.action(testUser, tap("Attack"), r)
        b3  <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        log <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        // отрава на последнем раунде: этот удар ещё режет, следующий — уже нет
        t2 <- makeState(h, lastRound)
        (state2, dao2, r2) = t2
        _   <- TestRandom.feedInts(60, 1, 99, 60, 1, 99) *> TestRandom.feedLongs(100L, 100L)
        _   <- state2.action(testUser, tap("Attack"), r2)
        e1  <- dao2.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        _   <- state2.action(testUser, tap("Attack"), r2)
        e2  <- dao2.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield assertTrue(b0.effects.heroVenomTurns == 5 && log.contains("оружие смазано")) &&
              // после первого удара: яд стакнут и уже отработал тик (−2), кровь 2%
              assertTrue(b1.effects.monsterPoison.exists(_.pct == Poison.OnHit - Poison.DecayPerRound)) &&
              assertTrue(b1.effects.monsterBleed.contains(Bleed(FlaskRates.VenomBleedPct))) &&
              assertTrue(b1.effects.heroVenomTurns == 4 && log.contains("отравлен")) &&
              // три удара — три порции крови
              assertTrue(b3.effects.monsterBleed.contains(Bleed(3 * FlaskRates.VenomBleedPct)) && b3.effects.heroVenomTurns == 2) &&
              assertTrue(e1.effects.monsterBleed.contains(Bleed(FlaskRates.VenomBleedPct)) && e1.effects.heroVenomTurns == 0) &&
              assertTrue(e2.effects.monsterBleed.contains(Bleed(FlaskRates.VenomBleedPct)))
    },

    test("глоток любой фляги тратит расходник раунда: второй в том же раунде не выпить") {
      val h = hero(FlaskKind.Smith, armor = 0L)
      for {
        r <- use(h, SoloPveBattle.from(monster(), h), "UseFlask", "UseFlask")
        (u, _, log, _, _) = r
      } yield assertTrue(charges(u) == 5) && assertTrue(log.contains("уже использовали"))
    }
  )
}
