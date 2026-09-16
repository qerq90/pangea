package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.FlaskGenerator
import pangea.model.battle.{Bleed, Burn, Element, Poison, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{FlaskKind, FlaskRates, Item, ItemDetails, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity}
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
      assertTrue(FlaskKind.families.size == 9 && FlaskKind.elemental.size == 4)
    },

    test("кнопка боя — просто «Фляга» с зарядами, какой бы вид ни был надет") {
      val h = hero(FlaskKind.Poison)
      for {
        t <- makeState(h, SoloPveBattle.from(monster(), h))
        (state, _, r) = t
        _      <- state.enter(testUser, r)
        screen <- r.sentScreens.map(_.last)
      } yield assertTrue(screen.choices.exists(_.label == "🧪 Фляга (6)"))
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

    test("дымная фляга: 4 раунда сосед не бьёт сбоку и не лечит; активный бьёт как обычно; на пятый дым рассеялся") {
      val h = hero(FlaskKind.Smoke, hp = 50000L)
      // Активный ранен и без энергии; сосед цел и с энергией — без дыма он бьёт сбоку и лечит активного.
      val healer = monster().copy(fightStats = monster().fightStats.copy(energy = pangea.model.skill.MonsterEnergy.maxEnergy(10L)))
      val b = SoloPveBattle.fromGroup(List(monster(), healer), h, List(0L, pangea.model.skill.MonsterEnergy.maxEnergy(10L)))
      val wounded = b.copy(monsterCurrentHp = 50000L)
      // раунд в дыму: герой попадает (60), активный бьёт (99), подкрепления нет (99); сосед бросков не делает
      val quiet = List(60, 99, 99)
      for {
        t <- makeState(h, wounded)
        (state, dao, r) = t
        _   <- TestRandom.feedInts(List.fill(4)(quiet).flatten: _*) *> TestRandom.feedLongs(List.fill(8)(100L): _*)
        _   <- state.action(testUser, tap("UseFlask"), r)
        b0  <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        _   <- ZIO.foreachDiscard(1 to 4)(_ => state.action(testUser, tap("Attack"), r))
        b4  <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        u4  <- dao.getHeroByUserId(userId).map(_.get)
        log4 <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        // пятый раунд: сосед снова видит — бьёт сбоку (99) и лечит активного
        _   <- TestRandom.feedInts(60, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        _   <- state.action(testUser, tap("Attack"), r)
        b5  <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        log5 <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(b0.effects.heroSmokeTurns == FlaskRates.SmokeRounds + 1 && log4.contains("Дымная фляга разбивается")) &&
              assertTrue(log4.contains("Дым держится")) &&
              assertTrue(!log4.contains("атаковал вас сбоку") && !log4.contains("ударил сбоку") && !log4.contains("исцелил")) &&
              // активный моб в паре всё это время бил
              assertTrue(u4.fightStats.hp < 50000L && log4.contains("наносит")) &&
              // дым ещё держится на последнем тике, после пятого раунда — рассеялся
              assertTrue(b4.effects.heroSmokeTurns == 1 && b5.effects.heroSmokeTurns == 0) &&
              assertTrue(log5.contains("атаковал вас сбоку") && log5.contains("исцелил")) &&
              assertTrue(b5.monsterCurrentHp > b4.monsterCurrentHp) // активного подлечили
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

    test("фляга яда: пока оружие смазано, удары по HP травят; фляга крови — пускают кровь; сошла — нет") {
      val hp = hero(FlaskKind.Poison)
      val hb = hero(FlaskKind.Bleeding)
      // Яд и кровь считаются от макс.HP, поэтому механика — на трёх ударах, срок — на смазке с последним раундом.
      def seed(rounds: Int) = TestRandom.feedInts(List.fill(rounds)(List(60, 1, 99)).flatten: _*) *> TestRandom.feedLongs(List.fill(rounds)(100L): _*)
      for {
        tp <- makeState(hp, SoloPveBattle.from(monster(), hp))
        (sp, dp, rp) = tp
        _   <- seed(3)
        _   <- sp.action(testUser, tap("UseFlask"), rp)
        p0  <- dp.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        _   <- sp.action(testUser, tap("Attack"), rp)
        p1  <- dp.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        plog <- rp.sentScreens.map(_.map(_.text).mkString("\n"))
        tb <- makeState(hb, SoloPveBattle.from(monster(), hb))
        (sb, db, rb) = tb
        _   <- seed(3)
        _   <- sb.action(testUser, tap("UseFlask"), rb)
        _   <- sb.action(testUser, tap("Attack"), rb)
        _   <- sb.action(testUser, tap("Attack"), rb)
        _   <- sb.action(testUser, tap("Attack"), rb)
        b3  <- db.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        blog <- rb.sentScreens.map(_.map(_.text).mkString("\n"))
        // смазка кровью на последнем раунде: этот удар ещё режет, следующий — уже нет
        base  = SoloPveBattle.from(monster(), hb)
        te <- makeState(hb, base.copy(effects = base.effects.copy(heroBleedCoatTurns = 1)))
        (se, de, re) = te
        _   <- seed(2)
        _   <- se.action(testUser, tap("Attack"), re)
        e1  <- de.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        _   <- se.action(testUser, tap("Attack"), re)
        e2  <- de.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield assertTrue(p0.effects.heroPoisonCoatTurns == 5 && plog.contains("Фляга яда")) &&
              // после первого удара: яд стакнут и уже отработал тик (−2), крови нет
              assertTrue(p1.effects.monsterPoison.exists(_.pct == Poison.OnHit - Poison.DecayPerRound)) &&
              assertTrue(p1.effects.monsterBleed.isEmpty && p1.effects.heroPoisonCoatTurns == 4 && plog.contains("отравлен")) &&
              // кровь: три удара — три порции, яда нет
              assertTrue(blog.contains("Фляга крови")) &&
              assertTrue(b3.effects.monsterBleed.contains(Bleed(3 * FlaskRates.CoatBleedPct)) && b3.effects.monsterPoison.isEmpty) &&
              assertTrue(b3.effects.heroBleedCoatTurns == 2) &&
              assertTrue(e1.effects.monsterBleed.contains(Bleed(FlaskRates.CoatBleedPct)) && e1.effects.heroBleedCoatTurns == 0) &&
              assertTrue(e2.effects.monsterBleed.contains(Bleed(FlaskRates.CoatBleedPct)))
    },

    test("вампирская фляга пьёт кровь павших: 10% на +1 заряд с каждого, не выше полной; полной бросок не нужен") {
      def kill(start: Int, roll: Int) = {
        val h  = hero(FlaskKind.Vampiric).copy(equipment = TestFixtures.emptyEquipment.copy(weapon = weapon, chestPlate = chest,
                   flask = flask(FlaskKind.Vampiric).copy(details = ItemDetails.Flask(FlaskKind.Vampiric.effect, start, 6))))
        val h2 = h.copy(fightStats = h.fightStats.copy(atk = 100000L))
        for {
          t <- makeState(h2, SoloPveBattle.from(monster(hp = 10L), h2))
          (state, dao, r) = t
          // удар героя (60), сид добычи (long), бросок фляги (roll)
          _   <- TestRandom.feedInts(60, roll) *> TestRandom.feedLongs(100L, 7L)
          _   <- state.action(testUser, tap("Attack"), r)
          u   <- dao.getHeroByUserId(userId).map(_.get)
          log <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        } yield (charges(u), log)
      }
      for {
        lucky   <- kill(2, 5)
        unlucky <- kill(2, 50)
        full    <- kill(6, 5)
      } yield assertTrue(lucky._1 == 3 && lucky._2.contains("напилась крови: +1 заряд (3/6)")) &&
              assertTrue(unlucky._1 == 2 && !unlucky._2.contains("напилась крови")) &&
              assertTrue(full._1 == 6 && !full._2.contains("напилась крови"))
    },

    test("смазка из отвара держится весь бой: каждый удар по HP травит, после боя сходит вместе с пылью") {
      val h = hero(FlaskKind.Smith).copy(weaponDust = pangea.model.hero.WeaponDust(coat = Some(pangea.model.hero.WeaponCoat.Poison)))
      for {
        t <- makeState(h, SoloPveBattle.from(monster(), h))
        (state, dao, r) = t
        _  <- TestRandom.feedInts(List.fill(3)(List(60, 1, 99)).flatten: _*) *> TestRandom.feedLongs(100L, 100L, 100L)
        _  <- state.action(testUser, tap("Attack"), r)
        b1 <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        _  <- state.action(testUser, tap("Attack"), r)
        _  <- state.action(testUser, tap("Attack"), r)
        b3 <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        // добиваем: смазка после боя должна сойти
        k  = h.copy(fightStats = h.fightStats.copy(atk = 100000L))
        t2 <- makeState(k, SoloPveBattle.from(monster(hp = 10L), k))
        (state2, dao2, r2) = t2
        _  <- TestRandom.feedInts(60) *> TestRandom.feedLongs(100L, 7L)
        _  <- state2.action(testUser, tap("Attack"), r2)
        u2 <- dao2.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(b1.effects.monsterPoison.exists(_.pct == Poison.OnHit - Poison.DecayPerRound)) &&
              // три удара — три стака яда (каждый +8, тик −2)
              assertTrue(b3.effects.monsterPoison.exists(_.pct == 3 * Poison.OnHit - 3 * Poison.DecayPerRound)) &&
              assertTrue(b3.effects.monsterBleed.isEmpty) &&
              assertTrue(u2.weaponDust.isEmpty)
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
