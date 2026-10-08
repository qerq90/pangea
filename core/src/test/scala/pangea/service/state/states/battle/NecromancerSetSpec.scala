package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleEffects, BattlePrefs, Bleed, Formation, Poison, SlainMonster, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemSet, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{MiniBoss, Race, Rarity}
import pangea.model.squad.{Ally, AllyKind, AllyRates, Squad, UndeadForm}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Набор «Некромант»: интеллект, ожившие слуги, миазмы тьмы и восставшие враги. */
object NecromancerSetSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private val necro = ItemSet.Necromancer

  private val slots = List(ItemType.Helmet, ItemType.ShoulderPads, ItemType.ChestPlate, ItemType.Bracelets,
    ItemType.Gloves, ItemType.Pants, ItemType.Boots, ItemType.Amulet,
    ItemType.Ring, ItemType.Ring, ItemType.Belt, ItemType.Weapon)

  private def gear(n: Int): pangea.model.hero.Equipment =
    slots.take(n).zipWithIndex.foldLeft(TestFixtures.emptyEquipment) { case (eq, (t, i)) =>
      val it = Item(800L + i, "Предмет", 1L, ItemRarity.Blue, t,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
        set = Some(ItemSet.Necromancer))
      i match {
        case 0  => eq.copy(helmet = it)
        case 1  => eq.copy(shoulderPads = it)
        case 2  => eq.copy(chestPlate = it)
        case 3  => eq.copy(bracelets = it)
        case 4  => eq.copy(gloves = it)
        case 5  => eq.copy(pants = it)
        case 6  => eq.copy(boots = it)
        case 7  => eq.copy(amulet = it)
        case 8  => eq.copy(firstRing = it)
        case 9  => eq.copy(secondRing = it)
        case 10 => eq.copy(belt = it)
        case _  => eq.copy(weapon = it)
      }
    }

  private def hero(pieces: Int, squad: Squad = Squad.empty): Hero = TestFixtures.hero(userId).copy(
    lvl        = 10L,
    fightStats = FightStats(atk = 100, hp = 5000, armor = 0, defence = 0,
                            evasion = 9999, accuracy = 9999, energy = 60),
    baseStats  = TestFixtures.hero(userId).baseStats.copy(int = 20, str = 1),
    equipment  = gear(pieces),
    squad      = squad)

  /** Моб, который никого не убьёт и сам не умрёт. */
  private def tough(race: Race = Race.Orc, hp: Long = 100000L) = SoloPveBattle(
    monsterLvl          = 10L,
    monsterRace         = race.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 1, hp = hp, armor = 0, defence = 0,
                                     evasion = 0, accuracy = 1, energy = 0),
    monsterCurrentHp    = hp,
    monsterCurrentArmor = 0L)

  /** Поднятый без запаса энергии: умений он не применяет, и ход читается ровно. */
  private def undeadAlly(pos: Int, hp: Long = 400L): Ally =
    Ally(AllyKind.Undead, pos, hp, 0L, 0L,
      undead = Some(UndeadForm("Поднятый орк", 10L,
        FightStats(atk = 50, hp = 1000, armor = 200, defence = 10, evasion = 10, accuracy = 100, energy = 0))))

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  /** Все броски проходят: так срабатывает и восстание (5%). */
  private val allPass = TestRandom.feedInts(99 :: List.fill(60)(1): _*)

  override def spec = suite("Набор «Некромант»")(

    test("порог 2: +10% к интеллекту") {
      val bare = hero(1).effectiveBaseStats(0L).int
      val set  = hero(2).effectiveBaseStats(0L).int
      assertTrue(set == bare + bare * necro.IntPct / 100L) &&
      assertTrue(necro.IntPct == 10L)
    },

    test("порог 4: максимум энергии ниже на десятую часть") {
      val two  = hero(2)
      val four = hero(4)
      // интеллект у обоих одинаков (порог 2 открыт), разница — только в энергии
      assertTrue(two.effectiveBaseStats(0L).int == four.effectiveBaseStats(0L).int) &&
      assertTrue(four.maxEnergy(0L) < two.maxEnergy(0L)) &&
      assertTrue(four.sets.energyBonusPct == -necro.EnergyCutPct)
    },

    test("порог 4: ожившие союзники выходят в бой на 20% сильнее") {
      val squad = Squad(heroPos = 1, allies = List(undeadAlly(2)))
      val weak  = SoloPveBattle.from(tough().toMonster, hero(2, squad))
      val boost = SoloPveBattle.from(tough().toMonster, hero(4, squad))
      val plain = weak.group.allies.head.stats
      val up    = boost.group.allies.head.stats
      assertTrue(up.hp == plain.hp + plain.hp * necro.UndeadBoostPct / 100L) &&
      assertTrue(up.atk == plain.atk + plain.atk * necro.UndeadBoostPct / 100L) &&
      // крыса — зверь, её это не касается
      assertTrue(necro.UndeadBoostPct == 20L)
    },

    test("порог 6: поднятый служит вдвое дольше и выходит в бой целым") {
      val now   = 1_000_000L
      val form  = UndeadForm("Поднятый орк", 10L,
        FightStats(atk = 50, hp = 1000, armor = 200, defence = 10, evasion = 10, accuracy = 100, energy = 100))
      val short = Squad.empty.raise(form, 10L, now, hero(4).sets.undeadLastsMult)
      val long  = Squad.empty.raise(form, 10L, now, hero(6).sets.undeadLastsMult)
      // побитый поднятый выходит в бой целым только с порога 6
      val hurt  = Squad(heroPos = 1, allies = List(undeadAlly(2, hp = 100L)))
      val asIs  = SoloPveBattle.from(tough().toMonster, hero(4, hurt)).group.allies.head
      val fresh = SoloPveBattle.from(tough().toMonster, hero(6, hurt)).group.allies.head
      assertTrue(short.allies.head.hiredUntil == now + AllyRates.UndeadMs) &&
      assertTrue(long.allies.head.hiredUntil == now + AllyRates.UndeadMs * necro.UndeadLastsMult) &&
      assertTrue(asIs.hp == 100L && fresh.hp == fresh.stats.hp)
    },

    test("порог 10: герой — нежить, яд и кровь с него сходят, а огонь берёт сильнее") {
      val rotten = tough().copy(effects = BattleEffects(
        heroPoison = Some(Poison(10)), heroBleed = Some(Bleed(10))))
      for {
        t <- makeState(hero(10), rotten)
        (state, dao, r) = t
        _     <- allPass
        before <- dao.getHeroByUserId(userId).map(_.get.fightStats.hp)
        _     <- state.action(testUser, tap("Attack"), r)
        after <- battleOf(dao)
        h     <- dao.getHeroByUserId(userId).map(_.get)
        said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.effects.heroPoison.isEmpty && after.effects.heroBleed.isEmpty) &&
              assertTrue(said.contains("Мёртвой плоти ни яд, ни кровь не страшны")) &&
              // от яда и крови он не потерял ни одного HP (мобу бить его нечем)
              assertTrue(h.fightStats.hp >= before) &&
              // и это видно в профиле
              assertTrue(hero(10).raceName.startsWith("Нежить-") && hero(8).raceName == hero(8).race.toString) &&
              assertTrue(hero(10).sets.fireTakenMult > 1.0 && hero(10).sets.heroBurnGrowthMult == 2L)
    },

    test("порог 10: миазмы тратят энергию, лечат нежить и гноят живых") {
      val squad = Squad(heroPos = 1, allies = List(undeadAlly(2), Ally(AllyKind.Human, 3, 500L, 0L, 0L)))
      val h     = hero(10, squad)
      val b     = SoloPveBattle.from(tough().toMonster, h)
      for {
        t <- makeState(h, b)
        (state, dao, r) = t
        _      <- allPass
        _      <- state.action(testUser, tap("Attack"), r)
        after  <- battleOf(dao)
        hAfter <- dao.getHeroByUserId(userId).map(_.get)
        said   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        risen   = after.group.allyAt(2).get
        merc    = after.group.allyAt(3).get
      } yield // герой заплатил силой, а аура сказала о себе одной строкой на поле
              assertTrue(hAfter.fightStats.energy < h.maxEnergy(0L)) &&
              assertTrue(said.contains("Миазмы держат поле боя в руках")) &&
              // нежить окрепла, наёмник сгнил, моб из плоти — тоже, и всё это
              // молча: строки по каждому задетому больше нет
              assertTrue(risen.hp > 400L && merc.hp < 500L) &&
              assertTrue(after.monsterCurrentHp < 100000L) &&
              assertTrue(!said.contains("крепнет во тьме") && !said.contains("гниёт на ходу"))
    },

    test("порог 10: без энергии миазмы гаснут, а кнопка их сворачивает насовсем") {
      // Ум низкий — и запас мал, и реген в конце раунда почти ничего не добавит.
      val drained = hero(10).copy(
        baseStats  = hero(10).baseStats.copy(int = 1, agi = 1),
        fightStats = hero(10).fightStats.copy(energy = 0L))
      for {
        t <- makeState(drained, tough())
        (state, dao, r) = t
        _      <- allPass
        _      <- state.action(testUser, tap("Attack"), r)
        said   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        // кнопка: выключили — помнится между боями
        _      <- state.action(testUser, tap("Miasma"), r)
        prefs  <- dao.readBattlePrefs(userId).map(_.flatMap(_.as[BattlePrefs].toOption).get)
        screen <- r.sentScreens.map(_.last)
      } yield assertTrue(said.contains("Сил держать миазмы больше нет")) &&
              assertTrue(!prefs.miasma && BattlePrefs.default.miasma) &&
              assertTrue(screen.choices.exists(c => c.id == "Miasma" && c.label.contains("выкл")))
    },

    test("порог 10: при выключенных миазмах тьма не стелется") {
      for {
        t <- makeState(hero(10), tough())
        (state, dao, r) = t
        _      <- dao.writeBattlePrefs(userId, BattlePrefs(miasma = false).asJson)
        _      <- allPass
        before <- battleOf(dao).map(_.monsterCurrentHp)
        _      <- state.action(testUser, tap("Attack"), r)
        said   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        after  <- battleOf(dao)
      } yield assertTrue(!said.contains("Миазмы держат поле боя")) &&
              // мобу досталось только от удара героя — тьма его не тронула
              assertTrue(before - after.monsterCurrentHp < 1000L)
    },

    test("порог 12: добитый героем встаёт в строй с четвертью HP и без брони") {
      val weak = tough(hp = 1L)
      for {
        t <- makeState(hero(12), weak)
        (state, dao, r) = t
        _     <- allPass
        _     <- state.action(testUser, tap("Attack"), r)
        said  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        h     <- dao.getHeroByUserId(userId).map(_.get)
        risen  = h.squad.allies.headOption
      } yield assertTrue(said.contains("поднимается на ваш зов")) &&
              // после боя он ушёл с героем, на двое суток
              assertTrue(risen.exists(_.kind == AllyKind.Undead)) &&
              assertTrue(risen.exists(_.hiredUntil > 0L)) &&
              assertTrue(necro.RiseChancePct == 5L && necro.RiseHpPct == 25L) &&
              assertTrue(AllyRates.RisenLasts == 2L)
    },

    test("кого поднимать нечем: элементали, Джо, Крысиный король; волк встаёт по клыку") {
      def slain(race: Race, name: String = "Орк раб") =
        SlainMonster(5L, race.entryName, Rarity.Common.entryName, marked = false, name)
      val wolf = BattleState.riseForm(slain(Race.Animal, "Белый Волк"), Some(MiniBoss.WhiteWolf))
      val orc  = BattleState.riseForm(slain(Race.Orc, "Деррик Мясник"), None)
      assertTrue(BattleState.riseForm(slain(Race.Elemental), Some(MiniBoss.FireElemental)).isEmpty) &&
      assertTrue(BattleState.riseForm(slain(Race.Elemental), Some(MiniBoss.StoneElemental)).isEmpty) &&
      assertTrue(BattleState.riseForm(slain(Race.Undead), Some(MiniBoss.RottenJoe)).isEmpty) &&
      assertTrue(BattleState.riseForm(slain(Race.Animal), Some(MiniBoss.RatKing)).isEmpty) &&
      // элементаль и сооружение не встают и без боссовой пометки
      assertTrue(BattleState.riseForm(slain(Race.Elemental), None).isEmpty) &&
      assertTrue(BattleState.riseForm(slain(Race.Construct), None).isEmpty) &&
      // волк — как по клыку, тёмным волком
      assertTrue(wolf.exists(_.name == pangea.service.state.states.events.cave.DarkAltar.DarkWolfName)) &&
      // прочие — как по трофею и под своим именем
      assertTrue(orc.exists(_.name == "Деррик Мясник") && orc.exists(_.lvl == 5L))
    },

    test("мест в отряде нет — восставший ждёт у шатра, и герой решает, кем жертвовать") {
      val now  = 1_000_000L
      val form = UndeadForm("Орк раб", 5L,
        FightStats(atk = 10, hp = 200, armor = 0, defence = 1, evasion = 1, accuracy = 10, energy = 10))
      val full = (2 to Formation.HeroPlaces).foldLeft(Squad.empty)((s, p) => s.copy(allies = s.allies :+ undeadAlly(p)))
      val waiting = full.admitRisen(form, 10L, now)
      val took    = waiting.takeRisenPlace(2, 10L, now)
      val dusted  = waiting.dismissRisen
      assertTrue(full.full && waiting.pendingRisen.contains(form)) &&
      // второй ждущий первого не перебивает
      assertTrue(waiting.admitRisen(form.copy(name = "Другой"), 10L, now).pendingRisen.contains(form)) &&
      assertTrue(took.pendingRisen.isEmpty && took.allyAt(2).exists(_.name == "Орк раб")) &&
      assertTrue(took.allyAt(2).exists(_.hiredUntil == now + AllyRates.UndeadMs * AllyRates.RisenLasts)) &&
      assertTrue(dusted.pendingRisen.isEmpty && dusted.allies.size == full.allies.size) &&
      // место свободно — ждать нечего, встаёт сразу
      assertTrue(Squad.empty.admitRisen(form, 10L, now).pendingRisen.isEmpty)
    }
  )
}
