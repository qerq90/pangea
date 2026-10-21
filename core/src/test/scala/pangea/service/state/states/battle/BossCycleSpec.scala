package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{Bleed, Burn, Poison, SlainMonster, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity => MobRarity}
import pangea.model.squad.{Ally, AllyKind, Squad}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Приёмы достаются тому, кто стоит перед мобом: героя прикрыли союзником —
  * значит бьют союзника. Круг минибосса при этом идёт своим чередом. */
object BossCycleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def attack(pos: Int): UserAction =
    UserAction("", Some(s"""{"action":"Attack","target":"$pos"}"""))

  /** Герой стоит вторым, впереди — союзник: напротив врага оказывается он. */
  private val behindAlly = Squad(heroPos = 2, allies = List(Ally(AllyKind.Human, 1, 100000L, 100000L, 0L)))

  private def hero(squad: Squad): Hero = {
    val base = TestFixtures.hero(userId)
    base.copy(lvl = 30L, dungeonLevel = 30, squad = squad,
      fightStats = FightStats(atk = 10, hp = 10000000L, armor = 1000000L, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = base.baseStats.copy(str = 1, int = 10, agi = 10, vit = 1000000L))
  }

  private def bossOne(boss: MiniBoss, h: Hero, turn: Int = 0): SoloPveBattle = {
    val lvl  = boss.bossLvl(h.lvl)
    val mob  = Monster(0L, lvl, boss.race, MobRarity.Legendary, boss.stats(lvl))
    val base = SoloPveBattle.from(mob, h)
    base.copy(bossKind = Some(boss.entryName), bossTurn = turn, minionLvl = 20L, noKin = true,
      effects = base.effects.copy(monsterPoisonsOnHit = boss.poisonsOnHit))
  }

  /** Некромант выходит с поднятой роднёй — как в пещере. */
  private def necroWithGuards(h: Hero): SoloPveBattle = {
    val boss   = MiniBoss.Necromancer
    val lvl    = boss.bossLvl(h.lvl)
    val stats  = boss.stats(lvl)
    val necro  = Monster(0L, lvl, boss.race, MobRarity.Legendary, stats, customName = Some(boss.monsterName))
    val guards = List.fill(3)(
      MonsterGenerator.generateOfRaceAndRarity(lvl.toInt, Race.Orc, MobRarity.Common).copy(undead = true))
    val base   = SoloPveBattle.fromGroup(necro :: guards, h, stats.energy :: List.fill(guards.size)(0L))
    base.copy(bossKind = Some(boss.entryName), minionLvl = 20L, noKin = true,
      group = base.group.copy(originRace = Some(Race.Orc.entryName)))
  }

  private def round(h: Hero, battle: SoloPveBattle, at: Int, seed: Long) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, battle.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
      state     = BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content)
      _        <- TestRandom.setSeed(seed)
      _        <- state.action(testUser, attack(at), renderer)
      after    <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      hurt     <- dao.getHeroByUserId(userId).map(_.get)
      log      <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (after.get, hurt, log)

  /** Раунд без героя: он лежит с нулём HP, отряд бьётся за него сам (см.
    * `BattleState.squadTick`). `feed` — броски, которые нужны тесту точно. */
  private def squadRound(h: Hero, battle: SoloPveBattle, seed: Long, feed: ZIO[Any, Nothing, Unit] = ZIO.unit) =
    for {
      dao      <- TestHeroDao.withHero(userId, h.copy(fightStats = h.fightStats.copy(hp = 0L)))
      _        <- dao.writeActiveBattle(userId, battle.copy(group = battle.group.copy(heroDown = true)).asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
      state     = BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content)
      _        <- TestRandom.setSeed(seed) *> feed
      out      <- state.action(testUser, UserAction("", Some("""{"action":"SquadTick"}""")), renderer)
      after    <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      log      <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (out, after, log)

  /** Всякий бросок на попадание — в цель, на прок — мимо. */
  private val sureHits: ZIO[Any, Nothing, Unit] =
    TestRandom.feedInts(List.fill(10)(100): _*) *> TestRandom.feedLongs(List.fill(10)(100L): _*)

  private def allyOf(b: SoloPveBattle) = b.group.allies.headOption

  /** Союзник, который переживёт удар минибосса: статы наёмника для этого малы. */
  private def tough(b: SoloPveBattle): SoloPveBattle =
    b.copy(group = b.group.copy(allies = b.group.allies.map(a =>
      a.copy(hp = 10000000L, armor = 0L,
             stats = a.stats.copy(hp = 10000000L, armor = 0L, defence = 0L, evasion = 0L)))))

  override def spec = suite("Приёмы бьют того, кто впереди")(

    test("прикрытый союзником Крысиный король всё равно зовёт крыс") {
      val h = hero(behindAlly)
      for {
        t <- round(h, bossOne(MiniBoss.RatKing, h), at = 1, seed = 13L)
        (after, _, log) = t
      } yield assertTrue(!bossOne(MiniBoss.RatKing, h).group.paired) &&   // напротив короля — союзник
              assertTrue(log.contains("Крысиный писк разносится по окрестностям")) &&
              assertTrue(after.group.others.nonEmpty) &&
              assertTrue(after.bossTurn == 1) &&
              assertTrue(after.monsterCurrentEnergy < bossOne(MiniBoss.RatKing, h).monsterCurrentEnergy)
    },

    test("призыв не рвёт строй: мест ровно столько, сколько слотов") {
      // Зов добавляет мобов в бой того, кто ходит вне пары. Если собрать строй
      // из мест нового образца и слотов старого, бой падает на первом обращении
      // к месту — здесь проверяем, что места и слоты сходятся.
      val h = hero(behindAlly)
      for {
        t <- round(h, bossOne(MiniBoss.RatKing, h), at = 1, seed = 13L)
        (after, _, _) = t
      } yield assertTrue(after.group.places.size == after.group.others.size) &&
              assertTrue(after.group.places.distinct.size == after.group.places.size) &&
              assertTrue(after.placesInOrder.count(_._2.isDefined) == after.group.others.size + 1)
    },

    test("смрад Гнилого Джо травит союзника, а не героя за его спиной") {
      val h = hero(behindAlly)
      for {
        t <- round(h, bossOne(MiniBoss.RottenJoe, h, turn = 0), at = 1, seed = 13L)
        (after, hurt, log) = t
      } yield assertTrue(log.contains("Ядовитые зловония")) &&
              assertTrue(allyOf(after).flatMap(_.poison).exists(_.pct > 0)) &&
              assertTrue(after.effects.heroPoison.isEmpty) &&
              assertTrue(hurt.fightStats.hp == h.fightStats.hp) &&
              // круг сдвинулся и энергия списана — приём состоялся
              assertTrue(after.bossTurn == 1) &&
              assertTrue(after.monsterCurrentEnergy < bossOne(MiniBoss.RottenJoe, h).monsterCurrentEnergy)
    },

    test("пасть волка рвёт союзника: урон ему, кровь на нём") {
      val h = hero(behindAlly)
      for {
        t <- round(h, tough(bossOne(MiniBoss.WhiteWolf, h, turn = 0)), at = 1, seed = 13L)
        (after, hurt, _) = t
      } yield assertTrue(allyOf(after).exists(_.hp < 10000000L)) &&
              assertTrue(allyOf(after).flatMap(_.bleed).exists(_.pct > 0)) &&
              assertTrue(after.effects.heroBleed.isEmpty) &&
              assertTrue(hurt.fightStats.hp == h.fightStats.hp)
    },

    test("всплеск огненного поджигает союзника") {
      val h = hero(behindAlly)
      for {
        t <- round(h, tough(bossOne(MiniBoss.FireElemental, h, turn = 0)), at = 1, seed = 13L)
        (after, _, _) = t
        // Раскалённая броня элементаля жжёт того, кто его бьёт, — героя она
        // достаёт и через союзника, так что смотрим только на союзника.
      } yield assertTrue(allyOf(after).flatMap(_.burn).exists(_.pct > 0)) &&
              assertTrue(allyOf(after).exists(_.hp < 10000000L))
    },

    test("раны союзника тикают в конце раунда и видны в строю") {
      val h    = hero(behindAlly)
      val base = bossOne(MiniBoss.RottenJoe, h, turn = 3) // пропуск хода: мешать не будет
      val sick = tough(base).copy(group = tough(base).group.copy(allies = tough(base).group.allies.map(
        _.copy(poison = Some(Poison(10)), bleed = Some(Bleed(10)), burn = Some(Burn(10))))))
      for {
        t <- round(h, sick, at = 1, seed = 13L)
        (after, _, log) = t
        ally = allyOf(after)
      } yield assertTrue(ally.exists(_.hp < 10000000L)) &&
              // яд слабеет, кровь держится, пламя растёт — как у всех
              assertTrue(ally.flatMap(_.poison).exists(_.pct < 10)) &&
              assertTrue(ally.flatMap(_.bleed).exists(_.pct == 10)) &&
              assertTrue(ally.flatMap(_.burn).exists(_.pct > 10)) &&
              assertTrue(log.contains(Poison.Icon) && log.contains(Burn.Icon))
    },

    // ── Герой лежит: приёмы и круг идут и без него ────────────────────────────

    test("герой лежит — круг минибосса идёт: смрад Гнилого Джо травит союзника напротив") {
      // Раньше у лежачего героя ноль HP глушил умения всем мобам: били только
      // ударом, а минибоссы стояли со своим кругом.
      val h = hero(behindAlly)
      val b = tough(bossOne(MiniBoss.RottenJoe, h, turn = 0))
      for {
        t <- squadRound(h, b, seed = 13L)
        (out, after, log) = t
      } yield assertTrue(out == StateType.Battle) &&
              assertTrue(log.contains("Ядовитые зловония")) &&
              assertTrue(after.exists(a => allyOf(a).flatMap(_.poison).exists(_.pct > 0))) &&
              // круг сдвинулся и энергия списана — приём состоялся
              assertTrue(after.exists(_.bossTurn == 1)) &&
              assertTrue(after.exists(_.monsterCurrentEnergy < b.monsterCurrentEnergy)) &&
              // лежачего не трогают: приём достался союзнику
              assertTrue(after.exists(_.effects.heroPoison.isEmpty))
    },

    test("герой лежит — Некромант колдует сам: павшая нежить встаёт, миазмы держат поле") {
      // Он держит позицию и колдует в ход героя; пока герой лежит, хода героя
      // нет, — раньше Некромант так и стоял молча.
      val h      = hero(behindAlly)
      val base   = tough(bossOne(MiniBoss.Necromancer, h, turn = 0))
      val fallen = SlainMonster(base.monsterLvl, Race.Orc.entryName, MobRarity.Common.entryName,
                     marked = false, name = "Орк", undead = true)
      val b      = base.copy(group = base.group.copy(slain = List(fallen)))
      for {
        t <- squadRound(h, b, seed = 13L)
        (out, after, log) = t
      } yield assertTrue(out == StateType.Battle) &&
              assertTrue(log.contains("павшая нежить встаёт снова")) &&
              assertTrue(after.exists(a => a.group.others.size == 1 && a.group.slain.isEmpty)) &&
              assertTrue(after.exists(_.bossTurn == 1)) &&
              // и его миазмы ходят по полю: держит их или выдохся — строка об этом есть
              assertTrue(log.contains("🟣"))
    },

    test("герой лежит — Гнилого Джо добил союзник: Джо встаёт, как от удара героя") {
      val h = hero(behindAlly)
      val b = tough(bossOne(MiniBoss.RottenJoe, h, turn = 3)).copy(monsterCurrentHp = 1L, monsterCurrentArmor = 0L)
      for {
        t <- squadRound(h, b, seed = 13L, feed = sureHits)
        (out, after, log) = t
      } yield assertTrue(out == StateType.Battle) &&
              assertTrue(log.contains("Гнилой Джо отказывается умирать")) &&
              assertTrue(after.exists(a => a.bossRevives == 1 && a.monsterCurrentHp > 0L))
    },

    test("Гнилого Джо добил союзник, пока герой ждёт, — Джо тоже встаёт") {
      // Герой третьим, союзники на 1 и 2: до Джо на первом месте ему не
      // дотянуться — он ждёт, а Джо добивает союзник.
      val squad = Squad(heroPos = 3, allies = List(
        Ally(AllyKind.Human, 1, 100000L, 100000L, 0L), Ally(AllyKind.Gnome, 2, 100000L, 100000L, 0L)))
      val h = hero(squad)
      val b = tough(bossOne(MiniBoss.RottenJoe, h, turn = 3)).copy(monsterCurrentHp = 1L, monsterCurrentArmor = 0L)
      for {
        dao      <- TestHeroDao.withHero(userId, h)
        _        <- dao.writeActiveBattle(userId, b.asJson)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content)
        _        <- TestRandom.setSeed(13L) *> sureHits
        out      <- state.action(testUser, UserAction("", Some("""{"action":"Wait"}""")), renderer)
        after    <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        log      <- renderer.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(b.group.heroPos == 3 && !b.group.paired) &&
              assertTrue(out == StateType.Battle) &&
              assertTrue(log.contains("Гнилой Джо отказывается умирать")) &&
              assertTrue(after.exists(a => a.bossRevives == 1 && a.monsterCurrentHp > 0L))
    },

    test("Некромант за союзником берёт свой ход один раз за раунд") {
      // Он держит позицию и колдует в тихой фазе конца раунда; если бы ход
      // доставался ему и там, и в фазе мобов вне пары, круг шёл бы по два шага.
      val h = hero(behindAlly)
      for {
        t <- round(h, necroWithGuards(h), at = 1, seed = 13L)
        (after, _, _) = t
      } yield assertTrue(after.bossTurn == 1) &&
              assertTrue(after.group.places.size == after.group.others.size)
    }
  )
}
