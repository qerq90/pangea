package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{Bleed, Burn, Poison, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity => MobRarity}
import pangea.model.squad.{Ally, AllyKind, Squad}
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
