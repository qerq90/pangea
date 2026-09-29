package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.monster.{MonsterGenerator, TowerRates}
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.squad.{Ally, AllyKind, Squad}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Башня со стрелком — сооружение: стоит на своём месте до конца боя, бьёт с
  * любого расстояния, не кровоточит и после боя не даёт ни опыта, ни добычи.
  * Когда охраны вокруг не осталось, стрелки уходят сами. */
object TowerBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val lvl = 10L

  /** Герой, которого хватает на весь бой: башня должна его поцарапать, а не убить. */
  private val HeroHp = 500000L

  private def hero(atk: Long, allies: List[Ally] = Nil, heroPos: Int = 1): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = lvl,
      fightStats = FightStats(atk = atk, hp = HeroHp, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000),
      squad      = Squad(heroPos = heroPos, allies = allies))

  private def guard(hp: Long, atk: Long = 10L): Monster =
    Monster(0L, lvl, Race.Orc, Rarity.Common,
      FightStats(atk = atk, hp = hp, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))

  /** Башня с боевой точностью: в тестах она должна попадать всегда. */
  private def tower(): Monster = {
    val t = MonsterGenerator.tower(lvl.toInt)
    t.copy(fightStats = t.fightStats.copy(accuracy = 9999L, armor = 0L, hp = 100000L))
  }

  /** Строй: охрана по местам от героя, башни — в самый хвост, как у каравана. */
  private def battle(h: Hero, guards: List[Monster], towers: List[Monster], tail: Int = 10): SoloPveBattle = {
    val b = SoloPveBattle.fromGroup(guards ++ towers, h, Nil)
    if (towers.isEmpty) b
    else {
      val places = b.group.places.zipWithIndex.map { case (p, i) =>
        val idx = i - (guards.size - 1)
        if (idx >= 0) tail - idx else p
      }
      b.copy(group = b.group.copy(places = places))
    }
  }

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))

  override def spec = suite("Башня со стрелком")(

    test("формула башни: атака 20×лвл, точность 30×лвл, защита 20×лвл, броня 150×лвл, HP 45×лвл") {
      val t = MonsterGenerator.tower(7)
      assertTrue(t.race == Race.Construct && t.rarity == Rarity.Rare) &&
      assertTrue(t.fightStats.atk == TowerRates.AtkPerLvl * 7 && t.fightStats.accuracy == TowerRates.AccuracyPerLvl * 7) &&
      assertTrue(t.fightStats.defence == TowerRates.DefencePerLvl * 7 && t.fightStats.armor == TowerRates.ArmorPerLvl * 7) &&
      assertTrue(t.fightStats.hp == TowerRates.HpPerLvl * 7 && t.fightStats.evasion == 0L && t.fightStats.energy == 0L) &&
      assertTrue(t.name == "Башня со стрелком")
    },

    test("сооружение не кровоточит и с места не сходит") {
      assertTrue(Race.immovable(Race.Construct) && Race.immuneToBleed(Race.Construct)) &&
      assertTrue(!Race.immovable(Race.Orc)) &&
      // именной башне звать сородичей неоткуда: она нарочно третьего грейда
      assertTrue(MonsterGenerator.tower(50).rarity == Rarity.Rare)
    },

    test("башня стреляет с десятого места по герою, хотя тот и близко не подошёл") {
      val h = hero(atk = 1L)
      for {
        t <- makeState(h, battle(h, List(guard(1_000_000L)), List(tower())))
        (state, dao, r) = t
        _       <- TestRandom.feedInts(60, 90, 90) *> TestRandom.feedLongs(100L, 100L, 100L)
        _       <- state.action(testUser, tap("Attack"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        after   <- battleOf(dao)
      } yield assertTrue(updated.fightStats.hp < HeroHp) &&
              // башня осталась на своём месте: её к герою не подтягивают
              assertTrue(after.exists(_.group.places == List(10)))
    },

    test("стрелок бьёт того, кто к нему ближе: союзника, а не героя за его спиной") {
      // Герой на первом месте, союзник на втором, башня на десятом: до союзника
      // ей ближе. Сравниваем тот же ход с башней и без неё — лишний урон по
      // союзнику и есть её выстрел.
      val ally = Ally(AllyKind.Human, position = 2, hp = 100000L, armor = 100000L, energy = 0L)
      val h    = hero(atk = 1L, allies = List(ally))
      def run(towers: List[Monster]) =
        for {
          t <- makeState(h, battle(h, List(guard(1_000_000L)), towers))
          (state, dao, r) = t
          _       <- TestRandom.feedInts(List.fill(12)(90): _*) *> TestRandom.feedLongs(List.fill(12)(100L): _*)
          _       <- state.action(testUser, tap("Attack"), r)
          after   <- battleOf(dao)
          updated <- dao.getHeroByUserId(userId).map(_.get)
          hurt     = after.flatMap(_.group.allyAt(2)).map(a => a.stats.hp + a.stats.armor - a.hp - a.armor)
        } yield (updated.fightStats.hp, hurt.getOrElse(-1L))
      for {
        plain  <- run(Nil)
        walled <- run(List(tower()))
      } yield assertTrue(walled._1 == plain._1) &&
              assertTrue(walled._2 > plain._2)
    },

    test("охрана полегла — стрелки бросают башни, и бой кончается победой") {
      val h = hero(atk = 1_000_000L)
      for {
        t <- makeState(h, battle(h, List(guard(1L)), List(tower())))
        (state, dao, r) = t
        _       <- TestRandom.feedInts(60, 90, 90) *> TestRandom.feedLongs(100L, 100L, 100L)
        _       <- state.action(testUser, tap("Attack"), r)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        left    <- battleOf(dao)
      } yield assertTrue(screens.contains("стрелки спускаются с башен")) &&
              assertTrue(left.isEmpty)
    },

    test("башня в добычу и опыт не идёт: за неё не дают ни строчки лута, ни убитого") {
      val h = hero(atk = 1_000_000L)
      def fight(towers: List[Monster]) =
        for {
          t <- makeState(h, battle(h, List(guard(1L)), towers))
          (state, dao, r) = t
          _       <- TestRandom.feedInts(60, 90, 90) *> TestRandom.feedLongs(100L, 100L, 100L)
          _       <- state.action(testUser, tap("Attack"), r)
          updated <- dao.getHeroByUserId(userId).map(_.get)
        } yield updated
      for {
        plain  <- fight(Nil)
        walled <- fight(List(tower()))
      } yield assertTrue(plain.exp > 0L && walled.exp == plain.exp) &&
              assertTrue(walled.kills == plain.kills && plain.kills == 1L)
    }
  )
}
