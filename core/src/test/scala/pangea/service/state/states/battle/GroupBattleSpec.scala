package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.skill.MonsterEnergy
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Групповой бой: герой под номером 1 против строя мобов. В паре — активный,
  * сосед под номером 2 достаёт героя сбоку, дальние копят и лечат. */
object GroupBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val lvl = 10L

  private def hero(atk: Long = 20L, hp: Long = 500000L): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = lvl,
      fightStats = FightStats(atk = atk, hp = hp, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000))

  private def monster(hp: Long, atk: Long = 20L): Monster =
    Monster(0L, lvl, Race.Orc, Rarity.Common,
      FightStats(atk = atk, hp = hp, armor = 0, defence = 0, evasion = 0, accuracy = 9999,
                 energy = MonsterEnergy.maxEnergy(lvl)))

  /** Строй: активный и остальные, все с нулевой стартовой энергией (умений нет). */
  private def group(hps: Long*): SoloPveBattle =
    SoloPveBattle.fromGroup(hps.toList.map(monster(_)), hero(), Nil)

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  /** Раунд без сюрпризов: герой попал (60), моб в паре промахнулся (99), сосед
    * промахнулся (99), подкрепление не пришло (99). `extra` — свои броски. */
  private def quietRound(extra: Int*) =
    TestRandom.feedInts(60 +: 99 +: extra: _*) *> TestRandom.feedLongs(100L, 100L, 100L)

  override def spec = suite("Групповой бой")(

    test("экран группы: активный в паре с героем, остальные — одной строкой каждый") {
      for {
        t <- makeState(hero(), group(1000L, 1000L, 1000L))
        (state, _, r) = t
        _       <- quietRound(99, 99)
        _       <- state.action(testUser, tap("Attack"), r)
        screens <- r.sentScreens.map(_.map(_.text))
        summary  = screens.find(_.contains("VS")).getOrElse("")
      } yield assertTrue(summary.linesIterator.count(_.contains("🔴")) == 3) &&
              assertTrue(summary.contains("🟢 Вы VS 🔴")) &&
              assertTrue(summary.contains("❤")) && assertTrue(summary.contains("🧥"))
    },

    test("сосед под номером 2 бьёт героя сбоку, дальний под номером 3 — нет") {
      val h = hero(hp = 100000L)
      for {
        t <- makeState(h, group(1000L, 1000L, 1000L))
        (state, dao, r) = t
        // герой попал, активный промахнулся, сосед попал (10), подкрепление нет
        _       <- TestRandom.feedInts(60, 99, 10, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        _       <- state.action(testUser, tap("Attack"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(updated.fightStats.hp < 100000L) &&
              assertTrue(screens.contains("атаковал вас сбоку")) &&
              // третий не достаёт: ни удара, ни промаха с его стороны
              assertTrue(screens.linesIterator.count(_.contains("сбоку")) == 1)
    },

    test("в бою 1 на 1 ничего из этого нет: ни ударов сбоку, ни сводки") {
      for {
        t <- makeState(hero(), SoloPveBattle.from(monster(1000L), hero()))
        (state, _, r) = t
        _       <- quietRound()
        _       <- state.action(testUser, tap("Attack"), r)
        screens <- r.sentScreens.map(_.map(_.text))
      } yield assertTrue(!screens.exists(_.contains("VS"))) &&
              assertTrue(!screens.exists(_.contains("сбоку")))
    },

    test("активный пал — в пару встаёт следующий, бой продолжается, павший ждёт добычи") {
      for {
        t <- makeState(hero(atk = 100000L), group(10L, 1000L))
        (state, dao, r) = t
        _       <- quietRound(99)
        result  <- state.action(testUser, tap("Attack"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(after.monsterCurrentHp == 1000L) &&
              assertTrue(after.group.others.isEmpty) &&
              assertTrue(after.group.slain.size == 1) &&
              assertTrue(screens.contains("На его место встаёт"))
    },

    test("последний моб пал — это победа, добыча за всех") {
      for {
        t <- makeState(hero(atk = 100000L), group(10L))
        (state, _, r) = t
        _      <- quietRound()
        result <- state.action(testUser, tap("Attack"), r)
      } yield assertTrue(result == StateType.Loot)
    },

    test("дальний моб с фляжкой лечит раненого соседа, а не себя") {
      // Третий цел и с энергией на фляжку; второй ранен наполовину.
      val trio = group(1000L, 1000L, 1000L)
      val hurtSecond = trio.group.others.head.copy(currentHp = 500L)
      val richThird  = trio.group.others(1).copy(currentEnergy = MonsterEnergy.maxEnergy(lvl))
      val b = trio.copy(group = trio.group.copy(others = List(hurtSecond, richThird)))
      for {
        t <- makeState(hero(), b)
        (state, dao, r) = t
        _       <- quietRound(99)
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(screens.contains("исцелил вашего врага")) &&
              assertTrue(after.group.others.head.currentHp > 500L) &&
              // лекарь заплатил энергией
              assertTrue(after.group.others(1).currentEnergy < MonsterEnergy.maxEnergy(lvl))
    },

    test("подкрепление: на 2% в конце раунда приходит сородич той же расы") {
      for {
        t <- makeState(hero(), group(1000L)) // один моб без энергии — умений не будет
        (state, dao, r) = t
        // герой, моб, подкрепление = 1 (≤ 2 — пришёл)
        _       <- TestRandom.feedInts(60, 99, 1) *> TestRandom.feedLongs(100L, 100L, 7L, 10L)
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.isGroup) &&
              assertTrue(after.group.others.head.race == Race.Orc.entryName) &&
              assertTrue(screens.contains("прибежал сородич"))
    },

    test("выше пяти мобов подкрепление не приходит") {
      val five = group(1000L, 1000L, 1000L, 1000L, 1000L)
      for {
        t <- makeState(hero(), five)
        (state, dao, r) = t
        _     <- quietRound(99, 1)
        _     <- state.action(testUser, tap("Attack"), r)
        after <- battleOf(dao)
      } yield assertTrue(after.group.aliveCount == 5) &&
              assertTrue(after.group.others.size == 4)
    },

    test("каждый четвёртый раунд ряды перемешиваются") {
      // Три раунда без перемен, на четвёртом — перемешивание. С двумя мобами
      // разных HP видно, кто встал в пару.
      val b = group(1000L, 2000L).copy(group = group(1000L, 2000L).group.copy(round = 3))
      for {
        t <- makeState(hero(), b)
        (state, dao, r) = t
        _       <- quietRound(99, 99)
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(after.group.round == 4) &&
              assertTrue(screens.contains("Ряды смешались") ||
                         after.monsterStats.hp == 1000L) // перемешивание могло вернуть тот же порядок
    },

    test("бегство: свободные мобы могут окружить — по 5% за каждого") {
      for {
        t <- makeState(hero(), group(1000L, 1000L, 1000L)) // двое свободных → 10%
        (state, dao, r) = t
        // окружение (5 ≤ 10 — окружили)
        _       <- TestRandom.feedInts(5, 99, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
        result  <- state.action(testUser, tap("ConfirmFlee"), r)
        after   <- battleOf(dao)
        screens <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(screens.contains("окружает вас")) &&
              assertTrue(after.isGroup)
    },

    test("бегство 1 на 1 — окружать некому, бросок на это не тратится") {
      for {
        t <- makeState(hero(), SoloPveBattle.from(monster(1000L), hero()))
        (state, _, r) = t
        _      <- TestRandom.feedInts(99) *> TestRandom.feedLongs(100L)
        result <- state.action(testUser, tap("ConfirmFlee"), r)
      } yield assertTrue(result == StateType.Dungeon)
    }
  )
}
