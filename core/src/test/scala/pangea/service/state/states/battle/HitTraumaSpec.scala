package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.trauma.Trauma
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Травма от удара моба: 1% с любого урона по HP, наверняка — если один удар
 *  снял больше половины потолка HP. Бросок не тратится, когда урон ушёл в броню. */
object HitTraumaSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  /** Герой бьёт без промаха; потолок HP = vit × 24. */
  private def hero(hp: Long, armor: Long = 0L, vit: Long = 5000L): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = 10L,
      fightStats = FightStats(atk = 20, hp = hp, armor = armor, defence = 0, evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = vit))

  private def monster(atk: Long): Monster =
    Monster(0L, 10L, Race.Orc, Rarity.Common,
      FightStats(atk = atk, hp = 100000L, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  private def round(h: Hero, b: SoloPveBattle, ints: Int*) =
    for {
      t <- makeState(h, b)
      (state, dao, r) = t
      _   <- TestRandom.feedInts(ints: _*) *> TestRandom.feedLongs(100L, 100L)
      _   <- state.action(testUser, tap("Attack"), r)
      u   <- dao.getHeroByUserId(userId).map(_.get)
      log <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (u, log)

  override def spec = suite("Травма от удара")(

    test("удар по HP: бросок 1 — травма, 50 — нет; травма лёгкая и держится 8 часов") {
      val h = hero(hp = 100000L)
      for {
        // герой попал (60), моб попал (99), травма (1 → да), индекс травмы (0), подкрепление нет (99)
        hurt <- round(h, SoloPveBattle.from(monster(20L), h), 60, 99, 1, 0, 99)
        safe <- round(h, SoloPveBattle.from(monster(20L), h), 60, 99, 50, 99)
      } yield assertTrue(hurt._1.traumaNames == List(Trauma.light.head.name) && hurt._1.traumaUntil.isDefined) &&
              assertTrue(hurt._2.contains("Удар пришёлся неудачно — вы получили травму")) &&
              assertTrue(safe._1.traumaNames.isEmpty && !safe._2.contains("травму"))
    },

    test("сокрушительный удар по живому герою: травма сразу, с репликой") {
      val h = hero(hp = 30000L, vit = 10L) // порог = половина от 30 000
      for {
        r <- round(h, SoloPveBattle.from(monster(20000L), h), 60, 99, 0, 99)
        (u, log) = r
      } yield assertTrue(u.fightStats.hp > 0L && u.traumaNames.size == 1) &&
              assertTrue(log.contains("Удар был сокрушительным — вы получили травму"))
    },

    test("удар в броню травмы не даёт и бросок не тратит: подкрепление читает следующий int") {
      val h = hero(hp = 100000L, armor = 100000L)
      for {
        // герой попал, моб попал (в броню), подкрепление = 1 (≤ 2 — пришёл бы, будь это его бросок)
        r <- round(h, SoloPveBattle.from(monster(20L), h), 60, 99, 1, 99)
        (u, log) = r
      } yield assertTrue(u.traumaNames.isEmpty && !log.contains("травму")) &&
              assertTrue(log.contains("прибежал сородич")) // бросок 1 ушёл подкреплению, а не травме
    },

    test("удар сбоку в группе тоже может оставить травму") {
      val h = hero(hp = 100000L)
      val b = SoloPveBattle.fromGroup(List(monster(20L), monster(20L)), h, Nil)
      for {
        // герой попал (60), активный промахнулся (1), сосед попал (99), травма от соседа (1), индекс (0), подкрепление (99)
        r <- round(h, b, 60, 1, 99, 1, 0, 99)
        (u, log) = r
      } yield assertTrue(u.traumaNames.size == 1) &&
              assertTrue(log.contains("атаковал вас сбоку") && log.contains("вы получили травму"))
    }
  )
}
