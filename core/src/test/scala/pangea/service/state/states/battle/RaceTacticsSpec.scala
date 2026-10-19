package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{RaceTactics, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.{TestRandom, _}

/** Расы в строю: эльфы — стрелки и бьют на три места, орки за раунд проходят
  * два. Прочие бьют соседей и идут по месту за раунд. */
object RaceTacticsSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val hero: Hero = {
    val base = TestFixtures.hero(userId)
    base.copy(
      lvl        = 10L,
      fightStats = FightStats(atk = 1L, hp = 500000L, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0),
      baseStats  = base.baseStats.copy(vit = 5000))
  }

  private def mob(race: Race): Monster =
    Monster(0L, 10L, race, Rarity.Common,
      FightStats(atk = 50, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))

  /** Строй: в паре с героем на первом месте человек, остальные — на `places`. */
  private def line(races: List[Race], places: List[Int]): SoloPveBattle = {
    val b = SoloPveBattle.fromGroup(mob(Race.Human) :: races.map(mob), hero, Nil)
    b.copy(group = b.group.copy(heroPos = 1, activePos = 1, places = places))
  }

  /** Один моб, стоящий не напротив героя, а на месте `at`. */
  private def lone(race: Race, at: Int): SoloPveBattle = {
    val b = SoloPveBattle.fromGroup(List(mob(race)), hero, Nil)
    b.copy(group = b.group.copy(heroPos = 1, activePos = at))
  }

  override def spec = suite("Расы в строю")(

    test("орк проходит за раунд два места, человек — одно") {
      val (orc, _)   = line(List(Race.Orc), List(5)).closeIn
      val (human, _) = line(List(Race.Human), List(5)).closeIn
      assertTrue(RaceTactics.speed(Race.Orc) == 2 && RaceTactics.speed(Race.Human) == 1) &&
      assertTrue(orc.group.places == List(3) && human.group.places == List(4))
    },

    test("орк останавливается, как только достаёт героя, и не перелетает чужую спину") {
      // с четвёртого: шаг на третье, шаг на второе — и он уже сосед героя
      val (near, _)    = line(List(Race.Orc), List(4)).closeIn
      // с четвёртого, а на втором стоит другой: дошёл до третьего и упёрся
      val (blocked, _) = line(List(Race.Human, Race.Orc), List(2, 4)).closeIn
      assertTrue(near.group.places == List(2)) &&
      assertTrue(blocked.group.places == List(2, 3))
    },

    test("эльф подходит только на расстояние выстрела — три места — и дальше стоит") {
      val (far, _)   = line(List(Race.Elf), List(6)).closeIn
      val (ready, _) = line(List(Race.Elf), List(4)).closeIn
      assertTrue(RaceTactics.reach(Race.Elf) == 3) &&
      assertTrue(far.group.places == List(5) && ready.group.places == List(4)) &&
      // тот, кто в полях, ходит по тем же правилам
      assertTrue(lone(Race.Elf, 4).stepActiveTowardsHero.isEmpty) &&
      assertTrue(lone(Race.Orc, 6).stepActiveTowardsHero.exists(_.group.activePos == 4))
    },

    test("в бою: эльф бьёт героя с трёх мест и стоит; человек оттуда не достаёт и шагает ближе") {
      def round(race: Race) =
        for {
          dao <- TestHeroDao.withHero(userId, hero)
          _   <- dao.writeActiveBattle(userId, lone(race, 4).asJson)
          r   <- TestRenderer.make
          c   <- ZIO.attempt(SceneContent.load())
          state = BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c)
          _   <- TestRandom.feedInts(60, 99, 99, 99, 99, 99) *> TestRandom.feedLongs(100L, 100L, 100L)
          _   <- state.action(testUser, tap("Wait"), r)
          h   <- dao.getHeroByUserId(userId).map(_.get)
          b   <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        } yield (hero.fightStats.hp - h.fightStats.hp, b.group.activePos)
      for {
        elf   <- round(Race.Elf)
        human <- round(Race.Human)
      } yield assertTrue(elf._1 > 0L && elf._2 == 4) &&
              assertTrue(human._1 == 0L && human._2 == 3)
    }
  )
}
