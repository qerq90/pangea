package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.monster.{Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._
import zio.test.TestRandom

/** Интеграционные тесты на каст способностей мобом внутри `BattleState.monsterAttacks`.
 *  Каст моба роллится после обычной атаки; рандом контролируется через `TestRandom.feedInts`.
 *  Порядок Int-вызовов в одном `Attack`: heroHitRoll, mobHitRoll, mobSkillRoll, [mobSkillIdx]. */
object MonsterSkillsBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  // Шанс каста моба зависит только от редкости; в тестах skillRoll=1 всегда проходит.
  private def baseHero = TestFixtures.hero(userId).copy(
    fightStats = FightStats(atk = 50, hp = 200, armor = 0, defence = 0,
                            evasion = 9999, accuracy = 9999, energy = 0)
  )

  // Слабый монстр с большим запасом hp (не умрёт от удара героя).
  private def battle(
    mobAtk:     Long = 10,
    mobArmor:   Long = 0,
    mobDefence: Long = 0,
    curHp:      Long = 100,
    maxHp:      Long = 100,
    curArmor:   Long = 0
  ): SoloPveBattle = SoloPveBattle(
    monsterLvl          = 1L,
    monsterRace         = Race.Human.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = mobAtk, hp = maxHp, armor = mobArmor, defence = mobDefence,
                                     evasion = 0, accuracy = 1, energy = 0),
    monsterCurrentHp    = curHp,
    monsterCurrentArmor = curArmor
  )

  private def makeState(hero: pangea.model.hero.Hero, b: SoloPveBattle) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      _        <- heroDao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(heroDao, content), heroDao, renderer)

  override def spec = suite("MonsterSkillsBattleSpec")(

    test("При прокшем skillRoll моб дополнительно кастует и сообщение содержит шаблон скилла") {
      // heroHitRoll=50 (попал в моба), mobHitRoll=50 (моб попал), skillRoll=1 (каст прокнул),
      // skillIdx=0 (первый applicable). У полной hp/armor моба applicable = [QuickStrike, CrushingStrike].
      val b = battle(curHp = 100, maxHp = 100, mobArmor = 0, curArmor = 0)
      for {
        triple                       <- makeState(baseHero, b)
        (state, _, renderer)          = triple
        _                            <- TestRandom.feedInts(50, 50, 1, 0)
        _                            <- state.action(testUser, tap("Attack"), renderer)
        screens                      <- renderer.sentScreens
        joined                        = screens.map(_.text).mkString("\n")
      } yield assertTrue(joined.contains("делает быстрые атаки")) // QuickStrike шаблон
    },

    test("CrushingStrike моба бьёт мимо большой брони игрока — hp падает") {
      // У героя огромная armor — обычная атака моба её слегка ест.
      // skillIdx=1 в applicable [QuickStrike, CrushingStrike] → CrushingStrike (мимо брони).
      val tankHero = baseHero.copy(
        fightStats = baseHero.fightStats.copy(hp = 500L, armor = 999_999L, defence = 0)
      )
      val b = battle(mobAtk = 100, curHp = 100, maxHp = 100, mobArmor = 0, curArmor = 0)
      for {
        triple                       <- makeState(tankHero, b)
        (state, heroDao, renderer)    = triple
        _                            <- TestRandom.feedInts(50, 50, 1, 1)
        _                            <- state.action(testUser, tap("Attack"), renderer)
        updated                      <- heroDao.getHeroByUserId(userId).map(_.get)
        screens                      <- renderer.sentScreens
        joined                        = screens.map(_.text).mkString("\n")
      } yield assertTrue(updated.fightStats.hp < 500L) &&
              assertTrue(updated.fightStats.armor > 0L) &&
              assertTrue(joined.contains("бьёт плашмя"))
    },

    test("HealingFlask и EmergencyRepair исключены из applicable при полных hp/armor") {
      // Моб с полной hp и полной armor (curArmor == maxArmor). applicable = [QuickStrike, CrushingStrike].
      // Любой skillIdx из 0..1 — это один из двух уроновых, не лечение/починка.
      val b = battle(curHp = 100, maxHp = 100, mobArmor = 5, mobDefence = 4, curArmor = 20)
      for {
        triple                       <- makeState(baseHero, b)
        (state, heroDao, renderer)    = triple
        _                            <- TestRandom.feedInts(50, 50, 1, 0)
        _                            <- state.action(testUser, tap("Attack"), renderer)
        afterBattle                  <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield
        // hp и armor моба не выросли (нет хила/ремонта); максимум — упали от обычной атаки героя.
        assertTrue(afterBattle.monsterCurrentHp <= 100L) &&
        assertTrue(afterBattle.monsterCurrentArmor <= 20L)
    },

    test("Скилл моба не кастуется, если игрок умер от обычной атаки") {
      // Полностью исключаем уклонение и точность героя, моб с гарантированным попаданием
      // и большим уроном — обычка убъёт за один удар.
      val dying = baseHero.copy(
        baseStats  = baseHero.baseStats.copy(agi = 0),
        fightStats = baseHero.fightStats.copy(hp = 1L, armor = 0L, evasion = 0, defence = 0)
      )
      // У моба atk большой, accuracy = 1000 → шанс попадания почти 95%.
      val mob = battle(mobAtk = 1000, curHp = 100, maxHp = 100, mobArmor = 0, curArmor = 0).copy(
        monsterStats = battle().monsterStats.copy(atk = 1000, accuracy = 1000))
      for {
        triple                       <- makeState(dying, mob)
        (state, _, renderer)          = triple
        // skillRoll=1 (если бы дошло — каст бы прокнул), но из-за смерти каста не должно быть.
        _                            <- TestRandom.feedInts(50, 50, 1, 0)
        result                       <- state.action(testUser, tap("Attack"), renderer)
        screens                      <- renderer.sentScreens
        joined                        = screens.map(_.text).mkString("\n")
      } yield assertTrue(result == pangea.model.state.StateType.Death) &&
              assertTrue(!joined.contains("делает быстрые атаки")) &&
              assertTrue(!joined.contains("бьёт плашмя"))
    }
  )
}
