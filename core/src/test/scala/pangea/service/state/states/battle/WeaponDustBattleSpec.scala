package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, WeaponDust}
import pangea.model.item.MaterialKind
import pangea.model.monster.{Race, Rarity}
import pangea.model.skill.MonsterEnergy
import pangea.model.stats.FightStats
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Пыль в бою: покрытие даёт стихию, неудачное покрытие режет урон, и всё это
  * держится ровно один бой. */
object WeaponDustBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val lvl = 10L

  private def hero(dust: WeaponDust = WeaponDust.empty): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = lvl,
      fightStats = FightStats(atk = 100, hp = 500000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000),
      weaponDust = dust)

  private def battle(hp: Long = 10000000L, armor: Long = 0L): SoloPveBattle = SoloPveBattle(
    monsterLvl           = lvl,
    monsterRace          = Race.Human.entryName,
    monsterRarity        = Rarity.Common.entryName,
    monsterStats         = FightStats(atk = 1, hp = hp.max(1L), armor = armor, defence = 0,
                                      evasion = 0, accuracy = 1, energy = MonsterEnergy.maxEnergy(lvl)),
    monsterCurrentHp     = hp,
    monsterCurrentArmor  = armor,
    monsterCurrentEnergy = 0L)

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, content), dao, renderer)

  /** Один удар: сколько снялось с HP и с брони моба. `extra` — броски после удара
    * героя и моба (например проки стихий). */
  private def strike(h: Hero, b: SoloPveBattle, extra: Int*) =
    for {
      t <- makeState(h, b)
      (state, dao, r) = t
      _     <- TestRandom.feedInts(60 +: 90 +: extra: _*) *> TestRandom.feedLongs(100L, 100L)
      _     <- state.action(testUser, tap("Attack"), r)
      after <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      log   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
    } yield (b.monsterCurrentHp - after.monsterCurrentHp,
             b.monsterCurrentArmor - after.monsterCurrentArmor,
             log)

  override def spec = suite("Пыль в бою")(

    test("штраф за всполох режет урон героя на четверть") {
      // Проки стихий не роллятся: покрытия нет, только штраф.
      for {
        clean   <- strike(hero(), battle())
        penalty <- strike(hero(WeaponDust(Nil, penalty = true)), battle())
        (cleanDmg, _, _)   = clean
        (penaltyDmg, _, _) = penalty
      } yield assertTrue(cleanDmg > 0L) &&
              assertTrue(penaltyDmg == (cleanDmg * 3) / 4)
    },

    test("покрытие даёт оружию стихию — молния бьёт по броне слабее") {
      // Молния: −20% по броне. Второй бросок в extra — прок стихии; кормим его
      // большими значениями, чтобы сравнивать чистый урон, а не эффект прока.
      for {
        plain <- strike(hero(), battle(armor = 100000L))
        dusty <- strike(hero(WeaponDust(List(MaterialKind.TopazDust))), battle(armor = 100000L), 99, 99, 99)
        (_, plainArmor, _) = plain
        (_, dustyArmor, _) = dusty
      } yield assertTrue(plainArmor > 0L) &&
              assertTrue(dustyArmor < plainArmor) &&
              // ровно −20% по броне, без прибавки к эффективности стихии
              assertTrue(dustyArmor == (plainArmor * 80) / 100)
    },

    test("пыль роллит прок стихии, как настоящий камень") {
      // Рубиновая пыль даёт Огонь: прокнувший огонь поджигает цель.
      val dusted = hero(WeaponDust(List(MaterialKind.RubyDust)))
      for {
        t <- makeState(dusted, battle())
        (state, dao, r) = t
        // Броски: удар героя (попал), прок огня (10 ≤ 30 — сработал), удар моба.
        _     <- TestRandom.feedInts(60, 10, 90) *> TestRandom.feedLongs(100L, 100L)
        _     <- state.action(testUser, tap("Attack"), r)
        after <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield assertTrue(after.effects.monsterBurn.isDefined)
    },

    test("после боя покрытие осыпается — и слои, и штраф") {
      val dusted = hero(WeaponDust(List(MaterialKind.RubyDust), penalty = true))
      for {
        t <- makeState(dusted, battle(hp = 1L))
        (state, dao, r) = t
        _       <- TestRandom.feedInts(60, 90, 99) *> TestRandom.feedLongs(100L, 100L)
        result  <- state.action(testUser, tap("Attack"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(result == StateType.Loot) &&
              assertTrue(updated.weaponDust.isEmpty)
    },

    test("пока бой идёт, покрытие остаётся на оружии") {
      val dusted = hero(WeaponDust(List(MaterialKind.RubyDust)))
      for {
        t <- makeState(dusted, battle())
        (state, dao, r) = t
        _       <- TestRandom.feedInts(60, 90, 99) *> TestRandom.feedLongs(100L, 100L)
        result  <- state.action(testUser, tap("Attack"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(updated.weaponDust.layers == List(MaterialKind.RubyDust))
    }
  )
}
