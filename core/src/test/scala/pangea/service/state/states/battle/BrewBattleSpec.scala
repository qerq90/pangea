package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.{Hero, WeaponDust}
import pangea.model.item.{BrewKind, BrewRates, Item}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.schedule.Scheduler
import pangea.service.state.UserAction
import pangea.test._
import zio.test.{TestRandom, _}
import zio.{Task, ZIO}

/** Отвары, которые работают в бою: грибная смесь бьёт по всему полю,
  * зеркальный настой подставляет под удар копии. */
object BrewBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val lvl = 10L

  private def hero(dust: WeaponDust = WeaponDust.empty): Hero =
    TestFixtures.hero(userId, state = StateType.Battle).copy(
      lvl        = lvl,
      baseStats  = BaseStats(agi = 10, vit = 10, str = 10, int = 10),
      fightStats = FightStats(atk = 20, hp = 5000, armor = 500, defence = 10, evasion = 5, accuracy = 20, energy = 100),
      weaponDust = dust)

  private def monster(hp: Long, atk: Long = 100L): Monster =
    Monster(0L, lvl, Race.Goblin, Rarity.Common,
      FightStats(atk = atk, hp = hp, armor = 0, defence = 0, evasion = 0, accuracy = 10000, energy = 0))

  private def mix(id: Long): Item = BrewKind.item(BrewKind.MushroomMix).copy(id = id)

  private def makeState(h: Hero, battle: SoloPveBattle, bag: List[Item] = Nil) =
    for {
      dao <- TestHeroDao.withHero(userId, h)
      _   <- dao.writeActiveBattle(userId, battle.asJson)
      inv  = TestInventoryRepository.withItems(bag)
      r   <- TestRenderer.make
      c   <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, inv, TestItemRepository.make, c, Scheduler.none), dao, inv, r)

  private def battleOf(dao: TestHeroDao): Task[SoloPveBattle] =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  override def spec = suite("Отвары в бою")(

    test("грибная смесь: кнопка есть только со склянкой в сумке") {
      val b = SoloPveBattle.from(monster(1000L), hero())
      for {
        with_ <- makeState(hero(), b, List(mix(1L)))
        (s1, _, _, r1) = with_
        _      <- s1.enter(testUser, r1)
        armed  <- r1.sentScreens.map(_.last)
        empty_ <- makeState(hero(), b)
        (s2, _, _, r2) = empty_
        _      <- s2.enter(testUser, r2)
        plain  <- r2.sentScreens.map(_.last)
      } yield assertTrue(armed.choices.exists(c => c.id == "ThrowMix" && c.label.contains("1"))) &&
              assertTrue(!plain.choices.exists(_.id == "ThrowMix"))
    },

    test("бросок смеси: достаётся всем в поле, все травятся, склянка уходит из сумки") {
      val group = SoloPveBattle.fromGroup(List(monster(100000L), monster(100000L), monster(100000L)),
        hero(), List(0L, 0L, 0L))
      for {
        t <- makeState(hero(), group, List(mix(1L)))
        (state, dao, inv, r) = t
        res    <- state.action(testUser, tap("ThrowMix"), r)
        after  <- battleOf(dao)
        said   <- texts(r)
        blow    = BrewRates.MushroomDamagePerLvl * lvl
      } yield assertTrue(res == StateType.Battle && said.contains("Склянка лопается")) &&
              // досталось и тому, кто в паре, и тем, кто в строю
              assertTrue(after.monsterCurrentHp == 100000L - blow) &&
              assertTrue(after.group.others.forall(_.currentHp == 100000L - blow)) &&
              assertTrue(after.effects.monsterPoison.isDefined) &&
              assertTrue(after.group.others.forall(_.effects.monsterPoison.isDefined)) &&
              // склянка одноразовая и считается расходником раунда
              assertTrue(inv.snapshot.isEmpty && after.consumableUsedThisRound)
    },

    test("вторую склянку в тот же раунд не бросить, а без склянки кнопка не сработает") {
      val b = SoloPveBattle.from(monster(100000L), hero())
      for {
        t <- makeState(hero(), b.copy(consumableUsedThisRound = true), List(mix(1L)))
        (state, _, inv, r) = t
        _    <- state.action(testUser, tap("ThrowMix"), r)
        busy <- texts(r)
        t2   <- makeState(hero(), b)
        (s2, _, _, r2) = t2
        _    <- s2.action(testUser, tap("ThrowMix"), r2)
        none <- texts(r2)
      } yield assertTrue(busy.contains("уже") && inv.snapshot.size == 1) &&
              assertTrue(none.contains("Грибной смеси в сумке нет"))
    },

    test("зеркальный настой: первые удары уходят в копии, потом бьют героя") {
      // Копии выпиты до боя — они приезжают в бой вместе с героем.
      val drunk = hero(WeaponDust.empty.copy(mirrors = 2))
      val b     = SoloPveBattle.from(monster(100000L), drunk)
      for {
        t <- makeState(drunk, b, Nil)
        (state, dao, _, r) = t
        // герой промахивается, моб бьёт в ответ: 1-й удар — в копию
        _      <- TestRandom.feedInts(1, 99) *> TestRandom.feedLongs(100L, 100L)
        _      <- state.action(testUser, tap("Attack"), r)
        first  <- dao.getHeroByUserId(userId).map(_.get)
        mid    <- battleOf(dao)
        said   <- texts(r)
        // 2-й удар — во вторую копию, 3-й достаётся уже герою
        _      <- TestRandom.feedInts(1, 99) *> TestRandom.feedLongs(100L, 100L)
        _      <- state.action(testUser, tap("Attack"), r)
        second <- dao.getHeroByUserId(userId).map(_.get)
        _      <- TestRandom.feedInts(1, 99) *> TestRandom.feedLongs(100L, 100L)
        _      <- state.action(testUser, tap("Attack"), r)
        third  <- dao.getHeroByUserId(userId).map(_.get)
        last   <- battleOf(dao)
      } yield assertTrue(said.contains("рассекает пустоту")) &&
              assertTrue(first.fightStats.hp == drunk.fightStats.hp && first.fightStats.armor == drunk.fightStats.armor) &&
              assertTrue(mid.effects.heroMirrors == 1) &&
              assertTrue(second.fightStats.hp == drunk.fightStats.hp) &&
              // копии кончились — удар дошёл
              assertTrue(last.effects.heroMirrors == 0) &&
              assertTrue(third.fightStats.armor < drunk.fightStats.armor || third.fightStats.hp < drunk.fightStats.hp)
    }
  )
}
