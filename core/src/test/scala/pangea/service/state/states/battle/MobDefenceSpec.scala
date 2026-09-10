package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.monster.MonsterGenerator
import pangea.domain.Rng
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.monster.{MonsterRaceFactor, Race, Rarity}
import pangea.model.skill.{MonsterEnergy, MonsterSkill}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Защита мобов: процентное снижение урона поверх брони, и пробитие героя,
  * которое эту защиту съедает ещё до расчёта процента. */
object MobDefenceSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val lvl = 10L

  /** Герой с ровно заданными ИНТ/ЛОВ и известной атакой: Мощь = сила×3 + атака. */
  private def hero(int: Long = 1L, agi: Long = 1L, str: Long = 1L, atk: Long = 100L): Hero = {
    val h = TestFixtures.hero(userId)
    h.copy(
      lvl        = lvl,
      fightStats = FightStats(atk = atk, hp = 500000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = h.baseStats.copy(int = int, agi = agi, str = str, vit = 5000)
    )
  }

  private def mobBattle(defence: Long, armor: Long = 0L, race: Race = Race.Human): SoloPveBattle =
    SoloPveBattle(
      monsterLvl           = lvl,
      monsterRace          = race.entryName,
      monsterRarity        = Rarity.Common.entryName,
      monsterStats         = FightStats(atk = 1, hp = 10000000L, armor = armor, defence = defence,
                                        evasion = 0, accuracy = 1,
                                        energy = MonsterEnergy.maxEnergy(lvl)),
      monsterCurrentHp     = 10000000L,
      monsterCurrentArmor  = armor,
      monsterCurrentEnergy = 0L
    )

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, content), dao, renderer)

  /** Один удар героя: возвращает, сколько HP реально снялось с моба, и экран. */
  private def strike(h: Hero, b: SoloPveBattle) =
    for {
      t <- makeState(h, b)
      (state, dao, r) = t
      // броски: попадание героя, попадание моба, разброс урона обоих ×100%
      _       <- TestRandom.feedInts(60, 90) *> TestRandom.feedLongs(100L, 100L)
      _       <- state.action(testUser, tap("Attack"), r)
      after   <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      screens <- r.sentScreens
    } yield (b.monsterCurrentHp - after.monsterCurrentHp, screens.map(_.text).mkString("\n"))

  /** Пробитие ровного билда на этом уровне: очки поровну на четыре характеристики. */
  private def evenBuild(l: Long): Long = 4L + (l - 1L)

  override def spec = suite("Защита мобов и пробитие")(

    // ── Формулы ───────────────────────────────────────────────────────────────
    test("Мощь — сила втрое плюс атака; у моба это просто его атака") {
      assertTrue(BattleState.power(str = 10L, atk = 100L) == 130L) &&
      assertTrue(BattleState.power(str = 0L, atk = 250L) == 250L)
    },

    test("пробитие = 2×ИНТ + 2×ЛОВ + десятая часть Мощи + стат с предметов") {
      assertTrue(BattleState.pierce(int = 20L, agi = 20L, power = 500L, pierceStat = 0L) == 130L) &&
      // стат «пробитие» пока никто не даёт, но слагаемое учитывается
      assertTrue(BattleState.pierce(int = 0L, agi = 0L, power = 0L, pierceStat = 7L) == 7L)
    },

    test("доля срезанной защиты держится ровной от 10 уровня до 150") {
      // Пробитие и защита растут линейно по уровню, поэтому отношение почти
      // постоянно — ради этого ИНТ и ЛОВ складываются, а не перемножаются.
      def ratio(l: Long): Double = {
        val stat   = evenBuild(l)
        val power  = BattleState.power(stat, 36L * l) // экипировка растёт с уровнем
        val pierce = BattleState.pierce(stat, stat, power, 0L)
        val base   = l.toDouble * Rarity.Common.factor * 1.1
        val def0   = (4.0 * base * MonsterRaceFactor.of(Race.Goblin).defenceFactor).toLong
        pierce.toDouble / def0.toDouble
      }
      val spread = List(10L, 50L, 150L).map(ratio)
      assertTrue(spread.max / spread.min < 1.2)
    },

    test("пробитие сначала съедает защиту, и только остаток режет урон") {
      // Защита 1000, пробитие 400 → в формулу идёт 600 против Мощи 300:
      // снижение = 600 / (600 + 600) = 50%.
      assertTrue(BattleState.defenceReduction(defence = 1000L, pierce = 400L, attackerPower = 300L) == 0.5) &&
      // пробитие больше защиты — снижения нет вовсе
      assertTrue(BattleState.defenceReduction(defence = 300L, pierce = 500L, attackerPower = 300L) == 0.0) &&
      // чем сильнее бьющий, тем меньше та же защита ему мешает
      assertTrue(BattleState.defenceReduction(1000L, 0L, 500L) < BattleState.defenceReduction(1000L, 0L, 100L))
    },

    // ── В бою ─────────────────────────────────────────────────────────────────
    test("защита режет обычную атаку героя, а не только урон умений") {
      val h = hero()
      for {
        naked   <- strike(h, mobBattle(defence = 0L))
        guarded <- strike(h, mobBattle(defence = 100000L))
        (nakedDmg, _)   = naked
        (guardedDmg, _) = guarded
      } yield assertTrue(nakedDmg > 0L) &&
              assertTrue(guardedDmg < nakedDmg) &&
              // потолок снижения — 70%, поэтому что-то проходит всегда
              assertTrue(guardedDmg > 0L)
    },

    test("прокачанные ИНТ и ЛОВ пробивают защиту: тот же моб получает больше") {
      val dumb  = hero(int = 1L, agi = 1L)
      val smart = hero(int = 60L, agi = 60L)
      for {
        a <- strike(dumb,  mobBattle(defence = 500L))
        b <- strike(smart, mobBattle(defence = 500L))
      } yield assertTrue(b._1 > a._1)
    },

    test("защиты меньше пробития — урон проходит целиком, как без защиты") {
      val h = hero(int = 50L, agi = 50L) // пробитие ≥ 200
      for {
        none <- strike(h, mobBattle(defence = 0L))
        weak <- strike(h, mobBattle(defence = 150L))
      } yield assertTrue(weak._1 == none._1)
    },

    test("экран боя показывает защиту моба процентом снижения") {
      val h = hero()
      for {
        t <- makeState(h, mobBattle(defence = 100000L))
        (state, _, r) = t
        _       <- state.enter(testUser, r)
        screens <- r.sentScreens
      } yield assertTrue(screens.last.text.contains("🛡")) &&
              // против этого героя защита упирается в потолок снижения — 70%
              assertTrue(screens.last.text.contains("🛡 70%"))
    },

    // ── Развязка со старыми ролями защиты ────────────────────────────────────
    test("защита больше не множит броню: в бой моб входит со своей бронёй") {
      val monster = MonsterGenerator.generateOfRace(10, Race.Gnome, Rng(5L))._1
      val battle  = SoloPveBattle.from(monster, TestFixtures.hero(userId))
      assertTrue(monster.fightStats.defence > 0L) &&
      assertTrue(battle.monsterCurrentArmor == monster.fightStats.armor) &&
      assertTrue(MonsterSkill.monsterMaxArmor(battle) == monster.fightStats.armor)
    },

    test("защита больше не влияет на уклонение моба") {
      val h = hero()
      for {
        t1 <- makeState(h, mobBattle(defence = 0L))
        (s1, _, r1) = t1
        _  <- s1.enter(testUser, r1)
        a  <- r1.sentScreens.map(_.last.text)
        t2 <- makeState(h, mobBattle(defence = 100000L))
        (s2, _, r2) = t2
        _  <- s2.enter(testUser, r2)
        b  <- r2.sentScreens.map(_.last.text)
        dodgeOf = (t: String) => t.linesIterator.find(_.contains("💨")).getOrElse("")
      } yield assertTrue(dodgeOf(a) == dodgeOf(b))
    },

    test("мобы защиту героя не пробивают — их урон считается как прежде") {
      // У формулы урона моба нет слагаемого пробития: снижение зависит только от
      // защиты героя, его интеллекта и атаки моба.
      val reduction = BattleState.damageReduction(
        protection = 100L, defenderInt = 10L, attackerInt = 50L, bonusPct = 0L)
      assertTrue(reduction == 110.0 / 210.0)
    }
  )
}
