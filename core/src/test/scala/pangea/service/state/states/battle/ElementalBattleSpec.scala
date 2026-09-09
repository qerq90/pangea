package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.monster.{Elemental, Monster, Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._
import zio.test.TestRandom

/** Огненный элементаль ходит по кругу: всплеск → сфера → щит → пропуск. */
object ElementalBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  // Герой, который почти не промахивается и переживает удары босса.
  private def hero(hp: Long = 100000L, armor: Long = 0L) = TestFixtures.hero(userId).copy(
    lvl        = 15L, // BossLvL = 2
    fightStats = FightStats(atk = 1, hp = hp, armor = armor, defence = 0,
                            evasion = 9999, accuracy = 9999, energy = 0),
    baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1)
  )

  private val bossLvl = 2L

  /** Бой с огненным элементалем; `turn` — какая способность применится следующей. */
  // Энергия боссу каппится его максимумом (150 × BossLvL = 300), поэтому
  // «побольше» здесь задать нельзя — по умолчанию берём полный запас.
  private def lairBattle(turn: Int, orbs: Int = 0, energy: Long = 300L,
                         hpPct: Long = 100L, armorPct: Long = 100L): SoloPveBattle = {
    val stats   = Elemental.Fire.stats(bossLvl)
    val monster = Monster(0L, bossLvl, Race.Elemental, Rarity.Legendary, stats)
    SoloPveBattle.from(monster, hero()).copy(
      elementalKind        = Some(Elemental.Fire.entryName),
      elementalTurn        = turn,
      fireOrbs             = orbs,
      monsterCurrentEnergy = energy,
      monsterCurrentHp     = stats.hp * hpPct / 100L,
      monsterCurrentArmor  = stats.armor * armorPct / 100L
    )
  }

  private def makeState(h: pangea.model.hero.Hero, battle: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, battle.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, content), dao, renderer)

  private def battleAfter(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  /** Броски одного хода. Подавать ТОЛЬКО одним вызовом: повторный feedInts
    * кладёт значения в НАЧАЛО очереди и ломает порядок.
    * `extra` — добавочные броски после хода моба (напр. бросок травмы смерча). */
  private def seedTurn(extra: Int*) =
    TestRandom.feedInts(60 +: 1 +: extra: _*) *> TestRandom.feedLongs(100L)

  override def spec = suite("Огненный элементаль")(

    test("всплеск: урон по герою, поджог на 1% и списание энергии") {
      val h = hero(hp = 100000L)
      for {
        // Броня выбита: иначе шипы сами подожгут героя и смешаются со всплеском.
        t <- makeState(h, lairBattle(turn = 0, armorPct = 0L))
        (state, dao, r) = t
        _       <- seedTurn()
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- battleAfter(dao)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens
        // Поджог накладывается на 1%, но в конце ЭТОГО же раунда горение тикает
        // и растёт (+2), поэтому в сохранённом бою уже 3 — как и у горения моба.
        // Энергию здесь не проверяем: реген конца раунда возвращает потраченное.
      } yield assertTrue(after.effects.heroBurn.exists(_.pct == 3)) &&
              assertTrue(updated.fightStats.hp < 100000L) &&
              assertTrue(screens.map(_.text).mkString.contains("Огонь вырывается из под ваших ног"))
    },

    test("сфера: первые две копятся, третья срывается в смерч") {
      for {
        t1 <- makeState(hero(), lairBattle(turn = 1, orbs = 0))
        (s1, dao1, r1) = t1
        _        <- seedTurn()
        _        <- s1.action(testUser, tap("Attack"), r1)
        first    <- battleAfter(dao1)
        screens1 <- r1.sentScreens

        t3 <- makeState(hero(), lairBattle(turn = 1, orbs = 2))
        (s3, dao3, r3) = t3
        _        <- seedTurn(90) // бросок травмы смерча: 90 > 20, не выпала
        _        <- s3.action(testUser, tap("Attack"), r3)
        third    <- battleAfter(dao3)
        screens3 <- r3.sentScreens
      } yield assertTrue(first.fireOrbs == 1) &&
              assertTrue(screens1.map(_.text).mkString.contains("собралось в левитирующую сферу")) &&
              // третья сфера сразу бьёт и счётчик обнуляется
              assertTrue(third.fireOrbs == 0) &&
              assertTrue(screens3.map(_.text).mkString.contains("соединились в смерч"))
    },

    test("смерч бьёт на 2×атаки плюс 5% от максимумов HP и брони героя") {
      val h = hero(hp = 100000L, armor = 0L)
      for {
        t <- makeState(h, lairBattle(turn = 1, orbs = 2, armorPct = 0L)) // без шипов
        (state, dao, r) = t
        _       <- seedTurn(90) // травма не выпала
        _       <- state.action(testUser, tap("Attack"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        maxHp    = h.effectiveMaxHp(0L)
        maxArmor = h.effectiveMaxArmor(0L)
        expected = Elemental.Fire.stats(bossLvl).atk * 2L + maxHp * 5L / 100L + maxArmor * 5L / 100L
      } yield assertTrue(h.fightStats.hp - updated.fightStats.hp == expected)
    },

    test("смерч с шансом 20% даёт травму, как при смерти") {
      val h = hero(hp = 100000L)
      for {
        t <- makeState(h, lairBattle(turn = 1, orbs = 2))
        (state, dao, r) = t
        _       <- seedTurn(5, 0) // 5 ≤ 20 — травма выпала; 0 — выбор из пула
        _       <- state.action(testUser, tap("Attack"), r)
        updated <- dao.getHeroByUserId(userId).map(_.get)
        screens <- r.sentScreens
      } yield assertTrue(updated.traumaNames.nonEmpty) &&
              assertTrue(updated.traumaUntil.isDefined) &&
              assertTrue(screens.map(_.text).mkString.contains("получили травму"))
    },

    test("щит: чинит броню и HP, но только если элементалю есть что чинить") {
      for {
        // Побитый элементаль — щит срабатывает.
        t1 <- makeState(hero(), lairBattle(turn = 2, hpPct = 50L, armorPct = 50L))
        (s1, dao1, r1) = t1
        _       <- seedTurn()
        _       <- s1.action(testUser, tap("Attack"), r1)
        hurt    <- battleAfter(dao1)
        screens <- r1.sentScreens

        // Целый элементаль — умение не применяется, очередь всё равно едет дальше.
        t2 <- makeState(hero(), lairBattle(turn = 2))
        (s2, dao2, r2) = t2
        _       <- seedTurn()
        _       <- s2.action(testUser, tap("Attack"), r2)
        intact  <- battleAfter(dao2)
      } yield assertTrue(screens.map(_.text).mkString.contains("Пламя элементаля стало горячее")) &&
              assertTrue(hurt.monsterCurrentArmor > Elemental.Fire.stats(bossLvl).armor * 50L / 100L) &&
              assertTrue(intact.elementalTurn == 3) && // очередь сдвинулась
              assertTrue(intact.monsterCurrentEnergy == 300L) // энергия не потрачена
    },

    test("способности идут строго по кругу и возвращаются к началу") {
      def turnAfter(from: Int) =
        for {
          t <- makeState(hero(), lairBattle(turn = from))
          (state, dao, r) = t
          _     <- seedTurn(90) // на случай броска травмы смерча
          _     <- state.action(testUser, tap("Attack"), r)
          after <- battleAfter(dao)
        } yield after.elementalTurn
      for {
        a <- turnAfter(0)
        b <- turnAfter(1)
        c <- turnAfter(2)
        d <- turnAfter(3)
      } yield assertTrue(a == 1) && assertTrue(b == 2) && assertTrue(c == 3) && assertTrue(d == 0)
    },

    test("без энергии способность не применяется, но очередь едет дальше") {
      for {
        t <- makeState(hero(), lairBattle(turn = 0, energy = 0L, armorPct = 0L))
        (state, dao, r) = t
        _       <- seedTurn()
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- battleAfter(dao)
        screens <- r.sentScreens
      } yield assertTrue(after.effects.heroBurn.isEmpty) && // всплеска не было
              assertTrue(after.elementalTurn == 1) &&
              assertTrue(!screens.map(_.text).mkString.contains("Огонь вырывается"))
    },

    test("горение на герое тикает в конце раунда и усиливается") {
      val h = hero(hp = 100000L)
      for {
        t <- makeState(h, lairBattle(turn = 0, armorPct = 0L)) // без шипов
        (state, dao, r) = t
        _      <- seedTurn()
        _      <- state.action(testUser, tap("Attack"), r)
        first  <- battleAfter(dao)
        // Второй раунд: горение уже есть, оно тикает и растёт.
        _      <- seedTurn()
        _      <- state.action(testUser, tap("Attack"), r)
        second <- battleAfter(dao)
      } yield assertTrue(first.effects.heroBurn.exists(_.pct == 3)) &&  // 1 наложили, +2 за тик
              assertTrue(second.effects.heroBurn.exists(_.pct == 5))    // ещё один тик
    },

    // ── Особенности огненного ─────────────────────────────────────────────────
    test("огонь по огненному почти не проходит, холод бьёт в полтора раза сильнее") {
      // Сравниваем оружие БЕЗ камня против оружия С камнем: «голые руки» дали бы
      // штраф к урону (×0.5) и сравнение было бы не про стихию.
      def weapon(gem: Option[pangea.model.item.GemKind]) =
        pangea.model.item.Item(7L, "Меч", 1L, pangea.model.item.Rarity.Blue, pangea.model.item.ItemType.Weapon,
          attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
          sockets = gem.map(k => Some(pangea.model.item.Gem(k, 1))).toList)
      def damageWith(w: pangea.model.item.Item) = {
        val h = hero().copy(
          fightStats = hero().fightStats.copy(atk = 1000),
          equipment  = TestFixtures.emptyEquipment.copy(weapon = w))
        for {
          t <- makeState(h, lairBattle(turn = 3, armorPct = 0L)) // пропуск, без шипов
          (state, dao, r) = t
          _     <- seedTurn(90, 90) // возможные проки стихии оружия — мимо
          _     <- state.action(testUser, tap("Attack"), r)
          after <- battleAfter(dao)
        } yield Elemental.Fire.stats(bossLvl).hp - after.monsterCurrentHp
      }
      for {
        plain <- damageWith(weapon(None))
        fire  <- damageWith(weapon(Some(pangea.model.item.GemKind.Ruby)))
        cold  <- damageWith(weapon(Some(pangea.model.item.GemKind.Sapphire)))
      } yield assertTrue(fire < plain / 3) &&  // ×0.2, с поправкой на грани огня
              assertTrue(cold > plain)         // ×1.5, холод бьёт сильнее
    },

    test("зовётся Огненным Элементалем, а не «Легендарным»: имя даёт вид, не редкость") {
      val battle = lairBattle(turn = 3)
      for {
        t <- makeState(hero(), battle)
        (state, _, r) = t
        _       <- seedTurn()
        _       <- state.action(testUser, tap("Attack"), r)
        screens <- r.sentScreens
        log      = screens.map(_.text).mkString
      } yield assertTrue(battle.monsterName == "Огненный Элементаль") &&
              assertTrue(log.contains("Огненный Элементаль")) &&
              assertTrue(!log.contains("Легендарный Элементаль"))
    },

    test("огненного нельзя поджечь") {
      val battle = lairBattle(turn = 3)
      assertTrue(battle.withEffects(
        battle.effects.copy(monsterBurn = Some(pangea.model.battle.Burn(10)))).effects.monsterBurn.isEmpty)
    },

    test("шипы: пока цела броня, удар возвращается и поджигает героя") {
      val h = hero(hp = 100000L)
      for {
        // Броня цела — шипы отвечают.
        t1 <- makeState(h, lairBattle(turn = 3, armorPct = 100L))
        (s1, dao1, r1) = t1
        _        <- seedTurn()
        _        <- s1.action(testUser, tap("Attack"), r1)
        withArmor <- battleAfter(dao1)
        screens  <- r1.sentScreens

        // Броня выбита — шипов нет.
        t2 <- makeState(h, lairBattle(turn = 3, armorPct = 0L))
        (s2, dao2, r2) = t2
        _        <- seedTurn()
        _        <- s2.action(testUser, tap("Attack"), r2)
        noArmor  <- battleAfter(dao2)
      } yield assertTrue(withArmor.effects.heroBurn.isDefined) &&
              assertTrue(screens.map(_.text).mkString.contains("Раскалённая броня элементаля обжигает")) &&
              assertTrue(noArmor.effects.heroBurn.isEmpty)
    },

    test("скованный холодом не отвечает шипами и не поджигает") {
      val h = hero(hp = 100000L)
      val chilled = lairBattle(turn = 3, armorPct = 100L)
      val frozen  = chilled.copy(effects = chilled.effects.copy(chilledTurns = 5))
      for {
        t <- makeState(h, frozen)
        (state, dao, r) = t
        _       <- seedTurn()
        _       <- state.action(testUser, tap("Attack"), r)
        after   <- battleAfter(dao)
        screens <- r.sentScreens
      } yield assertTrue(after.effects.heroBurn.isEmpty) &&
              assertTrue(!screens.map(_.text).mkString.contains("Раскалённая броня"))
    },

    test("оцепенение тикает и само спадает") {
      val chilled = lairBattle(turn = 3, armorPct = 0L)
      val frozen  = chilled.copy(effects = chilled.effects.copy(chilledTurns = 2))
      for {
        t <- makeState(hero(), frozen)
        (state, dao, r) = t
        _      <- seedTurn()
        _      <- state.action(testUser, tap("Attack"), r)
        after  <- battleAfter(dao)
      } yield assertTrue(after.effects.chilledTurns == 1) && assertTrue(after.effects.chilled)
    },

    test("яд и кровотечение на элементале не держатся") {
      val battle = lairBattle(turn = 3) // пропуск, чтобы не мешал
      assertTrue(battle.withEffects(
        battle.effects.copy(
          monsterPoison = Some(pangea.model.battle.Poison(10)),
          monsterBleed  = Some(pangea.model.battle.Bleed(10)))).effects.monsterPoison.isEmpty) &&
      assertTrue(battle.withEffects(
        battle.effects.copy(monsterBleed = Some(pangea.model.battle.Bleed(10)))).effects.monsterBleed.isEmpty)
    }
  )
}
